(*
 * oBus_signal.ml
 * --------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let section = Lwt_log.Section.make "obus(signal)"

open Lwt
open OBus_private_connection

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

(* State of a signal receiver *)
type signal_state =
  | Sig_init
      (* Still initialising *)
  | Sig_disconnected
      (* The receiver has been disconnected *)
  | Sig_connected
      (* Initialisation is done, the signal receiver is up and
         running *)

(* A signal description *)
type signal_descriptor = {
  mutable sd_state : signal_state;
  mutable sd_auto_match_rule : bool;
  mutable sd_filters : (int * OBus_match.argument_filter) list;

  sd_receiver : signal_receiver;
  sd_group : signal_receiver_group;
  sd_node : signal_receiver Lwt_sequence.node;

  sd_sender : OBus_name.bus option;

  (* Thread which is wake up when the signal is disconnected: *)
  sd_done_waiter : unit Lwt.t;
  sd_done_wakener : unit Lwt.u;
}

type 'a t = {
  s_event : (OBus_context.void OBus_context.t * 'a) React.event;
  (* The event that is passed to the user *)

  s_descr : signal_descriptor;
  (* Description which is shared by all mapped version of the
     signal *)
}

(* +-----------------------------------------------------------------+
   | Signal transformations                                          |
   +-----------------------------------------------------------------+ *)

let map f signal = {
  signal with
    s_event = React.E.map (fun (context, x) -> (context, f x)) signal.s_event
}

let map_with_context f signal = {
  signal with
    s_event = React.E.map (fun (context, x) -> (context, f context x)) signal.s_event
}

(* +-----------------------------------------------------------------+
   | Rules computing and setting                                     |
   +-----------------------------------------------------------------+ *)

(* Add a matching rule to a list of incomparable most general rules *)
let rec add_rule rule rules = match rules with
  | [] ->
      [rule]
  | rule' :: rest ->
      match OBus_match.compare_rules rule rule' with
        | OBus_match.Incomparable ->
            rule' :: add_rule rule rest
        | OBus_match.Equal
        | OBus_match.Less_general ->
            rules
        | OBus_match.More_general ->
            rule :: rest

(* Commit rules changes on the message bus *)
let commit_rules descr =
  let group = descr.sd_group in
  let rules =
    (* Computes the set of more general rules: *)
    Lwt_sequence.fold_l
      (fun receiver rules ->
         if receiver.sr_active then
           match receiver.sr_rule with
             | None -> rules
             | Some rule -> add_rule rule rules
         else
           rules)
      group.srg_receivers []
  in
  let rules =
    List.fold_left
      (fun set rule ->
         String_set.add (OBus_match.string_of_rule rule) set)
      String_set.empty
      rules
  in

  (* If rules have changed, compute the minimal set of changes to make
     with the message bus and do them: *)
  if not (String_set.equal rules group.srg_rules) then
   Lwt_mutex.with_lock group.srg_mutex
     (fun () ->
        let new_rules = String_set.diff rules group.srg_rules
        and old_rules = String_set.diff group.srg_rules rules in
        group.srg_rules <- new_rules;
        let connection = descr.sd_group.srg_connection in
        lwt () = String_set.fold (fun rule thread -> thread <&> OBus_private_bus.add_match connection rule) new_rules (return ())
        and () = String_set.fold (fun rule thread -> thread <&> OBus_private_bus.remove_match connection rule) old_rules (return ()) in
        return ())
 else
   return ()

(* +-----------------------------------------------------------------+
   | Signal initialisation                                           |
   +-----------------------------------------------------------------+ *)

let init_signal descr =
  let connection = descr.sd_group.srg_connection in
  try_lwt
    if OBus_connection.name connection = None then begin
      (* If the connection is a peer-to-peer connection, there is
         nothing else to do. *)
      descr.sd_receiver.sr_active <- true;
      descr.sd_done_waiter
    end else begin
      lwt resolver =
        match descr.sd_sender with
          | None ->
              return None
          | Some name ->
              (* If we are interested on signals coming from a
                 particular peer, we need a name resolver: *)
              lwt resolver = OBus_resolver.make connection name in
              descr.sd_receiver.sr_sender <- Some (OBus_resolver.owner resolver);
              return (Some resolver)
      in
      try_lwt
        (* Yield to let the user add argument filters: *)
        lwt () = pause () in
        (* Since we yielded, check the connection again: *)
        check_connection connection;
        if descr.sd_state = Sig_disconnected then
          return ()
        else begin
          descr.sd_receiver.sr_active <- true;
          lwt () = commit_rules descr in
          descr.sd_done_waiter
        end
      finally
        match resolver with
          | None -> return ()
          | Some resolver -> OBus_resolver.disable resolver
    end
  finally
    Lwt_sequence.remove descr.sd_node;
    lwt () = commit_rules descr in
    if Lwt_sequence.is_empty descr.sd_group.srg_receivers then begin
      let running = connection#get in
      running.rc_receiver_groups <- (
        Signal_map.remove
          (descr.sd_group.srg_path,
           descr.sd_group.srg_interface,
           descr.sd_group.srg_member)
          running.rc_receiver_groups;
      )
    end;
    return ()

(* +-----------------------------------------------------------------+
   | Signal creation                                                 |
   +-----------------------------------------------------------------+ *)

let cast info (context, message) =
  try
    Some(context,
         OBus_value.C.cast_sequence
           (OBus_value.arg_types
              (OBus_member.Signal.args info))
           (OBus_message.body message))
  with OBus_value.C.Signature_mismatch ->
    ignore (
      Lwt_log.error_f ~section "failed to cast signal from %S, interface %S, member %S with signature %S to %S"
        (match OBus_message.sender message with None -> "" | Some n -> n)
        (OBus_member.Signal.interface info)
        (OBus_member.Signal.member info)
        (OBus_value.string_of_signature
           (OBus_value.V.type_of_sequence
              (OBus_message.body message)))
        (OBus_value.string_of_signature
           (OBus_value.C.type_sequence
              (OBus_value.arg_types
                 (OBus_member.Signal.args info))))
    );
    None

let disconnect signal =
  let descr = signal.s_descr in
   if descr.sd_state <> Sig_disconnected then begin
     descr.sd_state <- Sig_disconnected;
     wakeup descr.sd_done_wakener ()
   end

let stop descr () =
  if descr.sd_state <> Sig_disconnected then begin
    descr.sd_state <- Sig_disconnected;
    wakeup descr.sd_done_wakener ()
  end

let connect info proxy =
  let connection = OBus_proxy.connection proxy in
  let running = connection#get in
  (* Signal groups are indexed by tuples [(path, interface, member)]: *)
  let key = (OBus_proxy.path proxy,
             OBus_member.Signal.interface info,
             OBus_member.Signal.member info) in
  let group =
    match try Some(Signal_map.find key running.rc_receiver_groups) with Not_found -> None with
      | Some group ->
          group
      | None ->
          (* If the group do not exists, create a new one *)
          let group = {
            srg_rules = String_set.empty;
            srg_mutex = Lwt_mutex.create ();
            srg_receivers = Lwt_sequence.create ();
            srg_connection = connection;
            srg_path = OBus_proxy.path proxy;
            srg_interface = OBus_member.Signal.interface info;
            srg_member = OBus_member.Signal.member info;
          } in
          running.rc_receiver_groups <- Signal_map.add key group running.rc_receiver_groups;
          group
  in
  let event, push = React.E.create () in
  let receiver = {
    sr_active = false;
    sr_sender = None;
    sr_rule = (
      Some(OBus_match.rule
             ~typ:`Signal
             ?sender:(OBus_proxy.name proxy)
             ~path:(OBus_proxy.path proxy)
             ~interface:(OBus_member.Signal.interface info)
             ~member:(OBus_member.Signal.member info)
             ())
    );
    sr_filter = (fun _ -> true);
    sr_push = push;
  } in
  (* Immediatly add the recevier to avoid race condition *)
  let node = Lwt_sequence.add_r receiver group.srg_receivers in

  (* Thread which is wake up when the signal is disconnected *)
  let done_waiter, done_wakener = Lwt.wait () in

  let descr = {
    sd_state = Sig_init;
    sd_auto_match_rule = true;
    sd_filters = [];
    sd_done_waiter = done_waiter;
    sd_done_wakener = done_wakener;
    sd_receiver = receiver;
    sd_group = group;
    sd_node = node;
    sd_sender = OBus_proxy.name proxy;
  } in
  let signal = {
    s_event = Lwt_event.with_finaliser (stop descr) (React.E.fmap (cast info) event);
    s_descr = descr;
  } in
  ignore (init_signal descr);
  signal

let event signal = React.E.map snd signal.s_event
let event_with_context signal = signal.s_event

(* +-----------------------------------------------------------------+
   | Signal settings                                                 |
   +-----------------------------------------------------------------+ *)

let auto_match_rule signal = signal.s_descr.sd_auto_match_rule

let set_auto_match_rule signal auto_match_rule  =
  let descr = signal.s_descr in
  if auto_match_rule <> descr.sd_auto_match_rule then begin
    descr.sd_auto_match_rule <- auto_match_rule;
    if auto_match_rule then begin
      let rule =
        OBus_match.rule
          ~typ:`Signal
          ?sender:descr.sd_sender
          ~path:descr.sd_group.srg_path
          ~interface:descr.sd_group.srg_interface
          ~member:descr.sd_group.srg_member
          ~arguments:(OBus_match.make_arguments descr.sd_filters)
          ()
      in
      (* Use the sorted list of argument filters: *)
      let filters = OBus_match.arguments rule in
      descr.sd_receiver.sr_filter <- (fun message -> OBus_match.match_values filters (OBus_message.body message));
      descr.sd_receiver.sr_rule <- Some rule
    end else
      descr.sd_receiver.sr_rule <- None;
    match descr.sd_state with
      | Sig_init ->
          ()
      | Sig_disconnected ->
          invalid_arg "OBus_signal.set_auto_match_rule: disconnected signal"
      | Sig_connected ->
          ignore (commit_rules descr)
  end

let set_filters signal filters =
  let descr = signal.s_descr in
  descr.sd_filters <- filters;
  if descr.sd_auto_match_rule then begin
    let rule =
      OBus_match.rule
        ~typ:`Signal
        ?sender:descr.sd_sender
        ~path:descr.sd_group.srg_path
        ~interface:descr.sd_group.srg_interface
        ~member:descr.sd_group.srg_member
        ~arguments:(OBus_match.make_arguments descr.sd_filters)
        ()
    in
    let filters = OBus_match.arguments rule in
    descr.sd_receiver.sr_filter <- (fun message -> OBus_match.match_values filters (OBus_message.body message));
    descr.sd_receiver.sr_rule <- Some rule;
    match descr.sd_state with
      | Sig_init ->
          ()
      | Sig_disconnected ->
          invalid_arg "OBus_signal.set_filters: disconnected signal"
      | Sig_connected ->
          ignore (commit_rules descr)
  end

let init ?(filters=[]) ?(auto_match_rule=true) signal =
  let descr = signal.s_descr in
  descr.sd_filters <- filters;
  (* Force a refresh of the match rule if necessary: *)
  descr.sd_auto_match_rule <- not auto_match_rule;
  set_auto_match_rule signal auto_match_rule;
  event signal

(* +-----------------------------------------------------------------+
   | Emitting signals                                                |
   +-----------------------------------------------------------------+ *)

let emit info obj ?peer args =
  OBus_object.emit obj
    ~interface:(OBus_member.Signal.interface info)
    ~member:(OBus_member.Signal.member info)
    ?peer
    (OBus_value.arg_types (OBus_member.Signal.args info))
    args
