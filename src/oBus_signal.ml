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
type descr = {
  mutable state : signal_state;
  mutable auto_match_rule : bool;
  mutable filters : (int * OBus_match.argument_filter) list;

  receiver : receiver;
  receiver_group : receiver_group;
  node : receiver Lwt_sequence.node;

  sender : OBus_name.bus option;

  (* Thread which is wake up when the signal is disconnected: *)
  done_waiter : unit Lwt.t;
  done_wakener : unit Lwt.u;
}

type 'a t = {
  event : (OBus_context.void OBus_context.t * 'a) React.event;
  (* The event that is passed to the user *)

  descr : descr;
  (* Description which is shared by all mapped version of the
     signal *)
}

(* +-----------------------------------------------------------------+
   | Signal transformations                                          |
   +-----------------------------------------------------------------+ *)

let map f signal = {
  signal with
    event = React.E.map (fun (context, x) -> (context, f x)) signal.event
}

let map_with_context f signal = {
  signal with
    event = React.E.map (fun (context, x) -> (context, f context x)) signal.event
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
  let group = descr.receiver_group in
  let rules =
    (* Computes the set of more general rules: *)
    Lwt_sequence.fold_l
      (fun receiver rules ->
         if receiver.receiver_active then
           match receiver.receiver_rule with
             | None -> rules
             | Some rule -> add_rule rule rules
         else
           rules)
      group.receiver_group_receivers []
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
  if not (String_set.equal rules group.receiver_group_rules) then
   Lwt_mutex.with_lock group.receiver_group_mutex
     (fun () ->
        let new_rules = String_set.diff rules group.receiver_group_rules
        and old_rules = String_set.diff group.receiver_group_rules rules in
        group.receiver_group_rules <- new_rules;
        let connection = descr.receiver_group.receiver_group_connection in
        lwt () = String_set.fold (fun rule thread -> thread <&> OBus_private_bus.add_match connection rule) new_rules (return ())
        and () = String_set.fold (fun rule thread -> thread <&> OBus_private_bus.remove_match connection rule) old_rules (return ()) in
        return ())
 else
   return ()

(* +-----------------------------------------------------------------+
   | Signal initialisation                                           |
   +-----------------------------------------------------------------+ *)

let init_signal descr =
  let connection = descr.receiver_group.receiver_group_connection in
  try_lwt
    if OBus_connection.name connection = None then begin
      (* If the connection is a peer-to-peer connection, there is
         nothing else to do. *)
      descr.receiver.receiver_active <- true;
      descr.done_waiter
    end else begin
      lwt resolver =
        match descr.sender with
          | None ->
              return None
          | Some name ->
              (* If we are interested on signals coming from a
                 particular peer, we need a name resolver: *)
              lwt resolver = OBus_resolver.make connection name in
              descr.receiver.receiver_sender <- Some (OBus_resolver.owner resolver);
              return (Some resolver)
      in
      try_lwt
        (* Yield to let the user add argument filters: *)
        lwt () = pause () in
        (* Since we yielded, check the connection again: *)
        check_connection connection;
        if descr.state = Sig_disconnected then
          return ()
        else begin
          descr.receiver.receiver_active <- true;
          lwt () = commit_rules descr in
          descr.done_waiter
        end
      finally
        match resolver with
          | None -> return ()
          | Some resolver -> OBus_resolver.disable resolver
    end
  finally
    Lwt_sequence.remove descr.node;
    lwt () = commit_rules descr in
    if Lwt_sequence.is_empty descr.receiver_group.receiver_group_receivers then begin
      let running = running_of_connection connection in
      running.running_receiver_groups <- (
        Signal_map.remove
          (descr.receiver_group.receiver_group_path,
           descr.receiver_group.receiver_group_interface,
           descr.receiver_group.receiver_group_member)
          running.running_receiver_groups;
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
  let descr = signal.descr in
   if descr.state <> Sig_disconnected then begin
     descr.state <- Sig_disconnected;
     wakeup descr.done_wakener ()
   end

let stop descr () =
  if descr.state <> Sig_disconnected then begin
    descr.state <- Sig_disconnected;
    wakeup descr.done_wakener ()
  end

let connect info proxy =
  let connection = OBus_proxy.connection proxy in
  let running = running_of_connection (OBus_proxy.connection proxy) in
  (* Signal groups are indexed by tuples [(path, interface, member)]: *)
  let key = (OBus_proxy.path proxy,
             OBus_member.Signal.interface info,
             OBus_member.Signal.member info) in
  let group =
    match try Some(Signal_map.find key running.running_receiver_groups) with Not_found -> None with
      | Some group ->
          group
      | None ->
          (* If the group do not exists, create a new one *)
          let group = {
            receiver_group_rules = String_set.empty;
            receiver_group_mutex = Lwt_mutex.create ();
            receiver_group_receivers = Lwt_sequence.create ();
            receiver_group_connection = connection;
            receiver_group_path = OBus_proxy.path proxy;
            receiver_group_interface = OBus_member.Signal.interface info;
            receiver_group_member = OBus_member.Signal.member info;
          } in
          running.running_receiver_groups <- Signal_map.add key group running.running_receiver_groups;
          group
  in
  let event, push = React.E.create () in
  let receiver = {
    receiver_active = false;
    receiver_sender = None;
    receiver_rule = (
      Some(OBus_match.rule
             ~typ:`Signal
             ?sender:(OBus_proxy.name proxy)
             ~path:(OBus_proxy.path proxy)
             ~interface:(OBus_member.Signal.interface info)
             ~member:(OBus_member.Signal.member info)
             ())
    );
    receiver_filter = (fun _ -> true);
    receiver_push = push;
  } in
  (* Immediatly add the recevier to avoid race condition *)
  let node = Lwt_sequence.add_r receiver group.receiver_group_receivers in

  (* Thread which is wake up when the signal is disconnected *)
  let done_waiter, done_wakener = Lwt.wait () in

  let descr = {
    state = Sig_init;
    auto_match_rule = true;
    filters = [];
    done_waiter = done_waiter;
    done_wakener = done_wakener;
    receiver = receiver;
    receiver_group = group;
    node = node;
    sender = OBus_proxy.name proxy;
  } in
  let signal = {
    event = Lwt_event.with_finaliser (stop descr) (React.E.fmap (cast info) event);
    descr = descr;
  } in
  ignore (init_signal descr);
  signal

let event signal = React.E.map snd signal.event
let event_with_context signal = signal.event

(* +-----------------------------------------------------------------+
   | Signal settings                                                 |
   +-----------------------------------------------------------------+ *)

let auto_match_rule signal = signal.descr.auto_match_rule

let set_auto_match_rule signal auto_match_rule  =
  let descr = signal.descr in
  if auto_match_rule <> descr.auto_match_rule then begin
    descr.auto_match_rule <- auto_match_rule;
    if auto_match_rule then begin
      let rule =
        OBus_match.rule
          ~typ:`Signal
          ?sender:descr.sender
          ~path:descr.receiver_group.receiver_group_path
          ~interface:descr.receiver_group.receiver_group_interface
          ~member:descr.receiver_group.receiver_group_member
          ~arguments:(OBus_match.make_arguments descr.filters)
          ()
      in
      (* Use the sorted list of argument filters: *)
      let filters = OBus_match.arguments rule in
      descr.receiver.receiver_filter <- (fun message -> OBus_match.match_values filters (OBus_message.body message));
      descr.receiver.receiver_rule <- Some rule
    end else
      descr.receiver.receiver_rule <- None;
    match descr.state with
      | Sig_init ->
          ()
      | Sig_disconnected ->
          invalid_arg "OBus_signal.set_auto_match_rule: disconnected signal"
      | Sig_connected ->
          ignore (commit_rules descr)
  end

let set_filters signal filters =
  let descr = signal.descr in
  descr.filters <- filters;
  if descr.auto_match_rule then begin
    let rule =
      OBus_match.rule
        ~typ:`Signal
        ?sender:descr.sender
        ~path:descr.receiver_group.receiver_group_path
        ~interface:descr.receiver_group.receiver_group_interface
        ~member:descr.receiver_group.receiver_group_member
        ~arguments:(OBus_match.make_arguments descr.filters)
        ()
    in
    let filters = OBus_match.arguments rule in
    descr.receiver.receiver_filter <- (fun message -> OBus_match.match_values filters (OBus_message.body message));
    descr.receiver.receiver_rule <- Some rule;
    match descr.state with
      | Sig_init ->
          ()
      | Sig_disconnected ->
          invalid_arg "OBus_signal.set_filters: disconnected signal"
      | Sig_connected ->
          ignore (commit_rules descr)
  end

let init ?(filters=[]) ?(auto_match_rule=true) signal =
  let descr = signal.descr in
  descr.filters <- filters;
  (* Force a refresh of the match rule if necessary: *)
  descr.auto_match_rule <- not auto_match_rule;
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
