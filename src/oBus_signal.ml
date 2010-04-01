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
open OBus_private

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

type 'a t = {
  mutable state : signal_state;
  mutable event : 'a React.event;
  mutable auto_match_rule : bool;
  mutable filters : (int * OBus_match.argument_filter) list;

  receiver : signal_receiver;
  set : signal_receiver_set;
  node : signal_receiver Lwt_sequence.node;

  connection : OBus_connection.t;
  sender : OBus_name.bus option;
  path : OBus_path.t;
  interface : OBus_name.interface;
  member : OBus_name.member;

  (* Thread which is wake up when the signal is disconnected: *)
  done_waiter : unit Lwt.t;
  done_wakener : unit Lwt.u;
}

let event signal = signal.event
let auto_match_rule signal = signal.auto_match_rule

let disconnect signal =
  if signal.state <> Sig_disconnected then begin
    signal.state <- Sig_disconnected;
    wakeup signal.done_wakener ()
  end

(* +-----------------------------------------------------------------+
   | Rules computing and setting                                     |
   +-----------------------------------------------------------------+ *)

(* Commit rules changes on the message bus *)
let commit_rules signal =
  let set = signal.set in
  let rules =
    Lwt_sequence.fold_l
      (fun sr rules ->
         if sr.sr_active then
           match sr.sr_rule with
             | "" -> rules
             | rule -> StringSet.add rule rules
         else
           rules)
      set.srs_receivers StringSet.empty
  in

  (* If rules have changed, compute the minimal set of changes to make
     with the message bus and do them: *)
  if not (StringSet.equal rules set.srs_rules) then
    Lwt_mutex.with_lock set.srs_mutex
      (fun () ->
         let new_rules = StringSet.diff rules set.srs_rules
         and old_rules = StringSet.diff set.srs_rules rules in
         set.srs_rules <- new_rules;
         lwt () = StringSet.fold (fun rule thread -> thread <&> OBus_private_bus.add_match signal.connection rule) new_rules (return ())
         and () = StringSet.fold (fun rule thread -> thread <&> OBus_private_bus.remove_match signal.connection rule) old_rules (return ()) in
         return ())
  else
    return ()

(* +-----------------------------------------------------------------+
   | Signal initialisation                                           |
   +-----------------------------------------------------------------+ *)

let init_signal signal =
  try_lwt
    if OBus_connection.name signal.connection = None then begin
      (* If the connection is a peer-to-peer connection, there is
         nothing else to do. *)
      signal.receiver.sr_active <- true;
      signal.done_waiter
    end else begin
      lwt resolver =
        match signal.sender with
          | None ->
              return None
          | Some name ->
              (* If we are interested on signals coming from a
                 particular peer, we need a name resolver: *)
              lwt resolver = OBus_resolver.make signal.connection name in
              signal.receiver.sr_sender <- Some(OBus_resolver.owner resolver);
              return (Some resolver)
      in
      try_lwt
        (* Yield to let the user add argument filters: *)
        lwt () = pause () in
        (* Since we yielded, check the connection again: *)
        check_connection signal.connection;
        if signal.state = Sig_disconnected then
          return ()
        else begin
          signal.receiver.sr_active <- true;
          lwt () = commit_rules signal in
          signal.done_waiter
        end
      finally
        match resolver with
          | None -> return ()
          | Some resolver -> OBus_resolver.disable resolver
    end
  finally
    React.E.stop signal.event;
    Lwt_sequence.remove signal.node;
    lwt () = commit_rules signal in
    if Lwt_sequence.is_empty signal.set.srs_receivers then begin
      let connection = unpack_connection signal.connection in
      connection.signal_receivers <- SignalMap.remove (signal.path, signal.interface, signal.member) connection.signal_receivers;
    end;
    return ()

(* +-----------------------------------------------------------------+
   | Signal creation                                                 |
   +-----------------------------------------------------------------+ *)

let stop signal () = disconnect signal

(* Creates a signal receiver. [event] is the event that is returned to
   the caller, and [push] the function passed to the connection to
   dispatch signals. *)
let connect_backend ~connection ?sender ~path ~interface ~member ~event ~push () =
  let connection = unpack_connection connection in
  (* Signal sets are indexed by tuples [(path, interface, member)]: *)
  let key = (path, interface, member) in
  let set =
    match SignalMap.lookup key connection.signal_receivers with
      | Some set ->
          set
      | None ->
          (* If the set do not exists, create a new one *)
          let set = {
            srs_rules = StringSet.empty;
            srs_mutex = Lwt_mutex.create ();
            srs_receivers = Lwt_sequence.create ();
          } in
          connection.signal_receivers <- SignalMap.add key set connection.signal_receivers;
          set
  in
  let receiver = {
    sr_active = false;
    sr_sender = None;
    sr_rule = OBus_match.string_of_rule (OBus_match.rule ~typ:`Signal ?sender ~path ~interface ~member ());
    sr_push = push;
  } in
  (* Immediatly add the recevier to avoid race condition *)
  let node = Lwt_sequence.add_r receiver set.srs_receivers in

  (* Thread which is wake up when the signal is disconnected *)
  let done_waiter, done_wakener = Lwt.wait () in

  let signal = {
    state = Sig_init;
    event = event;
    auto_match_rule = true;
    filters = [];
    done_waiter = done_waiter;
    done_wakener = done_wakener;
    receiver = receiver;
    set = set;
    node = node;
    connection = connection.packed;
    sender = sender;
    path = path;
    interface = interface;
    member = member;
  } in
  signal.event <- Lwt_event.with_finaliser (stop signal) event;
  ignore (init_signal signal);
  signal

(* +-----------------------------------------------------------------+
   | Signal settings                                                 |
   +-----------------------------------------------------------------+ *)

let set_auto_match_rule signal auto_match_rule  =
  if auto_match_rule <> signal.auto_match_rule then begin
    signal.auto_match_rule <- auto_match_rule;
    if auto_match_rule then
      signal.receiver.sr_rule <- (OBus_match.string_of_rule
                                    (OBus_match.rule
                                       ~typ:`Signal
                                       ?sender:signal.sender
                                       ~path:signal.path
                                       ~interface:signal.interface
                                       ~member:signal.member
                                       ~arguments:signal.filters
                                       ()))
    else
      signal.receiver.sr_rule <- "";
    match signal.state with
      | Sig_init ->
          ()
      | Sig_disconnected ->
          invalid_arg "OBus_signal.set_auto_match_rule: disconnected signal"
      | Sig_connected ->
          ignore (commit_rules signal)
  end

let set_filters signal filters =
  signal.filters <- filters;
  if signal.auto_match_rule then begin
    signal.receiver.sr_rule <- (OBus_match.string_of_rule
                                  (OBus_match.rule
                                     ~typ:`Signal
                                     ?sender:signal.sender
                                     ~path:signal.path
                                     ~interface:signal.interface
                                     ~member:signal.member
                                     ~arguments:signal.filters
                                     ()));
    match signal.state with
      | Sig_init ->
          ()
      | Sig_disconnected ->
          invalid_arg "OBus_signal.set_filters: disconnected signal"
      | Sig_connected ->
          ignore (commit_rules signal)
  end

let init ?(filters=[]) ?(auto_match_rule=true) signal =
  signal.filters <- filters;
  (* Force a refresh of the match rule if necessary: *)
  signal.auto_match_rule <- not auto_match_rule;
  set_auto_match_rule signal auto_match_rule;
  signal.event

(* +-----------------------------------------------------------------+
   | Signal connection                                               |
   +-----------------------------------------------------------------+ *)

let dyn_connect ~connection ?sender ~path ~interface ~member () =
  let event, push = React.E.create () in
  connect_backend
    ~connection
    ?sender
    ~path
    ~interface
    ~member
    ~push
    ~event:(React.E.map (fun (connection, message) -> OBus_message.body message) event)
    ()

let cast interface member typ (connection, message) =
  try
    Some(OBus_type.cast_sequence typ ~context:(connection, message) (OBus_message.body message))
  with exn ->
    ignore (
      Lwt_log.error_f ~section ~exn "failed to cast signal from %S, interface %S, member %S with signature %S to %S"
        (match OBus_message.sender message with None -> "" | Some n -> n) interface member
        (OBus_value.string_of_signature (OBus_value.type_of_sequence (OBus_message.body message)))
        (OBus_value.string_of_signature (OBus_type.type_sequence typ))
    );
    None

let connect ~connection ?sender ~path ~interface ~member typ =
  let event, push = React.E.create () in
  connect_backend
    ~connection
    ?sender
    ~path
    ~interface
    ~member
    ~push
    ~event:(React.E.fmap (cast interface member typ) event)
    ()

(* +-----------------------------------------------------------------+
   | Emitting signals                                                |
   +-----------------------------------------------------------------+ *)

let emit ~connection ?flags ?sender ?destination ~path ~interface ~member ty x =
  OBus_connection.send_message connection (OBus_message.signal ?flags ?sender ?destination ~path ~interface ~member (OBus_type.make_sequence ty x))

let dyn_emit ~connection ?flags ?sender ?destination ~path ~interface ~member body =
  OBus_connection.send_message connection (OBus_message.signal ?flags ?sender ?destination ~path ~interface ~member body)
