(*
 * oBus_signal.ml
 * --------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let section = Lwt_log.Section.make "obus(signal)"

open Lwt_react

(* +-----------------------------------------------------------------+
   | Signal descriptors                                              |
   +-----------------------------------------------------------------+ *)

type 'a t = {
  interface : OBus_name.interface;
  (* The interface of the signal. *)

  member : OBus_name.member;
  (* The name of the signal. *)

  peer : OBus_peer.t;
  (* The peer emitting the signal. *)

  path : OBus_path.t option;
  (* The path of the object emitting the signa or [None] if we want to
     match signals comming from any objects. *)

  map : (OBus_context.t * OBus_path.t * OBus_value.V.sequence) event -> (OBus_context.t * 'a) event;
  (* The function which maps the event into an event holding values of
     type ['a]. *)

  filters : OBus_match.arguments;
  (* Argument filters. *)

  match_rule : bool;
  (* Whether the managed mode for the match rule is enabled *)
}

let empty_filters = OBus_match.make_arguments []

(* Cast a message body into an ocaml value: *)
let cast signal (context, path, body) =
  try
    Some(context,
         OBus_value.C.cast_sequence
           (OBus_value.arg_types
              (OBus_member.Signal.args signal))
           body)
  with OBus_value.C.Signature_mismatch ->
    ignore (
      Lwt_log.error_f ~section "failed to cast signal from %S, interface %S, member %S with signature %S to %S"
        (OBus_peer.name (OBus_context.sender context))
        (OBus_member.Signal.interface signal)
        (OBus_member.Signal.member signal)
        (OBus_value.string_of_signature
           (OBus_value.V.type_of_sequence body))
        (OBus_value.string_of_signature
           (OBus_value.C.type_sequence
              (OBus_value.arg_types
                 (OBus_member.Signal.args signal))))
    );
    None

let cast_any signal (context, path, body) =
  match cast signal (context, path, body) with
    | Some(context, v) -> Some(context, (OBus_proxy.make (OBus_context.sender context) path, v))
    | None -> None

let make signal proxy = {
  interface = OBus_member.Signal.interface signal;
  member = OBus_member.Signal.member signal;
  peer = OBus_proxy.peer proxy;
  path = Some(OBus_proxy.path proxy);
  map = E.fmap (cast signal);
  filters = empty_filters;
  match_rule = OBus_connection.name (OBus_proxy.connection proxy) <> "";
}

let make_any signal peer = {
  interface = OBus_member.Signal.interface signal;
  member = OBus_member.Signal.member signal;
  peer = peer;
  path = None;
  map = E.fmap (cast_any signal);
  filters = empty_filters;
  match_rule = OBus_connection.name (OBus_peer.connection peer) <> "";
}

(* +-----------------------------------------------------------------+
   | Signals transformations and parameters                          |
   +-----------------------------------------------------------------+ *)

let map_event f sd =
  { sd with map = fun event -> f (sd.map event) }

let map f sd =
  { sd with map = fun event -> E.map (fun (context, value) -> (context, f value)) (sd.map event) }

let map_with_context f sd =
  { sd with map = fun event -> E.map (fun (context, value) -> (context, f context value)) (sd.map event) }

let with_context sd =
  { sd with map = fun event -> E.map (fun (context, value) -> (context, (context, value))) (sd.map event) }

let with_filters filters sd =
  { sd with filters }

let with_match_rule match_rule sd =
  { sd with match_rule }

(* +-----------------------------------------------------------------+
   | Signals dispatching                                             |
   +-----------------------------------------------------------------+ *)

module Signal_map = Map.Make
  (struct
     type t = OBus_path.t option * OBus_name.interface * OBus_name.member
     let compare = Pervasives.compare
   end)

type info = {
  mutable senders : (OBus_context.t * OBus_path.t * OBus_value.V.sequence -> unit) Lwt_sequence.t Signal_map.t;
}

let dispatch connection info message =
  match OBus_message.typ message with
    | OBus_message.Signal(path, interface, member) ->
        begin
          match try Some(Signal_map.find (Some path, interface, member) info.senders) with Not_found -> None with
            | Some senders ->
                Lwt_sequence.iter_l
                  (fun send ->
                     try
                       send (OBus_context.make connection message, path, OBus_message.body message)
                     with exn ->
                       ignore (Lwt_log.error ~section ~exn "signal event failed with"))
                  senders
            | None ->
                ()
        end;
        begin
          match try Some(Signal_map.find (None, interface, member) info.senders) with Not_found -> None with
            | Some senders ->
                Lwt_sequence.iter_l
                  (fun send ->
                     try
                       send (OBus_context.make connection message, path, OBus_message.body message)
                     with exn ->
                       ignore (Lwt_log.error ~section ~exn "signal event failed with"))
                  senders
            | None ->
                ()
        end;
        Some message
    | _ ->
        Some message

(* +-----------------------------------------------------------------+
   | Signals connection                                              |
   +-----------------------------------------------------------------+ *)

let finalise disconnect _ =
  ignore (Lazy.force disconnect)

let key = OBus_connection.new_key ()

let connect ?switch sd =
  Lwt_switch.check switch;
  let connection = OBus_peer.connection sd.peer and name = OBus_peer.name sd.peer in

  (* Switch freeing resources allocated for this signal: *)
  let resources_switch = Lwt_switch.create () in

  try%lwt
    (* Add the match rule if requested: *)
    let%lwt () =
      if sd.match_rule then
        OBus_match.export
          ~switch:resources_switch
          connection
          (OBus_match.rule
             ~typ:`Signal
             ~sender:name
             ?path:sd.path
             ~interface:sd.interface
             ~member:sd.member
             ())
      else
        Lwt.return ()

    (* Plus the resolver if needed: *)
    and owner_option =
      if OBus_connection.name connection <> "" && name <> "" then
        if OBus_name.is_unique name then
          Lwt.return (Some (S.const name))
        else
          let%lwt owner = OBus_resolver.make ~switch:resources_switch connection name in
          Lwt.return (Some owner)
      else
        Lwt.return None
    in

    let info =
      match OBus_connection.get connection key with
        | Some info ->
            info
        | None ->
            let info = {
              senders = Signal_map.empty;
            } in
            OBus_connection.set connection key (Some info);
            let _ = Lwt_sequence.add_l (dispatch connection info) (OBus_connection.incoming_filters connection) in
            info
    in

    let senders =
      match try Some(Signal_map.find (sd.path, sd.interface, sd.member) info.senders) with Not_found -> None with
        | Some senders ->
            senders
        | None ->
            let senders = Lwt_sequence.create () in
            info.senders <- Signal_map.add (sd.path, sd.interface, sd.member) senders info.senders;
            senders
    in

    let event, send = E.create () in
    let send v = send v in
    let node = Lwt_sequence.add_r send senders in

    let event =
      E.filter
        (fun (context, path, body) ->
           match owner_option with
             | Some owner when S.value owner <> OBus_peer.name (OBus_context.sender context) ->
                 false
             | _ ->
                 OBus_match.match_values sd.filters body)
        event
    in

    let disconnect = lazy(
      try%lwt
        Lwt_sequence.remove node;
        if Lwt_sequence.is_empty senders then
          info.senders <- Signal_map.remove (sd.path, sd.interface, sd.member) info.senders;
        Lwt_switch.turn_off resources_switch
      with exn ->
        let%lwt () =
          Lwt_log.warning_f
            ~section
            ~exn
            "failed to disconnect signal \"%s.%s\" of object \"%s\" from \"%s\""
            sd.interface
            sd.member
            (match sd.path with
               | Some path -> OBus_path.to_string path
               | None -> "<any>")
            (OBus_peer.name sd.peer)
        in
        Lwt.fail exn
    ) in

    let event = E.with_finaliser (finalise disconnect) (E.map snd (sd.map event)) in

    let%lwt () =
      Lwt_switch.add_hook_or_exec
        switch
        (fun () ->
           E.stop event;
           Lazy.force disconnect)
    in

    Lwt.return event
  with exn ->
    let%lwt () = Lwt_switch.turn_off resources_switch in
    Lwt.fail exn

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
