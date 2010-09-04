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

(* +-----------------------------------------------------------------+
   | Signal descriptors                                              |
   +-----------------------------------------------------------------+ *)

type 'a t = {
  interface : OBus_name.interface;
  (* The interface of the signal. *)

  member : OBus_name.member;
  (* The name of the signal. *)

  proxy : OBus_proxy.t;
  (* The proxy emitting the signal. *)

  map : (OBus_context.t * OBus_value.V.sequence) React.event -> (OBus_context.t * 'a) React.event;
  (* The function which maps the event into an event holding values of
     type ['a]. *)

  filters : OBus_match.arguments;
  (* Argument filters. *)

  match_rule : bool;
  (* Whether the managed mode for the match rule is enabled *)
}

let empty_filters = OBus_match.make_arguments []

(* Cast a message body into an ocaml value: *)
let cast signal (context, body) =
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

let make signal proxy = {
  interface = OBus_member.Signal.interface signal;
  member = OBus_member.Signal.member signal;
  proxy = proxy;
  map = React.E.fmap (cast signal);
  filters = empty_filters;
  match_rule = OBus_connection.name (OBus_proxy.connection proxy) <> "";
}

(* +-----------------------------------------------------------------+
   | Signals transformations and parameters                          |
   +-----------------------------------------------------------------+ *)

let map_event f sd =
  { sd with map = fun event -> f (sd.map event) }

let map f sd =
  { sd with map = fun event -> React.E.map (fun (context, value) -> (context, f value)) (sd.map event) }

let map_with_context f sd =
  { sd with map = fun event -> React.E.map (fun (context, value) -> (context, f context value)) (sd.map event) }

let with_context sd =
  { sd with map = fun event -> React.E.map (fun (context, value) -> (context, (context, value))) (sd.map event) }

let with_filters filters sd =
  { sd with filters }

let with_match_rule match_rule sd =
  { sd with match_rule }

(* +-----------------------------------------------------------------+
   | Signals dispatching                                             |
   +-----------------------------------------------------------------+ *)

module Signal_map = Map.Make
  (struct
     type t = OBus_path.t * OBus_name.interface * OBus_name.member
     let compare = Pervasives.compare
   end)

type info = {
  mutable senders : (OBus_context.t * OBus_value.V.sequence -> unit) Lwt_sequence.t Signal_map.t;
  (* Signal senders *)
}

let dispatch connection info message =
  match OBus_message.typ message with
    | OBus_message.Signal(path, interface, member) -> begin
        match try Some(Signal_map.find (path, interface, member) info.senders) with Not_found -> None with
          | Some senders ->
              Lwt_sequence.iter_l
                (fun send ->
                   try
                     send (OBus_context.make connection message, OBus_message.body message)
                   with exn ->
                     ignore (Lwt_log.error ~section ~exn "signal event failed with"))
                senders;
              Some message
          | None ->
              Some message
      end
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
  let connection = OBus_proxy.connection sd.proxy
  and name = OBus_proxy.name sd.proxy
  and path = OBus_proxy.path sd.proxy in

  (* Switch freeing resources allocated for this signal: *)
  let resources_switch = Lwt_switch.create () in

  try_lwt
    (* Add the match rule if requested: *)
    lwt () =
      if sd.match_rule then
        OBus_match.export
          ~switch:resources_switch
          connection
          (OBus_match.rule
             ~typ:`Signal
             ~sender:name
             ~path
             ~interface:sd.interface
             ~member:sd.member
             ())
      else
        return ()

    (* Plus the resolver if needed: *)
    and owner_option =
      if OBus_connection.name connection <> "" && name <> "" then
        if OBus_name.is_unique name then
          return (Some (React.S.const name))
        else
          lwt owner = OBus_resolver.make ~switch:resources_switch connection name in
          return (Some owner)
      else
        return None
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
      match try Some(Signal_map.find (path, sd.interface, sd.member) info.senders) with Not_found -> None with
        | Some senders ->
            senders
        | None ->
            let senders = Lwt_sequence.create () in
            info.senders <- Signal_map.add (path, sd.interface, sd.member) senders info.senders;
            senders
    in

    let event, send = React.E.create () in
    let node = Lwt_sequence.add_r send senders in

    let event =
      React.E.filter
        (fun (context, body) ->
           match owner_option with
             | Some owner when React.S.value owner <> OBus_peer.name (OBus_context.sender context) ->
                 false
             | _ ->
                 OBus_match.match_values sd.filters body)
        event
    in

    let disconnect = lazy(
      try_lwt
        Lwt_sequence.remove node;
        if Lwt_sequence.is_empty senders then
          info.senders <- Signal_map.remove (path, sd.interface, sd.member) info.senders;
        Lwt_switch.turn_off resources_switch
      with exn ->
        lwt () =
          Lwt_log.warning_f
            ~section
            ~exn
            "failed to disconnect signal \"%s.%s\" of object \"%s\" from \"%s\""
            sd.interface
            sd.member
            (OBus_path.to_string (OBus_proxy.path sd.proxy))
            (OBus_proxy.name sd.proxy)
        in
        raise_lwt exn
    ) in

    let event = Lwt_event.with_finaliser (finalise disconnect) (React.E.map snd (sd.map event)) in

    lwt () =
      Lwt_switch.add_hook_or_exec
        switch
        (fun () ->
           React.E.stop event;
           Lazy.force disconnect)
    in

    return event
  with exn ->
    lwt () = Lwt_switch.turn_off resources_switch in
    raise_lwt exn

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
