open Lwt
open OBus_members

(* Definition of a method *)
let m_GetId = {
  Method.interface = "org.freedesktop.DBus";
  Method.member = "GetId";
  Method.i_args = C.seq0;
  Method.o_args = C.seq1 C.basic_string;
  Method.annotations = [];
}

(* Definition of a signal *)
let s_NameAcquired = {
  Signal.interface = "org.freedesktop.DBus";
  Signal.member = "NameAcquired";
  Signal.args = C.seq1 (C.basic C.string);
  Signal.annotations = [];
}

lwt () =
  lwt bus = OBus_bus.session () in
  let proxy =
    OBus_proxy.make
      (OBus_peer.make bus "org.freedesktop.DBus")
      ["org"; "freedesktop"; "DBus"]
  in

  (* Call the method we just defined: *)
  lwt id = OBus_method.call m_GetId proxy () in

  (* Register to the signal we just defined: *)
  lwt event = OBus_signal.connect (OBus_signal.make s_NameAcquired proxy) in

  Lwt_event.always_notify_p
    (fun name ->
       Lwt_io.printlf "name acquired: %s" name)
    event;

  Lwt_io.printlf "The message bus id is %s" id
