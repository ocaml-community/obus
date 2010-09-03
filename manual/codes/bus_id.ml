open Lwt

lwt () =
  (* Open a connection to the session message bus: *)
  lwt bus = OBus_bus.session () in

  (* Obtain its id: *)
  lwt id = OBus_bus.get_id bus in

  Lwt_io.printlf "The session bus id is %d." id
