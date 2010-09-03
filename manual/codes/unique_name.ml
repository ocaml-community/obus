open Lwt

lwt () =
  (* Connects to the session bus: *)
  lwt bus = OBus_bus.session () in

  (* Read our unique name: *)
  let name = OBus_bus.name bus in

  Lwt_io.printlf "My unique connection name is %s." name
