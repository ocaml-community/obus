open Lwt
open OBus_value

lwt () =
  lwt bus = OBus_bus.session () in

  (* Create the peer: *)
  let peer = OBus_peer.make ~name:"org.freedesktop.DBus" ~connection:bus in

  (* Create the proxy: *)
  let proxy = OBus_proxy.make ~peer ~path:["org"; "freedesktop"; "DBus"] in

  (* Call a method: *)
  lwt id =
    OBus_proxy.call proxy
      ~interface:"org.freedesktop.DBus"
      ~member:"GetId"
      ~i_args:C.seq0
      ~o_args:(C.seq1 C.basic_string)
      ()
  in

  Lwt_io.printlf "The bus id is: %s" id
