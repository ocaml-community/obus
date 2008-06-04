(*
 * unixT.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Transport

let make_transport fd =
  make
    ~backend:(Unix fd)
    ~send:(Unix.write fd)
    ~recv:(Unix.read fd)
    ~close:(fun () ->
              Unix.shutdown fd Unix.SHUTDOWN_ALL;
              Unix.close fd)
    ()

let _ =
  register_maker begin fun (_, known, _) -> match known with
    | Address.Unix path ->
        let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
          Unix.connect fd (Unix.ADDR_UNIX(path));
          Some(make_transport fd)
    | _ -> None
  end
