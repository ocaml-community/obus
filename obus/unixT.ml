(*
 * unixT.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

let make_transport fd =
  Transport.unix_like (Transport.Unix fd) (Unix.read fd) (Unix.write fd)
    (fun () ->
       Unix.shutdown fd Unix.SHUTDOWN_ALL;
       Unix.close fd)

let _ =
  Transport.register_maker begin function
    | Address.Unix path ->
        let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
          Unix.connect fd (Unix.ADDR_UNIX(path));
          Some(make_transport fd)
    | _ -> None
  end
