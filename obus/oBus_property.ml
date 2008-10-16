(*
 * oBus_property.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt
open OBus_type
open OBus_value
open OBus_connection

type connection =
  | Direct of OBus_connection.t
  | Lazy of OBus_connection.t Lwt.t Lazy.t

let with_connection c f = match c with
  | Direct c -> f c
  | Lazy c -> Lazy.force c >>= f

type access = [ `readable | `writable ]
let rd_only = `readable
let wr_only = `writable
let rdwr = `readable

type desc = {
  connection : connection;
  destination : OBus_name.connection option;
  path : OBus_path.t;
  interface : OBus_name.interface;
  member : OBus_name.member;
}

type ('a, 'access) t = desc * 'a cl_single
type 'access dt = desc

let call member typ desc =
  kmethod_call (with_connection desc.connection)
    ?destination:desc.destination
    ~path:desc.path
    ~interface:"org.freedesktop.DBus.Properties"
    ~member
    (tstring --> (tstring --> typ))
    desc.interface
    desc.member

let dget = call "Get" << variant >>
let dset = call "Set" << variant -> unit >>

let dget_all ~connection ?destination ~path ~interface =
  method_call connection
    ?destination
    ~path:path
    ~interface:"org.freedesktop.DBus.Properties"
    ~member:"GetAll"
    (<< string -> {string, variant} list >>)
    interface

let dmake ~connection ?destination ~path ~interface ~member ~access = {
  connection = Direct connection;
  destination = destination;
  path = path;
  interface = interface;
  member = member;
}

let make ~connection ?destination ~path ~interface ~member ~access typ =
  (dmake ~connection ?destination ~path ~interface ~member ~access, (typ :> 'a cl_single))

let set (desc, typ) x = dset desc (make_single typ x)
let get (desc, typ) =
  dget desc >>= fun v ->
    match opt_cast_single typ v with
      | Some x -> return x
      | _ ->
          fail (Failure
                  (Printf.sprintf
                     "invalid signature for property %S on interface %S; \
                      expected is %S, got: %S"
                     desc.member desc.interface
                     (string_of_signature [type_single typ])
                     (string_of_signature [type_of_single v])))

let ldmake ~connection ?destination ~path ~interface ~member ~access = {
  connection = Lazy connection;
  destination = destination;
  path = path;
  interface = interface;
  member = member;
}

let lmake ~connection ?destination ~path ~interface ~member ~access typ =
  (ldmake ~connection ?destination ~path ~interface ~member ~access, (typ :> 'a cl_single))
