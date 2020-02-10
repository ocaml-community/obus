(*
 * oBus_member.ml
 * --------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open OBus_introspect

let introspect_arguments args =
  List.map2
    (fun name typ -> (name, typ))
    (OBus_value.arg_names args)
    (OBus_value.C.type_sequence (OBus_value.arg_types args))

module Method =
struct
  type ('a, 'b) t = {
    interface : OBus_name.interface;
    member : OBus_name.member;
    i_args : 'a OBus_value.arguments;
    o_args : 'b OBus_value.arguments;
    annotations : OBus_introspect.annotation list;
  }

  let make ~interface ~member ~i_args ~o_args ~annotations = {
    interface = interface;
    member = member;
    i_args = i_args;
    o_args = o_args;
    annotations = annotations;
  }

  let interface m = m.interface
  let member m = m.member
  let i_args m = m.i_args
  let o_args m = m.o_args
  let annotations m = m.annotations

  let introspect m =
    Method(m.member, introspect_arguments m.i_args, introspect_arguments m.o_args, m.annotations)
end

module Signal =
struct
  type 'a t = {
    interface : OBus_name.interface;
    member : OBus_name.member;
    args : 'a OBus_value.arguments;
    annotations : OBus_introspect.annotation list;
  }

  let make ~interface ~member ~args ~annotations = {
    interface = interface;
    member = member;
    args = args;
    annotations = annotations;
  }

  let interface s = s.interface
  let member s = s.member
  let args s = s.args
  let annotations s = s.annotations

  let introspect s =
    Signal(s.member, introspect_arguments s.args, s.annotations)
end

module Property =
struct
  type 'a access =
    | Readable
    | Writable
    | Readable_writable

  let readable = Readable
  let writable = Writable
  let readable_writable = Readable_writable

  type ('a, 'access) t = {
    interface : OBus_name.interface;
    member : OBus_name.member;
    typ : 'a OBus_value.C.single;
    access : 'access access;
    annotations : OBus_introspect.annotation list;
  }

  let make ~interface ~member ~typ ~access ~annotations = {
    interface = interface;
    member = member;
    typ = typ;
    access = access;
    annotations = annotations;
  }

  let interface p = p.interface
  let member p = p.member
  let typ p = p.typ
  let access p = p.access
  let annotations p = p.annotations

  let introspect p =
    Property(p.member, OBus_value.C.type_single p.typ,
             (match p.access with
                | Readable -> Read
                | Writable -> Write
                | Readable_writable -> Read_write),
             p.annotations)
end
