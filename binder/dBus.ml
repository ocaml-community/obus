(*
 * dBus.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

include Common

open Type

type dbus_id =
  | Byte
  | Boolean
  | Int16
  | Int32
  | Int64
  | Uint16
  | Uint32
  | Uint64
  | Double
  | String
  | Signature
  | Object_path
  | Array
  | Dict
  | Structure
  | Variant
type 'a dbus_type = (dbus_id, 'a) Type.term

let dbyte = Type(Byte, [])
let dboolean = Type(Boolean, [])
let dint16 = Type(Int16, [])
let dint32 = Type(Int32, [])
let dint64 = Type(Int64, [])
let duint16 = Type(Uint16, [])
let duint32 = Type(Uint32, [])
let duint64 = Type(Uint64, [])
let ddouble = Type(Double, [])
let dstring = Type(String, [])
let dsignature = Type(Signature, [])
let dobject_path = Type(Object_path, [])
let darray t = Type(Array, [t])
let ddict k v = Type(Dict, [k; v])
let dstructure l = Type(Structure, [l])
let dvariant = Type(Variant, [])

let rec dbus_type_of_dtype = function
  | Tbyte -> dbyte
  | Tboolean -> dboolean
  | Tint16 -> dint16
  | Tint32 -> dint32
  | Tint64 -> dint64
  | Tuint16 -> duint16
  | Tuint32 -> duint32
  | Tuint64 -> duint64
  | Tdouble -> ddouble
  | Tstring -> dstring
  | Tsignature -> dsignature
  | Tobject_path -> dobject_path
  | Tarray(t) -> darray (dbus_type_of_dtype t)
  | Tdict(k, v) -> ddict (dbus_type_of_dtype k) (dbus_type_of_dtype v)
  | Tstructure(l) -> dstructure (dbus_type_of_dtypes l)
  | Tvariant -> dvariant
and dbus_type_of_dtypes l = tuple (List.map dbus_type_of_dtype l)

type interface = dtypes Sig.t

open Xparser

type direction = In | Out

let rename_args args =
  snd begin List.fold_right begin fun (name, typ) (i, l) ->
    match name with
      | "?" -> (i + 1, Sig.Arg("__unamed" ^ (string_of_int i), typ) :: l)
      | _ -> (i, Sig.Arg(name, typ) :: l)
  end args (0, []) end

(*type value = string
type annotation = Annotation of name * value
type argument = Arg of name * Values.dtype
type access = Read | Write | Read_write
type definition =
  | Method of name * (*in*) argument list * (*out*) argument list * annotation list
  | Signal of name * argument list * annotation list
  | Property of name * Values.dtype * access * annotation list
type signature = Interface of name * definition list * annotation list*)

let from_xml xml =
  parse (elt "node" [<>]
           (s2
              (any (elt "interface"  [< (P"name") >]
                      (s1 (union
                             [elt "method" [< (P"name") >]
                                (s2
                                   (any (elt "arg" [< (D("name","?")); (A("direction", "in", ["in"])); (P"type") >]
                                           s0
                                           (fun name _ typ ->
                                              (name, type_of_string typ))))
                                   (any (elt "arg" [< (D("name","?")); (A("direction", "in", ["out"])); (P"type") >]
                                           s0
                                           (fun name _ typ ->
                                              (name, type_of_string typ)))))
                                (fun name ins outs -> Sig.Method(name, rename_args ins, rename_args outs));
                              elt "signal" [< (P"name") >]
                                (s1 (any (elt "arg" [< (D("name","?")); (P"type") >]
                                            s0
                                            (fun name typ -> (name, type_of_string typ)))))
                                (fun name args -> Sig.Signal(name, rename_args args))]))
                      (fun name defs -> Sig.Sig(name, defs))))
              (any (elt "node" [< (P"name") >]
                      s0
                      (fun x -> x))))
           (fun interfs _ -> interfs)) xml
