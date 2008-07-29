(*
 * oBus_pervasives.ml
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_types
open OBus_wire
open OBus_annot
open OBus_comb

let (-->) = abstract
let ob_reply = reply

let ob_byte = make dbyte rbyte wbyte
let ob_char = ob_byte
let ob_boolean = make dboolean rboolean wboolean
let ob_bool = ob_boolean
let ob_int8 = make dint8 rint8 wint8
let ob_uint8 = make duint8 ruint8 wuint8
let ob_int16 = make dint16 rint16 wint16
let ob_uint16 = make duint16 ruint16 wuint16
let ob_int = make dint rint wint
let ob_uint = make duint ruint wuint
let ob_int32 = make dint32 rint32 wint32
let ob_uint32 = make duint32 ruint32 wuint32
let ob_int64 = make dint64 rint64 wint64
let ob_uint64 = make duint64 ruint64 wuint64
let ob_double = make ddouble rdouble wdouble
let ob_float = ob_double
let ob_string = make dstring rstring wstring
let ob_signature = make dsignature rsignature wsignature
let ob_object_path = make dobject_path robject_path wobject_path
let ob_path = ob_object_path
let ob_proxy = OBus_proxy.ob_t
let ob_list t = make (darray(annot t)) (rlist (annot t) (reader t)) (wlist (annot t) (writer t))
let ob_set t = make (darray(annot t)) (rset (annot t) (reader t)) (wlist (annot t) (writer t))
let ob_assoc tk tv =
  make (ddict (annot tk) (annot tv))
    (rassoc (reader tk) (reader tv))
    (wassoc (writer tk) (writer tv))
let ob_byte_array = make dbyte_array rbyte_array wbyte_array
let ob_structure t = make (dstruct (annot t)) (rstruct (reader t)) (fun x -> wstruct (writer t x))
let ob_variant = make dvariant rvariant wvariant
let ob_unit = make dnil (return ()) (fun _ -> return ())
let ob_pair t1 t2 =
  make (annot t1 ++ annot t2)
    (perform
       x1 <-- reader t1;
       x2 <-- reader t2;
       return (x1, x2))
    (fun (x1, x2) ->
       perform
         writer t1 x1;
         writer t2 x2)

let ob_tuple2 = ob_pair
let ob_tuple3 t1 t2 t3 =
  make (annot t1 ++ annot t2 ++ annot t3)
    (perform
       x1 <-- reader t1;
       x2 <-- reader t2;
       x3 <-- reader t3;
       return (x1, x2, x3))
    (fun (x1, x2, x3) ->
       perform
         writer t1 x1;
         writer t2 x2;
         writer t3 x3)
let ob_tuple4 t1 t2 t3 t4 =
  make (annot t1 ++ annot t2 ++ annot t3 ++ annot t4)
    (perform
       x1 <-- reader t1;
       x2 <-- reader t2;
       x3 <-- reader t3;
       x4 <-- reader t4;
       return (x1, x2, x3, x4))
    (fun (x1, x2, x3, x4) ->
       perform
         writer t1 x1;
         writer t2 x2;
         writer t3 x3;
         writer t4 x4)
let ob_tuple5 t1 t2 t3 t4 t5 =
  make (annot t1 ++ annot t2 ++ annot t3 ++ annot t4 ++ annot t5)
    (perform
       x1 <-- reader t1;
       x2 <-- reader t2;
       x3 <-- reader t3;
       x4 <-- reader t4;
       x5 <-- reader t5;
       return (x1, x2, x3, x4, x5))
    (fun (x1, x2, x3, x4, x5) ->
       perform
         writer t1 x1;
         writer t2 x2;
         writer t3 x3;
         writer t4 x4;
         writer t5 x5)
let ob_tuple6 t1 t2 t3 t4 t5 t6 =
  make (annot t1 ++ annot t2 ++ annot t3 ++ annot t4 ++ annot t5 ++ annot t6)
    (perform
       x1 <-- reader t1;
       x2 <-- reader t2;
       x3 <-- reader t3;
       x4 <-- reader t4;
       x5 <-- reader t5;
       x6 <-- reader t6;
       return (x1, x2, x3, x4, x5, x6))
    (fun (x1, x2, x3, x4, x5, x6) ->
       perform
         writer t1 x1;
         writer t2 x2;
         writer t3 x3;
         writer t4 x4;
         writer t5 x5;
         writer t6 x6)
let ob_tuple7 t1 t2 t3 t4 t5 t6 t7 =
  make (annot t1 ++ annot t2 ++ annot t3 ++ annot t4 ++ annot t5 ++ annot t6 ++ annot t7)
    (perform
       x1 <-- reader t1;
       x2 <-- reader t2;
       x3 <-- reader t3;
       x4 <-- reader t4;
       x5 <-- reader t5;
       x6 <-- reader t6;
       x7 <-- reader t7;
       return (x1, x2, x3, x4, x5, x6, x7))
    (fun (x1, x2, x3, x4, x5, x6, x7) ->
       perform
         writer t1 x1;
         writer t2 x2;
         writer t3 x3;
         writer t4 x4;
         writer t5 x5;
         writer t6 x6;
         writer t7 x7)
let ob_tuple8 t1 t2 t3 t4 t5 t6 t7 t8 =
  make (annot t1 ++ annot t2 ++ annot t3 ++ annot t4 ++ annot t5 ++ annot t6 ++ annot t7 ++ annot t8)
    (perform
       x1 <-- reader t1;
       x2 <-- reader t2;
       x3 <-- reader t3;
       x4 <-- reader t4;
       x5 <-- reader t5;
       x6 <-- reader t6;
       x7 <-- reader t7;
       x8 <-- reader t8;
       return (x1, x2, x3, x4, x5, x6, x7, x8))
    (fun (x1, x2, x3, x4, x5, x6, x7, x8) ->
       perform
         writer t1 x1;
         writer t2 x2;
         writer t3 x3;
         writer t4 x4;
         writer t5 x5;
         writer t6 x6;
         writer t7 x7;
         writer t8 x8)
let ob_tuple9 t1 t2 t3 t4 t5 t6 t7 t8 t9 =
  make (annot t1 ++ annot t2 ++ annot t3 ++ annot t4 ++ annot t5 ++ annot t6 ++ annot t7 ++ annot t8 ++ annot t9)
    (perform
       x1 <-- reader t1;
       x2 <-- reader t2;
       x3 <-- reader t3;
       x4 <-- reader t4;
       x5 <-- reader t5;
       x6 <-- reader t6;
       x7 <-- reader t7;
       x8 <-- reader t8;
       x9 <-- reader t9;
       return (x1, x2, x3, x4, x5, x6, x7, x8, x9))
    (fun (x1, x2, x3, x4, x5, x6, x7, x8, x9) ->
       perform
         writer t1 x1;
         writer t2 x2;
         writer t3 x3;
         writer t4 x4;
         writer t5 x5;
         writer t6 x6;
         writer t7 x7;
         writer t8 x8;
         writer t9 x9)
let ob_tuple10 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 =
  make (annot t1 ++ annot t2 ++ annot t3 ++ annot t4 ++ annot t5 ++ annot t6 ++ annot t7 ++ annot t8 ++ annot t9 ++ annot t10)
    (perform
       x1 <-- reader t1;
       x2 <-- reader t2;
       x3 <-- reader t3;
       x4 <-- reader t4;
       x5 <-- reader t5;
       x6 <-- reader t6;
       x7 <-- reader t7;
       x8 <-- reader t8;
       x9 <-- reader t9;
       x10 <-- reader t10;
       return (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10))
    (fun (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) ->
       perform
         writer t1 x1;
         writer t2 x2;
         writer t3 x3;
         writer t4 x4;
         writer t5 x5;
         writer t6 x6;
         writer t7 x7;
         writer t8 x8;
         writer t9 x9;
         writer t10 x10)
