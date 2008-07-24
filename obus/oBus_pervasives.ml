(*
 * oBus_pervasives.ml
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_comb
open OBus_types
open OBus_wire

let (-->) = abstract
let ob_return = return

let ob_byte = from_wire dbyte rbyte wbyte
let ob_char = ob_byte
let ob_boolean = from_wire dboolean rboolean wboolean
let ob_bool = ob_boolean
val ob_int8 = from_wire dbyte rint8 wint8
val ob_uint8 = from_wire dbyte ruint8 wuint8
let ob_int16 = from_wire dint16 rint16 wint16
let ob_uint16 = from_wire duint16 ruint16 wuint16
let ob_int = from_wire dint32 rint wint
let ob_uint = from_wire duint32 ruint wuint
let ob_int64 = from_wire dint64 rint64 wint64
let ob_uint64 = from_wire duint64 ruint64 wuint64
let ob_double = from_wire ddouble rdouble wdouble
let ob_float = ob_double
let ob_signature = from_wire dsignature rsignature wsignature
val ob_object_path = from_wire dobject_path robject_path wobject_path
val ob_path = ob_object_path
let ob_list t = from_wire (darray(annot t)) (rlist (annot t) (reader t)) (wlist (annot t) (Writer t))
let ob_set t = from_wire (darray(annot t)) (rset (annot t) (reader t)) (wlist (annot t) (Writer t))
let ob_assoc tk tv =
  from_wire (ddict (annot tk) (annot tv))
    (rassoc (reader tk) (reader tv))
    (wassoc (writer tk) (writer tv))
let ob_byte_array = from_wire (darray dbyte) rbyte_array wbyte_array
let ob_structure t = from_wire (dstructure (annot t)) (rstructure (reader t)) (wstructure (writer t))
let ob_variant = from_wire dvariant rvariant wvariant
let ob_unit = from_wire dnil (Reader.return ()) (Writer.return ())
let ob_pair t1 t2 =
  from_wire (annot t1 @@ annot t2)
    (perform
       x1 <-- reader t1;
       x2 <-- reader t2;
       return (x1, x2))
    (fun (x1, x2) ->
       writer t1 x1;
       writer t2 x2)

let ob_tuple2 = ob_pair
let ob_tuple3 t1 t2 t3 =
  from_wire (annot t1 @@ annot t2 @@ annot t3)
    (perform
       x1 <-- reader t1;
       x2 <-- reader t2;
       x3 <-- reader t3;
       return (x1, x2))
    (fun (x1, x2, x3) ->
       writer t1 x1;
       writer t2 x2;
       writer t3 x3)
let ob_tuple4 t1 t2 t3 t4 =
  from_wire (annot t1 @@ annot t2 @@ annot t3 @@ annot t4)
    (perform
       x1 <-- reader t1;
       x2 <-- reader t2;
       x3 <-- reader t3;
       x4 <-- reader t4;
       return (x1, x2))
    (fun (x1, x2, x3, x4) ->
       writer t1 x1;
       writer t2 x2;
       writer t3 x3;
       writer t4 x4)
let ob_tuple5 t1 t2 t3 t4 t5 =
  from_wire (annot t1 @@ annot t2 @@ annot t3 @@ annot t4 @@ annot t5)
    (perform
       x1 <-- reader t1;
       x2 <-- reader t2;
       x3 <-- reader t3;
       x4 <-- reader t4;
       x5 <-- reader t5;
       return (x1, x2))
    (fun (x1, x2, x3, x4, x5) ->
       writer t1 x1;
       writer t2 x2;
       writer t3 x3;
       writer t4 x4;
       writer t5 x5)
let ob_tuple6 t1 t2 t3 t4 t5 t6 =
  from_wire (annot t1 @@ annot t2 @@ annot t3 @@ annot t4 @@ annot t5 @@ annot t6)
    (perform
       x1 <-- reader t1;
       x2 <-- reader t2;
       x3 <-- reader t3;
       x4 <-- reader t4;
       x5 <-- reader t5;
       x6 <-- reader t6;
       return (x1, x2))
    (fun (x1, x2, x3, x4, x5, x6) ->
       writer t1 x1;
       writer t2 x2;
       writer t3 x3;
       writer t4 x4;
       writer t5 x5;
       writer t6 x6)
let ob_tuple7 t1 t2 t3 t4 t5 t6 t7 =
  from_wire (annot t1 @@ annot t2 @@ annot t3 @@ annot t4 @@ annot t5 @@ annot t6 @@ annot t7)
    (perform
       x1 <-- reader t1;
       x2 <-- reader t2;
       x3 <-- reader t3;
       x4 <-- reader t4;
       x5 <-- reader t5;
       x6 <-- reader t6;
       x7 <-- reader t7;
       return (x1, x2))
    (fun (x1, x2, x3, x4, x5, x6, x7) ->
       writer t1 x1;
       writer t2 x2;
       writer t3 x3;
       writer t4 x4;
       writer t5 x5;
       writer t6 x6;
       writer t7 x7)
let ob_tuple8 t1 t2 t3 t4 t5 t6 t7 t8 =
  from_wire (annot t1 @@ annot t2 @@ annot t3 @@ annot t4 @@ annot t5 @@ annot t6 @@ annot t7 @@ annot t8)
    (perform
       x1 <-- reader t1;
       x2 <-- reader t2;
       x3 <-- reader t3;
       x4 <-- reader t4;
       x5 <-- reader t5;
       x6 <-- reader t6;
       x7 <-- reader t7;
       x8 <-- reader t8;
       return (x1, x2))
    (fun (x1, x2, x3, x4, x5, x6, x7, x8) ->
       writer t1 x1;
       writer t2 x2;
       writer t3 x3;
       writer t4 x4;
       writer t5 x5;
       writer t6 x6;
       writer t7 x7;
       writer t8 x8)
let ob_tuple9 t1 t2 t3 t4 t5 t6 t7 t8 t9 =
  from_wire (annot t1 @@ annot t2 @@ annot t3 @@ annot t4 @@ annot t5 @@ annot t6 @@ annot t7 @@ annot t8 @@ annot t9)
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
       return (x1, x2))
    (fun (x1, x2, x3, x4, x5, x6, x7, x8, x9) ->
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
  from_wire (annot t1 @@ annot t2 @@ annot t3 @@ annot t4 @@ annot t5 @@ annot t6 @@ annot t7 @@ annot t8 @@ annot t9 @@ annot t10)
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
       return (x1, x2))
    (fun (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) ->
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
