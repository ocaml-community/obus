(*
 * oBus_type.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_internals
open Wire
open OBus_value

(***** Sequence type as tree *****)

(* Type description involve a lot of small concatenation, especially
   in functionnal types, and most of the time we will get the type as
   a list only one time (in OBus_connection.send_...), so to avoid
   multiple list concatenation we use this intermediate
   representation: *)

type 'a tree =
  | Tcons of 'a tree * 'a tree
  | Tone of 'a
  | Tnil

(* Compute the boundary of a tree *)
let tree_get_boundary t =
  let rec aux acc = function
    | Tone t -> t :: acc
    | Tcons(x, y) -> aux (aux acc y) x
    | Tnil -> acc
  in
  aux [] t

(***** Type descriptions *****)

type ('a, 'typ, 'make, 'cast) ty_desc = {
  (* The dbus type *)
  dtype : 'typ;

  (* Value functions *)
  make : 'make;
  cast : 'cast;

  (* Wire functions *)
  reader : 'a reader;
  writer : 'a -> writer;
}

type 'a ty_desc_basic = ('a, tbasic, 'a -> basic, basic -> 'a) ty_desc
type 'a ty_desc_single =  ('a, tsingle, 'a -> single, single -> 'a) ty_desc
type 'a ty_desc_sequence = ('a, tsingle tree, 'a -> single tree, sequence -> 'a * sequence) ty_desc

type 'a ty_basic = [ `basic of 'a ty_desc_basic ]
type 'a ty_single = [ 'a ty_basic | `single of 'a ty_desc_single ]
type 'a ty_sequence = [ 'a ty_single | `sequence of 'a ty_desc_sequence ]

type ('a, 'b, 'c) ty_function = {
  isignature : tsingle tree;
  osignature : signature;

  (* If used to send a message.

     It take as argument an accumulator, which is a writer, and a
     continuation. [send] must construct a writer from the next
     argument and pass it to the continuation. *)
  send : writer -> (writer -> 'b) -> 'a;

  (* If used to receive a message.

     [recv] is a reader which must construct a closure containing the
     value read from the input, which when applied on a function give
     these values to the function.

     i.e., suppose the combinator is of type [int -> string -> 'a],
     and we have a message containing: [1; "toto"], then when run on
     this message [recv] must return:

     [fun f -> f 1 "toto"] *)
  recv : ('a -> 'b) reader;

  reply_reader : 'c reader;
  reply_writer : 'c -> writer;
}

let type_basic (`basic { dtype = t }) = t
let type_single = function
  | `basic { dtype = t } -> Tbasic t
  | `single { dtype = t } -> t
let type_sequence = function
  | `basic { dtype = t } -> [Tbasic t]
  | `single { dtype = t } -> [t]
  | `sequence { dtype = t } -> tree_get_boundary t

let isignature { isignature = s } = tree_get_boundary s
let osignature { osignature = s } = s

let make_basic (`basic { make = f }) = f
let make_single = function
  | `basic { make = f } -> (fun x -> vbasic(f x))
  | `single { make = f } -> f
let make_sequence = function
  | `basic { make = f } -> (fun x -> [vbasic(f x)])
  | `single { make = f } -> (fun x -> [f x])
  | `sequence { make = f } -> (fun x -> tree_get_boundary (f x))

exception Cast_failure

let cast_basic (`basic { cast = f }) = f
let cast_single = function
  | `basic { cast = f } ->
      (function
         | Basic x -> f x
         | _ -> raise Cast_failure)
  | `single { cast = f } -> f
let cast_sequence = function
  | `basic { cast = f } ->
      (function
         | [Basic x] -> f x
         | _ -> raise Cast_failure)
  | `single { cast = f } ->
      (function
         | [x] -> f x
         | _ -> raise Cast_failure)
  | `sequence { cast = f } ->
      (fun l -> match f l with
         | v, [] -> v
         | _ -> raise Cast_failure)

let opt_cast f x = try Some(f x) with Cast_failure -> None
let opt_cast_basic t = opt_cast (cast_basic t)
let opt_cast_single t = opt_cast (cast_single t)
let opt_cast_sequence t = opt_cast (cast_sequence t)

let ty_function_send { send = f } = f (fun ctx ptr -> ptr)
let ty_function_recv { recv = f } = f
let ty_function_reply_writer { reply_writer = f } = f
let ty_function_reply_reader { reply_reader = f } = f
let ty_writer = function
  | `basic { writer = f } -> f
  | `single { writer = f } -> f
  | `sequence { writer = f } -> f
let ty_reader = function
  | `basic { reader = f } -> f
  | `single { reader = f } -> f
  | `sequence { reader = f } -> f

let wrap t f g = {
  dtype = t.dtype;
  make = (fun x -> t.make (g x));
  cast = (fun x -> f (t.cast x));
  writer = (fun x -> t.writer (g x));
  reader = (fun ctx i -> let i, v = t.reader ctx i in (i, f v));
}

let wrap_basic (`basic t) f g = `basic (wrap t f g)
let wrap_single ty f g = match ty with
  | `basic t -> `basic (wrap t f g)
  | `single t -> `single (wrap t f g)
let wrap_sequence ty f g = match ty with
  | `basic t -> `basic (wrap t f g)
  | `single t -> `single (wrap t f g)
  | `sequence t ->`sequence {
      dtype = t.dtype;
      make = (fun x -> t.make (g x));
      cast = (fun l -> let v, rest = t.cast l in (f v, rest));
      writer = (fun x -> t.writer (g x));
      reader = (fun ctx i -> let i, v = t.reader ctx i in (i, f v));
    }

(***** Predefined types *****)

let tbyte = `basic
  { dtype = Tbyte;
    make = vbyte;
    cast = (function
              | Byte x -> x
              | _ -> raise Cast_failure);
    reader = rbyte;
    writer = wbyte }

let tchar = tbyte

let tint8 = wrap_basic tbyte
  (fun x -> let x = int_of_char x in
   if x >= 128 then x - 256 else x)
  (fun x -> Char.unsafe_chr & x land 0xff)

let tuint8 = wrap_basic tbyte
  int_of_char
  (fun x -> Char.unsafe_chr & x land 0xff)

let tboolean = `basic
  { dtype = Tboolean;
    make = vboolean;
    cast = (function
              | Boolean x -> x
              | _ -> raise Cast_failure);
    reader = rboolean;
    writer = wboolean }

let tbool = tboolean

let tint16 = `basic
  { dtype = Tint16;
    make = vint16;
    cast = (function
              | Int16 x -> x
              | _ -> raise Cast_failure);
    reader = rint16;
    writer = wint16 }

let tint32 = `basic
  { dtype = Tint32;
    make = vint32;
    cast = (function
              | Int32 x -> x
              | _ -> raise Cast_failure);
    reader = rint32;
    writer = wint32 }

let tint = `basic
  { dtype = Tint32;
    make = (fun x -> Int32(Int32.of_int x));
    cast = (function
              | Int32 x -> Int32.to_int x
              | _ -> raise Cast_failure);
    reader = rint;
    writer = wint }

let tint64 = `basic
  { dtype = Tint64;
    make = vint64;
    cast = (function
              | Int64 x -> x
              | _ -> raise Cast_failure);
    reader = rint64;
    writer = wint64 }

let tuint16 = `basic
  { dtype = Tuint16;
    make = vuint16;
    cast = (function
              | Uint16 x -> x
              | _ -> raise Cast_failure);
    reader = ruint16;
    writer = wuint16 }

let tuint32 = `basic
  { dtype = Tuint32;
    make = vuint32;
    cast = (function
              | Uint32 x -> x
              | _ -> raise Cast_failure);
    reader = ruint32;
    writer = wuint32 }

let tuint = `basic
  { dtype = Tuint32;
    make = (fun x -> Uint32(Int32.of_int x));
    cast = (function
              | Uint32 x -> Int32.to_int x
              | _ -> raise Cast_failure);
    reader = ruint;
    writer = wuint }

let tuint64 = `basic
  { dtype = Tuint64;
    make = vuint64;
    cast = (function
              | Uint64 x -> x
              | _ -> raise Cast_failure);
    reader = ruint64;
    writer = wuint64 }

let tdouble = `basic
  { dtype = Tdouble;
    make = vdouble;
    cast = (function
              | Double x -> x
              | _ -> raise Cast_failure);
    reader = rdouble;
    writer = wdouble }

let tfloat = tdouble

let tstring = `basic
  { dtype = Tstring;
    make = vstring;
    cast = (function
              | String x -> x
              | _ -> raise Cast_failure);
    reader = rstring;
    writer = wstring }

let tsignature = `basic
  { dtype = Tsignature;
    make = vsignature;
    cast = (function
              | Signature x -> x
              | _ -> raise Cast_failure);
    reader = rsignature;
    writer = wsignature }

let tobject_path = `basic
  { dtype = Tobject_path;
    make = vobject_path;
    cast = (function
              | Object_path x -> x
              | _ -> raise Cast_failure);
    reader = robject_path;
    writer = wobject_path }

let tpath = tobject_path

let tproxy = `basic
  { dtype = Tobject_path;
    make = (fun x -> Object_path x.proxy_path);
    cast = (function
              | Object_path x -> failwith "tproxy can not be used here"
              | _ -> raise Cast_failure);
    reader = (fun ctx i ->
                let i, path = robject_path ctx i in
                i, { proxy_connection = ctx.connection;
                     proxy_service = ctx.bus_name;
                     proxy_path = path });
    writer = (fun x -> wobject_path x.proxy_path) }

let tlist ty =
  let typ = type_single ty in
  `single
    { dtype = Tarray typ;
      make = (fun l ->
                let f = make_single ty in
                varray typ (List.map f l));
      cast = (fun l ->
                let f = cast_single ty in
                match l with
                  | Array(t, l) when t = typ -> List.map f l
                  | _ -> raise Cast_failure);
      reader = (fun ctx i -> rlist typ (ty_reader ty) ctx i);
      writer = (fun x -> wlist typ (ty_writer ty) x) }

let tset ty =
  let typ = type_single ty in
  `single
    { dtype = Tarray typ;
      make = (fun l ->
                let f = make_single ty in
                varray typ (List.map f l));
      cast = (fun l ->
                let f = cast_single ty in
                match l with
                  | Array(t, l) when t = typ -> List.map f l
                  | _ -> raise Cast_failure);
      reader = (fun ctx i -> rset typ (ty_reader ty) ctx i);
      writer = (fun x -> wlist typ (ty_writer ty) x) }

let list_of_string str =
  let rec aux i acc =
    if i = 0
    then acc
    else aux (i - 1) (vbasic (Byte str.[i]) :: acc)
  in
  aux (String.length str) []

let string_of_list l =
  let str = String.create (List.length l) in
  let rec aux i = function
    | [] -> str
    | (Basic (Byte x)) :: l ->
        str.[i] <- x;
        aux (i + 1) l
    | _ -> raise Cast_failure
  in
  aux 0 l

let tbyte_array = `single
  { dtype = Tarray (Tbasic Tbyte);
    make = (fun s -> varray (Tbasic Tbyte) (list_of_string s));
    cast =(function
             | Array(Tarray (Tbasic Tbyte), l) -> string_of_list l
             | _ -> raise Cast_failure);
    reader = rbyte_array;
    writer = wbyte_array }

let tassoc tyk tyv =
  let ktyp = type_basic tyk
  and vtyp = type_single tyv in
  `single
    { dtype = Tdict(ktyp, vtyp);
      make = (fun l ->
                let f = make_basic tyk
                and g = make_single tyv in
                vdict ktyp vtyp (List.map (fun (k, v) -> (f k, g v)) l));
      cast = (fun l ->
                let f = cast_basic tyk
                and g = cast_single tyv in
                match l with
                  | Dict(tk, tv, l) when tk = ktyp && tv = vtyp ->
                      List.map (fun (k, v) -> (f k, g v)) l
                  | _ -> raise Cast_failure);
      reader = (fun ctx i -> rassoc (ty_reader tyk) (ty_reader tyv) ctx i);
      writer = (fun x -> wassoc (ty_writer tyk) (ty_writer tyv) x) }

let tstructure ty = `single
  { dtype = Tstruct(type_sequence ty);
    make = (fun x -> vstruct (make_sequence ty x));
    cast = (function
              | Struct l -> cast_sequence ty l
              | _ -> raise Cast_failure);
    reader = (fun ctx i -> rstruct (ty_reader ty) ctx i);
    writer = (fun x -> wstruct (ty_writer ty x)) }

let tvariant = `single
  { dtype = Tvariant;
    make = vvariant;
    cast = (function
              | Variant v -> v
              | _ -> raise Cast_failure);
    reader = rvariant;
    writer = wvariant }

let tunit = `sequence
  { dtype = Tnil;
    make = (fun () -> Tnil);
    cast = (fun l -> ((), l));
    reader = (fun ctx i -> (i, ()));
    writer = (fun () ctx i -> i) }

let (@) a b = Tcons(a, b)
let typ = function
  | `basic { dtype = t } -> Tone (Tbasic t)
  | `single { dtype = t } -> Tone t
  | `sequence { dtype = t } -> t
let make = function
  | `basic { make = f } -> (fun v -> Tone (vbasic(f v)))
  | `single { make = f } -> (fun v -> Tone (f v))
  | `sequence { make = f } -> f
let cast = function
  | `basic { cast = f } ->
      (function
         | Basic x :: l -> f x, l
         | _ -> raise Cast_failure)
  | `single { cast = f } ->
      (function
         |  x :: l -> f x, l
         | _ -> raise Cast_failure)
  | `sequence { cast = f } -> f

let reply ty = {
  isignature = Tnil;
  osignature = type_sequence ty;
  send = (fun acc cont -> cont acc);
  recv = (fun ctx i -> (i, fun f -> f));
  reply_reader = ty_reader ty;
  reply_writer = ty_writer ty;
}

let abstract ty fty =
  { fty with
      isignature = typ ty @ fty.isignature;
      send = (fun acc cont x -> fty.send (fun ctx i -> ty_writer ty x ctx (acc ctx i)) cont);
      recv = (fun ctx i ->
                let ptr, x = ty_reader ty ctx i in
                let ptr, f = fty.recv ctx i in
                (ptr, fun g -> f (g x))) }

let (-->) = abstract

let tpair ty1 ty2 = `sequence
  { dtype = typ ty1 @ typ ty2;
    make = (fun (x1, x2) ->
              make ty1 x1
              @ make ty2 x2);
    cast = (fun l ->
              let v1, l = cast ty1 l in
              let v2, l = cast ty2 l in
              ((v1, v2), l));
    writer = (fun (x1, x2) ctx i ->
                let i = ty_writer ty1 x1 ctx i in
                let i = ty_writer ty2 x2 ctx i in
                i);
    reader = (fun ctx i ->
                let i, v1 = ty_reader ty1 ctx i in
                let i, v2 = ty_reader ty2 ctx i in
                (i, (v1, v2))) }

let tup2 = tpair

let tup3 ty1 ty2 ty3 = `sequence
  { dtype = typ ty1 @ typ ty2 @ typ ty3;
    make = (fun (x1, x2, x3) ->
              make ty1 x1
              @ make ty2 x2
              @ make ty3 x3);
    cast = (fun l ->
              let v1, l = cast ty1 l in
              let v2, l = cast ty2 l in
              let v3, l = cast ty3 l in
              ((v1, v2, v3), l));
    writer = (fun (x1, x2, x3) ctx i ->
                let i = ty_writer ty1 x1 ctx i in
                let i = ty_writer ty2 x2 ctx i in
                let i = ty_writer ty3 x3 ctx i in
                i);
    reader = (fun ctx i ->
                let i, v1 = ty_reader ty1 ctx i in
                let i, v2 = ty_reader ty2 ctx i in
                let i, v3 = ty_reader ty3 ctx i in
                (i, (v1, v2, v3))) }

let tup4 ty1 ty2 ty3 ty4 = `sequence
  { dtype = typ ty1 @ typ ty2 @ typ ty3 @ typ ty4;
    make = (fun (x1, x2, x3, x4) ->
              make ty1 x1
              @ make ty2 x2
              @ make ty3 x3
              @ make ty4 x4);
    cast = (fun l ->
              let v1, l = cast ty1 l in
              let v2, l = cast ty2 l in
              let v3, l = cast ty3 l in
              let v4, l = cast ty4 l in
              ((v1, v2, v3, v4), l));
    writer = (fun (x1, x2, x3, x4) ctx i ->
                let i = ty_writer ty1 x1 ctx i in
                let i = ty_writer ty2 x2 ctx i in
                let i = ty_writer ty3 x3 ctx i in
                let i = ty_writer ty4 x4 ctx i in
                i);
    reader = (fun ctx i ->
                let i, v1 = ty_reader ty1 ctx i in
                let i, v2 = ty_reader ty2 ctx i in
                let i, v3 = ty_reader ty3 ctx i in
                let i, v4 = ty_reader ty4 ctx i in
                (i, (v1, v2, v3, v4))) }

let tup5 ty1 ty2 ty3 ty4 ty5 = `sequence
  { dtype = typ ty1 @ typ ty2 @ typ ty3 @ typ ty4 @ typ ty5;
    make = (fun (x1, x2, x3, x4, x5) ->
              make ty1 x1
              @ make ty2 x2
              @ make ty3 x3
              @ make ty4 x4
              @ make ty5 x5);
    cast = (fun l ->
              let v1, l = cast ty1 l in
              let v2, l = cast ty2 l in
              let v3, l = cast ty3 l in
              let v4, l = cast ty4 l in
              let v5, l = cast ty5 l in
              ((v1, v2, v3, v4, v5), l));
    writer = (fun (x1, x2, x3, x4, x5) ctx i ->
                let i = ty_writer ty1 x1 ctx i in
                let i = ty_writer ty2 x2 ctx i in
                let i = ty_writer ty3 x3 ctx i in
                let i = ty_writer ty4 x4 ctx i in
                let i = ty_writer ty5 x5 ctx i in
                i);
    reader = (fun ctx i ->
                let i, v1 = ty_reader ty1 ctx i in
                let i, v2 = ty_reader ty2 ctx i in
                let i, v3 = ty_reader ty3 ctx i in
                let i, v4 = ty_reader ty4 ctx i in
                let i, v5 = ty_reader ty5 ctx i in
                (i, (v1, v2, v3, v4, v5))) }

let tup6 ty1 ty2 ty3 ty4 ty5 ty6 = `sequence
  { dtype = typ ty1 @ typ ty2 @ typ ty3 @ typ ty4 @ typ ty5 @ typ ty6;
    make = (fun (x1, x2, x3, x4, x5, x6) ->
              make ty1 x1
              @ make ty2 x2
              @ make ty3 x3
              @ make ty4 x4
              @ make ty5 x5
              @ make ty6 x6);
    cast = (fun l ->
              let v1, l = cast ty1 l in
              let v2, l = cast ty2 l in
              let v3, l = cast ty3 l in
              let v4, l = cast ty4 l in
              let v5, l = cast ty5 l in
              let v6, l = cast ty6 l in
              ((v1, v2, v3, v4, v5, v6), l));
    writer = (fun (x1, x2, x3, x4, x5, x6) ctx i ->
                let i = ty_writer ty1 x1 ctx i in
                let i = ty_writer ty2 x2 ctx i in
                let i = ty_writer ty3 x3 ctx i in
                let i = ty_writer ty4 x4 ctx i in
                let i = ty_writer ty5 x5 ctx i in
                let i = ty_writer ty6 x6 ctx i in
                i);
    reader = (fun ctx i ->
                let i, v1 = ty_reader ty1 ctx i in
                let i, v2 = ty_reader ty2 ctx i in
                let i, v3 = ty_reader ty3 ctx i in
                let i, v4 = ty_reader ty4 ctx i in
                let i, v5 = ty_reader ty5 ctx i in
                let i, v6 = ty_reader ty6 ctx i in
                (i, (v1, v2, v3, v4, v5, v6))) }

let tup7 ty1 ty2 ty3 ty4 ty5 ty6 ty7 = `sequence
  { dtype = typ ty1 @ typ ty2 @ typ ty3 @ typ ty4 @ typ ty5 @ typ ty6 @ typ ty7;
    make = (fun (x1, x2, x3, x4, x5, x6, x7) ->
              make ty1 x1
              @ make ty2 x2
              @ make ty3 x3
              @ make ty4 x4
              @ make ty5 x5
              @ make ty6 x6
              @ make ty7 x7);
    cast = (fun l ->
              let v1, l = cast ty1 l in
              let v2, l = cast ty2 l in
              let v3, l = cast ty3 l in
              let v4, l = cast ty4 l in
              let v5, l = cast ty5 l in
              let v6, l = cast ty6 l in
              let v7, l = cast ty7 l in
              ((v1, v2, v3, v4, v5, v6, v7), l));
    writer = (fun (x1, x2, x3, x4, x5, x6, x7) ctx i ->
                let i = ty_writer ty1 x1 ctx i in
                let i = ty_writer ty2 x2 ctx i in
                let i = ty_writer ty3 x3 ctx i in
                let i = ty_writer ty4 x4 ctx i in
                let i = ty_writer ty5 x5 ctx i in
                let i = ty_writer ty6 x6 ctx i in
                let i = ty_writer ty7 x7 ctx i in
                i);
    reader = (fun ctx i ->
                let i, v1 = ty_reader ty1 ctx i in
                let i, v2 = ty_reader ty2 ctx i in
                let i, v3 = ty_reader ty3 ctx i in
                let i, v4 = ty_reader ty4 ctx i in
                let i, v5 = ty_reader ty5 ctx i in
                let i, v6 = ty_reader ty6 ctx i in
                let i, v7 = ty_reader ty7 ctx i in
                (i, (v1, v2, v3, v4, v5, v6, v7))) }

let tup8 ty1 ty2 ty3 ty4 ty5 ty6 ty7 ty8 = `sequence
  { dtype = typ ty1 @ typ ty2 @ typ ty3 @ typ ty4 @ typ ty5 @ typ ty6 @ typ ty7 @ typ ty8;
    make = (fun (x1, x2, x3, x4, x5, x6, x7, x8) ->
              make ty1 x1
              @ make ty2 x2
              @ make ty3 x3
              @ make ty4 x4
              @ make ty5 x5
              @ make ty6 x6
              @ make ty7 x7
              @ make ty8 x8);
    cast = (fun l ->
              let v1, l = cast ty1 l in
              let v2, l = cast ty2 l in
              let v3, l = cast ty3 l in
              let v4, l = cast ty4 l in
              let v5, l = cast ty5 l in
              let v6, l = cast ty6 l in
              let v7, l = cast ty7 l in
              let v8, l = cast ty8 l in
              ((v1, v2, v3, v4, v5, v6, v7, v8), l));
    writer = (fun (x1, x2, x3, x4, x5, x6, x7, x8) ctx i ->
                let i = ty_writer ty1 x1 ctx i in
                let i = ty_writer ty2 x2 ctx i in
                let i = ty_writer ty3 x3 ctx i in
                let i = ty_writer ty4 x4 ctx i in
                let i = ty_writer ty5 x5 ctx i in
                let i = ty_writer ty6 x6 ctx i in
                let i = ty_writer ty7 x7 ctx i in
                let i = ty_writer ty8 x8 ctx i in
                i);
    reader = (fun ctx i ->
                let i, v1 = ty_reader ty1 ctx i in
                let i, v2 = ty_reader ty2 ctx i in
                let i, v3 = ty_reader ty3 ctx i in
                let i, v4 = ty_reader ty4 ctx i in
                let i, v5 = ty_reader ty5 ctx i in
                let i, v6 = ty_reader ty6 ctx i in
                let i, v7 = ty_reader ty7 ctx i in
                let i, v8 = ty_reader ty8 ctx i in
                (i, (v1, v2, v3, v4, v5, v6, v7, v8))) }

let tup9 ty1 ty2 ty3 ty4 ty5 ty6 ty7 ty8 ty9 = `sequence
  { dtype = typ ty1 @ typ ty2 @ typ ty3 @ typ ty4 @ typ ty5 @ typ ty6 @ typ ty7 @ typ ty8 @ typ ty9;
    make = (fun (x1, x2, x3, x4, x5, x6, x7, x8, x9) ->
              make ty1 x1
              @ make ty2 x2
              @ make ty3 x3
              @ make ty4 x4
              @ make ty5 x5
              @ make ty6 x6
              @ make ty7 x7
              @ make ty8 x8
              @ make ty9 x9);
    cast = (fun l ->
              let v1, l = cast ty1 l in
              let v2, l = cast ty2 l in
              let v3, l = cast ty3 l in
              let v4, l = cast ty4 l in
              let v5, l = cast ty5 l in
              let v6, l = cast ty6 l in
              let v7, l = cast ty7 l in
              let v8, l = cast ty8 l in
              let v9, l = cast ty9 l in
              ((v1, v2, v3, v4, v5, v6, v7, v8, v9), l));
    writer = (fun (x1, x2, x3, x4, x5, x6, x7, x8, x9) ctx i ->
                let i = ty_writer ty1 x1 ctx i in
                let i = ty_writer ty2 x2 ctx i in
                let i = ty_writer ty3 x3 ctx i in
                let i = ty_writer ty4 x4 ctx i in
                let i = ty_writer ty5 x5 ctx i in
                let i = ty_writer ty6 x6 ctx i in
                let i = ty_writer ty7 x7 ctx i in
                let i = ty_writer ty8 x8 ctx i in
                let i = ty_writer ty9 x9 ctx i in
                i);
    reader = (fun ctx i ->
                let i, v1 = ty_reader ty1 ctx i in
                let i, v2 = ty_reader ty2 ctx i in
                let i, v3 = ty_reader ty3 ctx i in
                let i, v4 = ty_reader ty4 ctx i in
                let i, v5 = ty_reader ty5 ctx i in
                let i, v6 = ty_reader ty6 ctx i in
                let i, v7 = ty_reader ty7 ctx i in
                let i, v8 = ty_reader ty8 ctx i in
                let i, v9 = ty_reader ty9 ctx i in
                (i, (v1, v2, v3, v4, v5, v6, v7, v8, v9))) }

let tup10 ty1 ty2 ty3 ty4 ty5 ty6 ty7 ty8 ty9 ty10 = `sequence
  { dtype = typ ty1 @ typ ty2 @ typ ty3 @ typ ty4 @ typ ty5 @ typ ty6 @ typ ty7 @ typ ty8 @ typ ty9 @ typ ty10;
    make = (fun (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) ->
              make ty1 x1
              @ make ty2 x2
              @ make ty3 x3
              @ make ty4 x4
              @ make ty5 x5
              @ make ty6 x6
              @ make ty7 x7
              @ make ty8 x8
              @ make ty9 x9
              @ make ty10 x10);
    cast = (fun l ->
              let v1, l = cast ty1 l in
              let v2, l = cast ty2 l in
              let v3, l = cast ty3 l in
              let v4, l = cast ty4 l in
              let v5, l = cast ty5 l in
              let v6, l = cast ty6 l in
              let v7, l = cast ty7 l in
              let v8, l = cast ty8 l in
              let v9, l = cast ty9 l in
              let v10, l = cast ty10 l in
              ((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10), l));
    writer = (fun (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) ctx i ->
                let i = ty_writer ty1 x1 ctx i in
                let i = ty_writer ty2 x2 ctx i in
                let i = ty_writer ty3 x3 ctx i in
                let i = ty_writer ty4 x4 ctx i in
                let i = ty_writer ty5 x5 ctx i in
                let i = ty_writer ty6 x6 ctx i in
                let i = ty_writer ty7 x7 ctx i in
                let i = ty_writer ty8 x8 ctx i in
                let i = ty_writer ty9 x9 ctx i in
                let i = ty_writer ty10 x10 ctx i in
                i);
    reader = (fun ctx i ->
                let i, v1 = ty_reader ty1 ctx i in
                let i, v2 = ty_reader ty2 ctx i in
                let i, v3 = ty_reader ty3 ctx i in
                let i, v4 = ty_reader ty4 ctx i in
                let i, v5 = ty_reader ty5 ctx i in
                let i, v6 = ty_reader ty6 ctx i in
                let i, v7 = ty_reader ty7 ctx i in
                let i, v8 = ty_reader ty8 ctx i in
                let i, v9 = ty_reader ty9 ctx i in
                let i, v10 = ty_reader ty10 ctx i in
                (i, (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10))) }

type 'a with_basic_ty = { with_basic_ty : 'b. 'b ty_basic -> 'a }
type 'a with_single_ty = { with_single_ty : 'b. 'b ty_single -> 'a }
type 'a with_sequence_ty = { with_sequence_ty : 'b. 'b ty_sequence -> 'a }

let with_basic_ty w = function
  | Tbyte -> w.with_basic_ty tbyte
  | Tboolean -> w.with_basic_ty tboolean
  | Tint16 -> w.with_basic_ty tint16
  | Tint32 -> w.with_basic_ty tint32
  | Tint64 -> w.with_basic_ty tint64
  | Tuint16 -> w.with_basic_ty tuint16
  | Tuint32 -> w.with_basic_ty tuint32
  | Tuint64 -> w.with_basic_ty tuint64
  | Tdouble -> w.with_basic_ty tdouble
  | Tstring -> w.with_basic_ty tstring
  | Tsignature -> w.with_basic_ty tsignature
  | Tobject_path -> w.with_basic_ty tobject_path

let rec with_single_ty w = function
  | Tbasic t -> with_basic_ty { with_basic_ty = fun t -> w.with_single_ty (t :> _ ty_single) } t
  | Tarray t -> with_single_ty { with_single_ty = fun t -> w.with_single_ty (tlist t) } t
  | Tdict(tk, tv) -> with_basic_ty { with_basic_ty = fun tk ->
                                       with_single_ty { with_single_ty = fun tv -> w.with_single_ty (tassoc tk tv) } tv } tk
  | Tstruct tl -> with_sequence_ty { with_sequence_ty = fun t -> w.with_single_ty (tstructure t) } tl
  | Tvariant -> w.with_single_ty tvariant

and with_sequence_ty w = function
  | [] -> w.with_sequence_ty tunit
  | tx :: tl -> with_single_ty
      { with_single_ty = fun tx ->
          with_sequence_ty
            { with_sequence_ty = fun tl -> w.with_sequence_ty (tpair tx tl) } tl } tx
