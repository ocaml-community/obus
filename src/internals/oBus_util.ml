(*
 * oBus_util.ml
 * ------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let section = Lwt_log.Section.make "obus(util)"

let rec assoc x = function
  | [] -> None
  | (k, v) :: _ when k = x -> Some(v)
  | _ :: l -> assoc x l

let rec assq x = function
  | [] -> None
  | (k, v) :: _ when k == x -> Some(v)
  | _ :: l -> assq x l

let rec find_map f = function
  | [] -> None
  | x :: l -> match f x with
      | None -> find_map f l
      | y -> y

let filter_map f l =
  List.fold_right (fun x acc -> match f x with
                    | None -> acc
                    | Some(v) -> v :: acc) l []

let part_map f l =
  List.fold_right (fun x (success, failure) -> match f x with
                     | None -> (success, x :: failure)
                     | Some(v) -> (v :: success, failure)) l ([], [])

type ('a, 'b) either =
  | InL of 'a
  | InR of 'b

let split f l =
  List.fold_right (fun x (a, b) -> match f x with
                     | InL x -> (x :: a, b)
                     | InR x -> (a, x :: b)) l ([], [])

let map_option x f = match x with
  | Some x -> Some(f x)
  | None -> None

let encode_char n =
  if n < 10 then
    char_of_int (n + Char.code '0')
  else if n < 16 then
    char_of_int (n + Char.code 'a' - 10)
  else
    assert false

let hex_encode str =
  let len = String.length str in
  let hex = Bytes.create (len * 2) in
  for i = 0 to len - 1 do
    let n = Char.code (String.unsafe_get str i) in
    Bytes.unsafe_set hex (i * 2) (encode_char (n lsr 4));
    Bytes.unsafe_set hex (i * 2 + 1) (encode_char (n land 15))
  done;
  Bytes.unsafe_to_string hex

let decode_char ch = match ch with
  | '0'..'9' -> Char.code ch - Char.code '0'
  | 'a'..'f' -> Char.code ch - Char.code 'a' + 10
  | 'A'..'F' -> Char.code ch - Char.code 'A' + 10
  | _ -> raise (Invalid_argument "OBus_util.decode_char")

let hex_decode hex =
  if String.length hex mod 2 <> 0 then raise (Invalid_argument "OBus_util.hex_decode");
  let len = String.length hex / 2 in
  let str = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.unsafe_set str i
      (char_of_int
         ((decode_char (String.unsafe_get hex (i * 2)) lsl 4) lor
            (decode_char (String.unsafe_get hex (i * 2 + 1)))))
  done;
  Bytes.unsafe_to_string str

let homedir = lazy(
  try
    Lwt.return (Sys.getenv "HOME")
  with Not_found ->
    let%lwt pwd = Lwt_unix.getpwuid (Unix.getuid ()) in
    Lwt.return pwd.Unix.pw_dir
)

let init_pseudo = Lazy.from_fun Random.self_init

let fill_pseudo buffer pos len =
  ignore (Lwt_log.warning ~section "using pseudo-random generator");
  Lazy.force init_pseudo;
  for i = pos to pos + len - 1 do
    Bytes.unsafe_set buffer i (char_of_int (Random.int 256))
  done

let fill_random buffer pos len =
  try
    let ic = open_in "/dev/urandom" in
    let n = input ic buffer pos len in
    if n < len then fill_pseudo buffer (pos + n) (len - n);
    close_in ic
  with exn ->
    ignore (Lwt_log.warning_f ~exn ~section "failed to get random data from /dev/urandom");
    fill_pseudo buffer pos len

let random_string n =
  let str = Bytes.create n in
  fill_random str 0 n;
  Bytes.unsafe_to_string str

let random_int32 () =
  let r = random_string 4 in
  Int32.logor
    (Int32.logor
       (Int32.of_int (Char.code r.[0]))
       (Int32.shift_left (Int32.of_int (Char.code r.[1])) 8))
    (Int32.logor
       (Int32.shift_left (Int32.of_int (Char.code r.[2])) 16)
       (Int32.shift_left (Int32.of_int (Char.code r.[3])) 24))

let random_int () = Int32.to_int (random_int32 ())

let random_int64 () =
  Int64.logor
    (Int64.of_int32 (random_int32 ()))
    (Int64.shift_left (Int64.of_int32 (random_int32 ())) 32)

(* Compute the sha1 of a string.

   Copied from uuidm by Daniel C. Bünzli, which can be found here:
   http://erratique.ch/software/uuidm *)
let sha_1 s =
  let sha_1_pad s =
    let len = String.length s in
    let blen = 8 * len in
    let rem = len mod 64 in
    let mlen = if rem > 55 then len + 128 - rem else len + 64 - rem in
    let m = Bytes.create mlen in
    Bytes.blit_string s 0 m 0 len;
    Bytes.fill m len (mlen - len) '\x00';
    Bytes.set m len '\x80';
    if Sys.word_size > 32 then begin
      Bytes.set m (mlen - 8) (Char.unsafe_chr (blen lsr 56 land 0xFF));
      Bytes.set m (mlen - 7) (Char.unsafe_chr (blen lsr 48 land 0xFF));
      Bytes.set m (mlen - 6) (Char.unsafe_chr (blen lsr 40 land 0xFF));
      Bytes.set m (mlen - 5) (Char.unsafe_chr (blen lsr 32 land 0xFF));
    end;
    Bytes.set m (mlen - 4) (Char.unsafe_chr (blen lsr 24 land 0xFF));
    Bytes.set m (mlen - 3) (Char.unsafe_chr (blen lsr 16 land 0xFF));
    Bytes.set m (mlen - 2) (Char.unsafe_chr (blen lsr 8 land 0xFF));
    Bytes.set m (mlen - 1) (Char.unsafe_chr (blen land 0xFF));
    Bytes.unsafe_to_string m
  in
  (* Operations on int32 *)
  let ( &&& ) = ( land ) in
  let ( lor ) = Int32.logor in
  let ( lxor ) = Int32.logxor in
  let ( land ) = Int32.logand in
  let ( ++ ) = Int32.add in
  let lnot = Int32.lognot in
  let sr = Int32.shift_right in
  let sl = Int32.shift_left in
  let cls n x = (sl x n) lor (Int32.shift_right_logical x (32 - n)) in
  (* Start *)
  let m = sha_1_pad s in
  let w = Array.make 16 0l in
  let h0 = ref 0x67452301l in
  let h1 = ref 0xEFCDAB89l in
  let h2 = ref 0x98BADCFEl in
  let h3 = ref 0x10325476l in
  let h4 = ref 0xC3D2E1F0l in
  let a = ref 0l in
  let b = ref 0l in
  let c = ref 0l in
  let d = ref 0l in
  let e = ref 0l in
  for i = 0 to ((String.length m) / 64) - 1 do             (* For each block *)
    (* Fill w *)
    let base = i * 64 in
    for j = 0 to 15 do
      let k = base + (j * 4) in
      w.(j) <- sl (Int32.of_int (Char.code m.[k])) 24 lor
        sl (Int32.of_int (Char.code m.[k + 1])) 16 lor
        sl (Int32.of_int (Char.code m.[k + 2])) 8 lor
        (Int32.of_int (Char.code m.[k + 3]))
    done;
    (* Loop *)
    a := !h0; b := !h1; c := !h2; d := !h3; e := !h4;
    for t = 0 to 79 do
      let f, k =
        if t <= 19 then (!b land !c) lor ((lnot !b) land !d), 0x5A827999l else
          if t <= 39 then !b lxor !c lxor !d, 0x6ED9EBA1l else
            if t <= 59 then
	      (!b land !c) lor (!b land !d) lor (!c land !d), 0x8F1BBCDCl
	    else
              !b lxor !c lxor !d, 0xCA62C1D6l
      in
      let s = t &&& 0xF in
      if (t >= 16) then begin
	w.(s) <- cls 1 begin
	  w.((s + 13) &&& 0xF) lxor
	    w.((s + 8) &&& 0xF) lxor
	    w.((s + 2) &&& 0xF) lxor
	    w.(s)
	end
      end;
      let temp = (cls 5 !a) ++ f ++ !e ++ w.(s) ++ k in
      e := !d;
      d := !c;
      c := cls 30 !b;
      b := !a;
      a := temp;
    done;
    (* Update *)
    h0 := !h0 ++ !a;
    h1 := !h1 ++ !b;
    h2 := !h2 ++ !c;
    h3 := !h3 ++ !d;
    h4 := !h4 ++ !e
  done;
  let h = Bytes.create 20 in
  let i2s h k i =
    Bytes.set h (k    ) (Char.unsafe_chr ((Int32.to_int (sr i 24)) &&& 0xFF));
    Bytes.set h (k + 1) (Char.unsafe_chr ((Int32.to_int (sr i 16)) &&& 0xFF));
    Bytes.set h (k + 2) (Char.unsafe_chr ((Int32.to_int (sr i 8)) &&& 0xFF));
    Bytes.set h (k + 3) (Char.unsafe_chr ((Int32.to_int i) &&& 0xFF));
  in
  i2s h 0 !h0;
  i2s h 4 !h1;
  i2s h 8 !h2;
  i2s h 12 !h3;
  i2s h 16 !h4;
  Bytes.unsafe_to_string h
