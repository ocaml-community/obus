(*
 * util.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

let rec assoc x = function
  | [] -> None
  | (k, v) :: _ when k = x -> Some(v)
  | _ :: l -> assoc x l

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

let try_finally f close arg =
  let result =
    try
      f arg
    with
      | e ->
          close arg;
          raise e
  in
    close arg;
    result

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

let rec split f l =
  List.fold_right (fun x (a, b) -> match f x with
                     | Left x -> (x :: a, b)
                     | Right x -> (a, x :: b)) l ([], [])

let with_open_in fname f =
  try_finally f close_in (open_in fname)

let with_open_out fname f =
  try_finally f close_out (open_out fname)

let with_process openp closep cmd f =
  let c = openp cmd in
  let result =
    try
      f c
    with
        exn ->
          ignore (closep c);
          raise exn
  in
  match closep c with
    | Unix.WEXITED 0 -> result
    | Unix.WEXITED n -> failwith (Printf.sprintf "command %S exited with status %d" cmd n)
    | Unix.WSIGNALED n -> failwith (Printf.sprintf "command %S killed by signal %d" cmd n)
    | Unix.WSTOPPED n -> failwith (Printf.sprintf "command %S stopped by signal %d" cmd n)

let with_process_in cmd = with_process Unix.open_process_in Unix.close_process_in cmd
let with_process_out cmd = with_process Unix.open_process_out Unix.close_process_out cmd

module type Monad = sig
  type 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module Maybe =
struct
  type 'a t = 'a option
  let bind m f = match m with
    | Some v -> f v
    | None -> None
  let return v = Some v
  let failwith _ = None
  let wrap f m = bind m (fun x -> return (f x))
  let rec fold f l =
    List.fold_right (fun x acc ->
                       perform
                         x <-- f x;
                         l <-- acc;
                         return (x :: l)) l (return [])
end

module MaybeT(M : Monad) =
struct
  type 'a t = 'a option M.t
  let bind m f =
    M.bind m (function
                | Some v -> f v
                | None -> M.return None)
  let return v = M.return (Some v)
  let failwith _ = M.return None
  let wrap f m = bind m (fun x -> return (f x))
  let rec fold f l =
    List.fold_right (fun x acc ->
                       perform
                         x <-- f x;
                         l <-- acc;
                         return (x :: l)) l (return [])
end

let match_rule ?typ ?sender ?interface ?member ?path ?destination ?(args=[]) () =
  let buf = Buffer.create 42 in
  let first = ref true in
  let coma () =
    if !first
    then first := false
    else Buffer.add_char buf ',' in
  let add key value =
    coma ();
    Printf.bprintf buf "%s='%s'" key value in
  let add_opt key = function
    | None -> ()
    | Some x -> add key x in
    begin match typ with
      | None -> ()
      | Some t ->
          add "type"
            (match t with
               | `method_call -> "method_call"
               | `method_return -> "method_return"
               | `error -> "error"
               | `signal -> "signal")
    end;
    add_opt "sender" sender;
    add_opt "interface" interface;
    add_opt "member" member;
    add_opt "path" path;
    add_opt "destination" destination;
    List.iter (fun (n, value) -> coma (); Printf.bprintf buf "arg%d='%s'" n value) args;
    Buffer.contents buf
