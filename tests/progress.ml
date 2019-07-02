(*
 * progress.ml
 * -----------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt

type t = {
  mutable current_percent : int;
  mutable current : int;
  prefix : string;
  max : int;
}

let make prefix max =
  let%lwt () = Lwt_io.printf "%s: 0%%%!" prefix in
  return {
    prefix = prefix;
    max = max;
    current = 0;
    current_percent = 0;
  }

let incr p =
  p.current <- p.current + 1;
  let x = p.current * 100 / p.max in
  if x <> p.current_percent then begin
    p.current_percent <- x;
    let%lwt () = Lwt_io.printf "\r%s: %d%%" p.prefix x in
    Lwt_io.flush Lwt_io.stdout
  end else
    return ()

let close p =
  let%lwt () = Lwt_io.printf "\r%s: 100%%\n" p.prefix in
  Lwt_io.flush Lwt_io.stdout
