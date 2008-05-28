(*
 * log.ml
 * ------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

let split str =
  let rec aux pos =
    try
      let i = String.index_from str pos ',' in
        String.sub str pos (i - pos) :: aux (i + 1)
    with
        Not_found -> [String.sub str pos (String.length str - pos)]
  in
    aux 0

module type Section = sig
  val authentification : bool
  val transport : bool
  val connection : bool
end

module Make(What : sig val var : string end) =
struct
  let (authentification,
       transport,
       connection,
       serialization) =
  try
    let s = Sys.getenv What.var in
      if String.contains s '*'
      then (true, true, true, true)
      else let sections = split s in
        (List.mem "authentification" sections,
         List.mem "transport" sections,
         List.mem "connection" sections,
         List.mem "serialization" sections)
  with
      Not_found -> (false, false, false, false)
end

module Verbose = Make(struct let var = "OBUSLOG" end)
module Debug = Make(struct let var = "OBUSDEBUG" end)
