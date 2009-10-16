(*
 * pa_log.ml
 * ---------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Logging facility.

    Replace things of the form:

    {[
      LOG("x = %d" x)
    ]}

    by:

    {[
      if !(OBus_log.verbose) then
        OBus_log.log ~section:"section" "x = %d" x
    ]}
*)

open Camlp4.PreCast

let section _loc =
  let fname = Filename.chop_extension (Loc.file_name _loc) in
  let i = String.index fname '_' in
  String.sub fname (i + 1) (String.length fname - i - 1)

let split expr =
  let rec aux acc = function
    | <:expr@_loc< $str:s$ >> ->
        (s, acc)
    | <:expr@_loc< $a$ $b$ >> ->
        aux (b :: acc) a
    | e ->
        Loc.raise (Ast.loc_of_expr e) (Failure "format string expected")
  in
  aux [] expr

let rec apply e = function
  | [] -> e
  | x :: l -> let _loc = Ast.loc_of_expr x in apply (<:expr< $e$ $x$ >>) l

let map =
object
  inherit Ast.map as super

  method expr st = match super#expr st with
    | <:expr@_loc< LOG($e$) >> ->
        let fmt, args = split e in
        <:expr<
          if !(OBus_log.verbose_enable) then
            $apply <:expr< OBus_log.log ~section:$str:section _loc$ $str:fmt$ >> args$
        >>
    | <:expr@_loc< DEBUG($e$) >> ->
        let fmt, args = split e in
        <:expr<
          if !(OBus_log.debug_enable) then
            $apply <:expr< OBus_log.debug ~section:$str:section _loc$ $str:fmt$ >> args$
        >>
    | <:expr@_loc< ERROR($e$) >> ->
        let fmt, args = split e in
        apply <:expr< OBus_log.error ~section:$str:section _loc$ $str:fmt$ >> args
    | <:expr@_loc< FAILURE($exn$, $e$) >> ->
        let fmt, args = split e in
        apply <:expr< OBus_log.failure ~section:$str:section _loc$ $exn$ $str:fmt$ >> args
    | expr ->
        expr
end

let () =
  AstFilters.register_str_item_filter map#str_item
