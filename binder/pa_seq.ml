(*
 * pa_seq.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Camlp4.PreCast
open Syntax

EXTEND Gram
  expr: LEVEL "simple"
   [ [ "[<"; seq = LIST0 expr LEVEL "simple" SEP ";"; ">]" ->
	 List.fold_right
	   (fun c acc -> <:expr< Seq.cons $c$ $acc$ >>)
	   seq (<:expr< Seq.nil >>)
     ] ]
  ;
END
