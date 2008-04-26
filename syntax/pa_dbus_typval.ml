(*
 * pa_dbus_typval.ml
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Camlp4.PreCast
open Syntax

EXTEND Gram
  expr: LEVEL "simple"
   [ [ "<+"; seq = LIST0 expr LEVEL "simple"; "+>" ->
	 List.fold_right
	   (fun c acc -> <:expr< OBus.Values.cons $c$ $acc$ >>)
	   seq (<:expr< OBus.Values.nil >>)
     ] ]
  ;

  expr: LEVEL "simple"
   [ [ "<-"; seq = LIST0 expr LEVEL "simple"; "->" ->
	 List.fold_right
	   (fun c acc -> <:expr< ( $c$ , $acc$ ) >>)
	   seq (<:expr< () >>)
     ] ]
  ;

  patt:
   [ [ "<-"; seq = LIST0 patt; "->" ->
	 List.fold_right
	   (fun c acc -> <:patt< ( $c$ , $acc$ ) >>)
	   seq (<:patt< () >>)
     ] ]
  ;
END
