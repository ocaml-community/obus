(*
 * caml_code.mli
 * -------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** OCaml code coloration *)

type color = string
  (** Type of a color. It is either a color name like "LightBlue", or
      a RGB color of the form ["#5fbf77"]. *)

exception Invalid_color of string
  (** Exception raised when an invalid color is used. *)

(** Type of a text style *)
type style = {
  color : color;
  bold : bool;
  emph : bool;
}

(** Type of colors for ocaml code: *)
type styles = {
  background : color;
  default : style;
  keyword : style;
  symbol : style;
  constructor : style;
  module_name : style;
  number : style;
  char : style;
  string : style;
  comment : style;
  variable : style;
}

val default_styles : styles
  (** Default styles (the one is use in emacs). *)

val bw_styles : styles
  (** Black and white colors with keywords in bold. *)

val input_file : ?styles : styles -> string -> Latex.t
  (** [input_file ?styles file_name] reads ocaml code from the given
      file and format it according to [styles], which defaults to
      {!default_stlyes}. *)
