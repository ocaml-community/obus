(*
 * codeConstants.mli
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Constants expression contant for generated code *)

open Types

val fixed_reader : string -> string -> expr -> expr
  (** [fixed_reader caml_type dbus_type idx_expr] caml expression for
      reading a [dbus_type] marshaled value into a [caml_type]
      value. Only for fixed length basic types. *)

val fixed_writer : string -> string -> expr -> expr -> expr
  (** [fixed_writer caml_type dbus_type idx_expr expr] same as
      [fixed_reader] but for writing *)

val string_reader : expr -> expr -> expr -> expr
  (** [string_reader idx_expr str_expr len_expr] expression for
      blitting a string into [str_expr] *)

val string_writer : expr -> expr -> expr -> expr
  (** [string_writer idx_expr str_expr len_expr] expression for
      blitting a string from [str_expr] *)

val signature_checker : string -> expr -> expr
  (** [signature_checker idx_expr signature] expression which check
      that the readed signature correspond to this hardcorded one *)

val inconsistent_exn : expr
  (** Exception to raise in case of inconsistent message *)

val content_exn : expr
  (** Exception to raise if there is something wrong in the content of
      the message *)

val realloc_buffer : expr
  (** [realloc_buffer] expression for reallocating a bigger buffer and
      copying already marshaled content *)
