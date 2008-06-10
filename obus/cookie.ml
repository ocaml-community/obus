(*
 * cookie.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Connection
type 'a t = 'a cookie
let get = intern_cookie_get
let is_ready = intern_cookie_is_ready
let is_value = intern_cookie_is_value
let is_exn = intern_cookie_is_exn
let get_if_ready = intern_cookie_get_if_ready
