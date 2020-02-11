(*
 * policy_kit.mli
 * --------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** PolicyKit interface *)

val not_authorized : OBus_error.name
(** Exception raised by services when trying to perform an action
      for which we do not have authorization from PolicyKit *)

val obtain_authorization :
  action_id:string -> ?xid:int -> pid:int -> unit -> bool Lwt.t
(** [obtain_authorization ~action_id ~xid ~pid] tries to obtain
      authorization for [action_id]. It returns whether it succeed or not.

      @param action_id PolicyKit action identifier; see PolKitAction
      @param xid X11 window ID for the top-level X11 window the dialog
             will be transient for
      @param pid Process ID to grant authorization to
  *)
