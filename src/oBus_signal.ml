(*
 * oBus_signal.ml
 * --------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt

class type ['a] t = object
  method event : 'a React.event
  method disconnect : unit
end

let dyn_connect proxy ~interface ~member =
  let event, push = React.E.create () and until_waiter, until_wakener = wait () in
  ignore_result (OBus_private_signal.connect proxy ~interface ~member ~push ~until:until_waiter);
  let event = React.E.map (fun (connection, message) -> OBus_message.body message) event
  and disconnect = lazy(wakeup until_wakener ()) in
  (object
     method event = event
     method disconnect = Lazy.force disconnect
   end)

let connect proxy ~interface ~member typ =
  let event, push = React.E.create () and until_waiter, until_wakener = wait () in
  ignore_result (OBus_private_signal.connect proxy ~interface ~member ~push ~until:until_waiter);
  let event = React.E.fmap (OBus_private_signal.cast interface member typ) event
  and disconnect = lazy(wakeup until_wakener ()) in
  (object
     method event = event
     method disconnect = Lazy.force disconnect
   end)
