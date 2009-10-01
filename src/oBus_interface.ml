(*
 * oBus_interface.ml
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt

module type S = sig
  type t
  val interface : OBus_name.interface
  val method_call : string -> ('a, 'b Lwt.t, 'b) OBus_type.func -> t -> 'a
  val signal : OBus_name.member -> ('a, _) OBus_type.cl_sequence -> t -> 'a OBus_signal.t
  val property_reader : OBus_name.member -> ('a, _) OBus_type.cl_single -> t -> 'a Lwt.t
  val property_writer : OBus_name.member -> ('a, _) OBus_type.cl_single -> t -> 'a -> unit Lwt.t
end

module type Name = sig
  val name : OBus_name.interface
end

module type Custom_proxy = sig
  type t
  val make_proxy : t -> OBus_proxy.t Lwt.t
end

module type Single_proxy = sig
  val proxy : OBus_proxy.t Lwt.t Lazy.t
end

module Make(Name : Name) =
struct
  type t = OBus_proxy.t
  let interface = Name.name
  let method_call member typ proxy = OBus_proxy.method_call ~interface ~member proxy typ
  let signal member typ proxy = OBus_signal.connect proxy ~interface ~member typ
  let property_reader member typ proxy = OBus_property.get proxy ~interface ~member typ
  let property_writer member typ proxy value = OBus_property.set proxy ~interface ~member typ value
end

module Make_custom(Proxy : Custom_proxy)(Name : Name) =
struct
  type t = Proxy.t
  let interface = Name.name
  let method_call member typ obj =
    OBus_type.make_func typ
      (fun body ->
         lwt proxy = Proxy.make_proxy obj in
         OBus_proxy.method_call' proxy ~interface ~member body (OBus_type.func_reply typ))
  let signal member typ obj =
    let event, push = React.E.create () and until_waiter, until_wakener = wait () in
    ignore_result (lwt proxy = Proxy.make_proxy obj in
                   OBus_private_signal.connect proxy ~interface ~member ~push ~until:until_waiter);
    let event = React.E.fmap (OBus_private_signal.cast Name.name member typ) event
    and disconnect = lazy(wakeup until_wakener ()) in
    (object
       method event = event
       method disconnect = Lazy.force disconnect
     end)
  let property_reader member typ obj =
    lwt proxy = Proxy.make_proxy obj in
    OBus_property.get proxy ~interface ~member typ
  let property_writer member typ obj value =
    lwt proxy = Proxy.make_proxy obj in
    OBus_property.set proxy ~interface ~member typ value
end

module Make_single(Proxy : Single_proxy)(Name : Name) =
struct
  let interface = Name.name
  let method_call member typ =
    OBus_type.make_func typ
      (fun body ->
         lwt proxy = Lazy.force Proxy.proxy in
         OBus_proxy.method_call' proxy ~interface ~member body (OBus_type.func_reply typ))
  let signal member typ () =
    let event, push = React.E.create () and until_waiter, until_wakener = wait () in
    ignore_result (lwt proxy = Lazy.force Proxy.proxy in
                   OBus_private_signal.connect proxy ~interface ~member ~push ~until:until_waiter);
    let event = React.E.fmap (OBus_private_signal.cast Name.name member typ) event
    and disconnect = lazy(wakeup until_wakener ()) in
    (object
       method event = event
       method disconnect = Lazy.force disconnect
     end)
  let property_reader member typ () =
    lwt proxy = Lazy.force Proxy.proxy in
    OBus_property.get proxy ~interface ~member typ
  let property_writer member typ value =
    lwt proxy = Lazy.force Proxy.proxy in
    OBus_property.set proxy ~interface ~member typ value
end
