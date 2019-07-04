(*
 * uPower_wakeups.ml
 * -----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt

type data = {
  data_is_userspace : bool;
  data_id : int;
  data_value : float;
  data_cmdline : string option;
  data_details : string;
}

open UPower_interfaces.Org_freedesktop_UPower_Wakeups

let proxy daemon = OBus_proxy.make (UPower.to_peer daemon) ["org"; "freedesktop"; "UPower"; "Wakeups"]

let has_capability daemon =
  OBus_property.make p_HasCapability (proxy daemon)

let get_total daemon =
  let%lwt value = OBus_method.call m_GetTotal (proxy daemon) () in
  let value = Int32.to_int value in
  return value

let total_changed daemon =
  OBus_signal.map
    (fun value ->
       let value = Int32.to_int value in
       value)
    (OBus_signal.make s_TotalChanged (proxy daemon))

let get_data daemon =
  let%lwt data = OBus_method.call m_GetData (proxy daemon) () in
  return
    (List.map
       (fun (is_userspace, id, value, cmdline, details) -> {
          data_is_userspace = is_userspace;
          data_id = Int32.to_int id;
          data_value = value;
          data_cmdline = if cmdline = "" then None else Some cmdline;
          data_details = details;
        })
       data)

let data_changed daemon =
  OBus_signal.make s_DataChanged (proxy daemon)
