(*
 * nm_access_point.mli
 * -------------------
 * Copyright : (c) 2010, Pierre Chambart <chambart@crans.org>
 *                 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Access point interface *)

include OBus_proxy.Private

(** {6 Signals} *)

val properties_changed : t -> (string * OBus_value.V.single) list OBus_signal.t

(** {6 Properties} *)

type ap_flag =
    [ `Privacy (** Access point supports privacy measures. *) ]

val flags : t -> ap_flag list OBus_property.r

type ap_security_flag =
    [ `Pair_wep40
        (** Access point supports pairwise 40-bit WEP encryption *)
    | `Pair_wep104
        (** Access point supports pairwise 104-bit WEP encryption *)
    | `Pair_tkip
        (** Access point supports pairwise TKIP encryption *)
    | `Pair_ccmp
        (** Access point supports pairwise CCMP encryption *)
    | `Group_wep40
        (** Access point supports a group 40-bit WEP cipher *)
    | `Group_wep104
        (** Access point supports a group 104-bit WEP cipher *)
    | `Group_tkip
        (** Access point supports a group TKIP cipher *)
    | `Group_ccmp
        (** Access point supports a group CCMP cipher *)
    | `Key_mgmt_psk
        (** Access point supports PSK key management *)
    | `Key_mgmt_802_1x
        (** Access point supports 802.1x key management *) ]

val wpa_flags : t -> ap_security_flag list OBus_property.r
val rsn_flags : t -> ap_security_flag list OBus_property.r

val ssid : t -> string OBus_property.r

val frequency : t -> int OBus_property.r
val hw_address : t -> string OBus_property.r
val mode : t -> int OBus_property.r
val max_bitrate : t -> int OBus_property.r
val strength : t -> int OBus_property.r

type properties = {
  flags : ap_flag list;
  wpa_flags : ap_security_flag list;
  rsn_flags : ap_security_flag list;
  ssid : string;
  frequency : int;
  hw_address : string;
  mode : int;
  max_bitrate : int;
  strength : int;
}

val properties : t -> properties OBus_property.r
