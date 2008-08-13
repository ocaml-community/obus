(*
 * notify.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open OBus_type
open OBus_value

include OBus_client.Make_constant
  (struct
     let name = "org.freedesktop.Notifications"
     let path = "/org/freedesktop/Notifications"
     let service = Some name
     let bus = OBus_bus.session
   end)

type id = int32
let tid = tuint32

type server_info = {
  server_name : string;
  server_vendor : string;
  server_version : string;
  server_spec_version : string;
}
type image = {
  img_width : int;
  img_height : int;
  img_rowstride : int;
  img_has_alpha: bool;
  img_bits_per_sample : int;
  img_channels : int;
  img_data : string;
}

type urgency =
    [ `low
    | `normal
    | `critical ]

type hint =
  | Hint_image of image
  | Hint_basic of OBus_value.basic
  | Hint_single of OBus_value.single

let thint = wrap_single tvariant
  (fun _ -> failwith "not implemented")
  (function
     | Hint_image img ->
         make_single (tstructure (tup7 tint tint tint tbool tint tint tbyte_array))
           (img.img_width,
            img.img_height,
            img.img_rowstride,
            img.img_has_alpha,
            img.img_bits_per_sample,
            img.img_channels,
            img.img_data)
     | Hint_basic value -> vvariant (vbasic value)
     | Hint_single value -> vvariant value)

let app_name = ref (Filename.basename Sys.argv.(0))
let desktop_entry = ref None

open Lwt

let get_server_information () =
  call "GetServerInformation" [: string * string * string * string ]
  >>= fun (name, vendor, version, spec_version) ->
    return { server_name = name;
             server_vendor = vendor;
             server_version = version;
             server_spec_version = spec_version }

let get_capabilities = call "GetCapabilities" [: unit -> string list ]
let close_notification = call "CloseNotification" [: id -> unit ]

let de = desktop_entry

let notify ?(app_name= !app_name) ?desktop_entry
    ?(replace=0l) ?(icon="") ?image ~summary ?(body="") ?(actions=[])
    ?urgency ?category ?sound_file ?suppress_sound ?pos ?(hints=[]) ?(timeout= -1) () =
  let desktop_entry = match desktop_entry with
    | None -> !de
    | x -> x in
  let mkhint v name f = match v with
    | Some x -> Some (name, f x)
    | None -> None in
  let mkbasic v name f = mkhint v name (fun x -> Hint_basic(f x)) in
  let mkstring v name = mkbasic v name vstring in
  let hints = Util.filter_map (fun x -> x)
    [mkstring desktop_entry "desktop_entry";
     mkhint image "image_data" (fun img -> Hint_image img);
     mkbasic urgency "urgency" (fun x -> make_basic tuint8
                                  (match x with
                                     | `low -> 0
                                     | `normal -> 1
                                     | `critical -> 2));
     mkstring category "category";
     mkstring sound_file "sound-file";
     mkbasic suppress_sound "suppress-sound" vboolean;
     mkbasic pos "x" (fun (x, y) -> make_basic tint x);
     mkbasic pos "y" (fun (x, y) -> make_basic tint y)]
    @ List.map (fun (name, value) -> (name, Hint_single value)) hints in
  call "Notify" [: string -> id -> string -> string -> string -> string list -> (string, hint) assoc -> int -> id ]
    app_name replace icon summary body (List.fold_right (fun (key, text) acc -> key :: text :: acc) actions []) hints timeout
