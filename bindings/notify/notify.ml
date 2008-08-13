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

let timage = tstructure (tup7 tint tint tint tbool tint tint tbyte_array)

type urgency =
    [ `low
    | `normal
    | `critical ]

type hint =
  | Hint_image of image
  | Hint_variant of string * single

let thint = wrap_element (tdict_entry tstring tvariant)
  (fun (name, value) ->
     match name with
       | "image_data" -> begin match opt_cast_single timage value with
           | None -> Hint_variant(name, value)
           | Some(img_width,
                  img_height,
                  img_rowstride,
                  img_has_alpha,
                  img_bits_per_sample,
                  img_channels,
                  img_data) ->
               Hint_image {
                 img_width = img_width;
                 img_height = img_height;
                 img_rowstride = img_rowstride;
                 img_has_alpha = img_has_alpha;
                 img_bits_per_sample = img_bits_per_sample;
                 img_channels = img_channels;
                 img_data = img_data;
               }
         end
       | _ -> Hint_variant(name, value))
  (function
     | Hint_image img ->
         ("image_data",
          make_single timage
            (img.img_width,
             img.img_height,
             img.img_rowstride,
             img.img_has_alpha,
             img.img_bits_per_sample,
             img.img_channels,
             img.img_data))
     | Hint_variant(name, value) -> (name, vvariant value))

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
  let mkhint v f = match v with
    | Some x -> Some (f x)
    | None -> None in
  let mkvariant v name f = mkhint v (fun x -> Hint_variant(name, f x)) in
  let mkstring v name = mkvariant v name (make_single tstring) in
  let hints = Util.filter_map (fun x -> x)
    [mkstring desktop_entry "desktop_entry";
     mkhint image (fun img -> Hint_image img);
     mkvariant urgency "urgency" (fun x -> make_single tuint8
                                    (match x with
                                       | `low -> 0
                                       | `normal -> 1
                                       | `critical -> 2));
     mkstring category "category";
     mkstring sound_file "sound-file";
     mkvariant suppress_sound "suppress-sound" (make_single tboolean);
     mkvariant pos "x" (fun (x, y) -> make_single tint x);
     mkvariant pos "y" (fun (x, y) -> make_single tint y)]
    @ List.map (fun (name, value) -> Hint_variant(name, value)) hints in
  call "Notify" [: string -> id -> string -> string -> string -> string list -> hint set -> int -> id ]
    app_name replace icon summary body (List.fold_right (fun (key, text) acc -> key :: text :: acc) actions []) hints timeout
