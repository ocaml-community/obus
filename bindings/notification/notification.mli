(*
 * notification.mli
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(** Popup notifications *)

(** For complete details about notifications,
    @see <http://www.galago-project.org/specs/notification/> the official specifications *)

(** Server informations *)
type server_info = {
  server_name : string;
  server_vendor : string;
  server_version : string;
  server_spec_version : string;
} with obus(sequence)

val app_name : string ref
  (** Application name used for notification. The default value is
      taken from [Sys.argv.(0)] *)

val desktop_entry : string option ref
  (** If the application has a desktop entry, it can be specified
      here *)

type urgency = [ `low | `normal | `critical ]
    (** Urgency level of popups *)

(** An image description *)
type image = {
  img_width : int;
  img_height : int;
  img_rowstride : int;
  img_has_alpha: bool;
  img_bits_per_sample : int;
  img_channels : int;
  img_data : string;
} with obus(container)

(** Type of notification id *)
type id

(** Type of opened notifications *)
class type ['a] t = object
  method result : 'a Lwt.t
    (** Wait for a notification to be closed then return:

        - [`closed] if the user clicked on the cross, timeout was
        reached or the notification daemon exited

        - [`default] if the default action was invoked, i.e. the user
        clicked on the notification, but not on a buttons

        - the corresponding action if the user clicked on a button
        other than the cross *)

  method close : unit Lwt.t
    (** Close the notification now *)

  method id : id
    (** Identifier for the notification *)
end

val notify :
  ?app_name:string ->
  ?desktop_entry:string ->
  ?replace:_ t ->
  ?icon:string ->
  ?image:image ->
  summary:string ->
  ?body:string ->
  ?actions:(string * ([> `default | `closed ] as 'a)) list ->
  ?urgency:urgency ->
  ?category:string ->
  ?sound_file:string ->
  ?suppress_sound:bool ->
  ?pos:int * int ->
  ?hints:(string * OBus_value.single) list ->
  ?timeout:int ->
  unit -> 'a t Lwt.t
  (** Open a notification.

      - [app_name] and [desktop_entry] can override default values
      taken from references
      - [replace] is a popup id this notification replace
      - [icon] is the notification icon. It is either as a URI (file://...) or a
      name in a freedesktop.org-compliant icon theme (not a GTK+ stock ID)
      - [image] is an image, it is used if [icon] is not present
      - [summary] is a single line overview of the notification
      - [body] is a multi-line body of text. Each line is a paragraph,
      server implementations are free to word wrap them as they see fit.
      The body may contain simple markup as specified in Markup. It must be
      encoded using UTF-8.  If the body is omitted, just the summary is
      displayed.
      - [action] is a list of (text, key) pair, [text] is the text displayed to the user
      and [key] is the value which will be returned when the action is invoked
      - [category] is a string representing the category of the
      notification, for example: "device.added", "email.arrived"
      (more category can be found in the specifications)
      - [sound_file] is a sound file to play while displaying the notification
      - [suppress_sound] tell the daemon to suppress sounds
      - [pos] is a screen position
      - [hints] is a list of additionnal hints
      - [timeout] is a timeout in millisecond
  *)

val get_server_information : unit -> server_info Lwt.t
  (** Retreive server informations *)

val get_capabilities : unit -> string list Lwt.t
  (** Retreive server capabilities, see specification for details *)
