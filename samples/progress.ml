(*
 * progress.ml
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

(* Simple service which create progress bar with zenity *)

open Printf
open Lwt
open OBus_type

OBUS_flag closed_reason : uint =
    | 0 -> Cancel
    | 1 -> OK
    | 2 -> Explicitly_closed
    | 3 -> Killed

OBUS_class if_manager "org.ocamlcore.forge.obus.ProgressBar.Manager" = object
  OBUS_method ServerVersion : string;
  OBUS_method CreateProgressBar : OBus_context.t -> int -> OBus_object.t;
end

OBUS_class if_bar "org.ocamlcore.forge.obus.ProgressBar.Bar" = object
  OBUS_property_rw Position : int;
  OBUS_method Close : unit;
  OBUS_signal Closed : closed_reason;
end

OBUS_global_exn Invalid_value = "org.ocamlcore.forge.obus.ProgressBar.InvalidValue"
OBUS_global_exn Closed = "org.ocamlcore.forge.obus.ProgressBar.Closed"

class bar bus client initial_value =
  let fdr, fdw = Lwt_unix.pipe_out () in
  (* Launch zenity *)
  let pid = Unix.create_process "zenity"
    [|"zenity"; "--progress"; string_of_int initial_value|]
    fdr Unix.stdout Unix.stderr in
  let _ = Unix.close fdr in

  (* Channel used to write values to zenity *)
  let oc = Lwt_chan.out_channel_of_descr fdw in

object(self)
  inherit OBus_object.owned bus client as super
  inherit if_bar

  (* Progress bar position *)
  val mutable position = initial_value

  (* Will be true is explicitly closed *)
  val mutable close_asked = false

  method position_set x =
    if x < 0 || x > 100 then
      fail (Invalid_value "position must be between 0 and 100")
    else begin
      position <- x;
      catch
        (fun _ -> perform
           Lwt_chan.output_string oc (string_of_int x);
           Lwt_chan.output_char oc '\n';
           Lwt_chan.flush oc)
        (function
           | Unix.Unix_error(err, _, _) ->
               printf "failed to output the position to zenity: %s\n%!" (Unix.error_message err);
               fail (OBus_error.Failed "error")
           | exn -> fail exn)
    end

  method position_get =
    return position

  method close =
    Unix.kill pid Sys.sigterm;
    return ()

  method obus_destroy =
    Unix.kill pid Sys.sigterm;
    super#obus_destroy

  initializer
    printf "new progress bar, with initial value %d\n%!" initial_value;

    ignore_result
      (perform
         (* Wait for its termination *)
         (_, status) <-- Lwt_unix.waitpid [] pid;
         let _ =
           printf "zenity with pid %d killed\n%!" pid;
           Lwt_unix.abort fdw (Closed "zenity exited");
           Lwt_chan.close_out oc
         in

         (* Send the close signal *)
         if close_asked then
           self#closed Explicitly_closed
         else
           (match status with
              | Unix.WEXITED 0 -> self#closed OK
              | Unix.WEXITED _ -> self#closed Cancel
              | _ -> self#closed Killed);

         let _ = super#obus_destroy in
         return ())

end

let manager = object
  inherit OBus_object.t
  inherit if_manager

  method obus_path = ["org"; "ocamlcore"; "forge"; "obus"; "ProgressBar"; "Manager"]

  method serverVersion = return "1.0"

  method createProgressBar ctx x =
    if x < 0 || x > 100 then
      fail (Invalid_value "position must be between 0 and 100")
    else
      return (new bar (OBus_context.connection ctx) (match OBus_context.sender ctx with
                                                       | Some s -> s
                                                       | None -> assert false) x :> OBus_object.t)
end

let _ = Lwt_unix.run
  (perform
     bus <-- Lazy.force OBus_bus.session;

     (* export the object *)
     let _ = manager#obus_export bus in

     (* ask for a well-know name *)
     OBus_bus.request_name bus "org.ocamlcore.forge.obus.ProgressBar" [];

     (* Wait for the message bus to exit *)
     OBus_connection.watch bus)