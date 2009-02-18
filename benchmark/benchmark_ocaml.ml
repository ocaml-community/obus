(*
 * benchmark_ocaml.ml
 * ------------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt

let rec make_array = function
  | 0 -> []
  | n -> n :: make_array (n - 1)

let values = [
  OBus_type.make_single <:obus_type< (int * int * int) structure >> (1, 2, 3);
  OBus_type.make_single <:obus_type< (int, string) dict >> [1, "Hello world!";
                                                            2, "I am a geek!";
                                                            3, "Are you mad?"];
  OBus_type.make_single <:obus_type< int list >> (make_array 1000);
]

let count = 1000
let total = count * List.length values

class benchmark w = object
  inherit OBus_object.t
  method obus_path = [ "obus"; "benchmark" ]

  inherit (OBUS_interface "obus.benchmark"
             OBUS_method Test : variant -> unit
           end)

  val mutable received = 0

  method test v =
    received <- received + 1;
    if received = total then wakeup w ();
    return ()
end

let test proxy x =
  OBus_proxy.method_call proxy
    ~interface:"obus.benchmark"
    ~member:"Test"
    <:obus_func< variant -> unit >>
    (OBus_value.variant x)

let rec loop proxy = function
  | 0 ->
      return ()
  | n ->
      Lwt_util.join
        [Lwt_util.iter (test proxy) values;
         loop proxy (n - 1)]

let main connection =
  let w = wait () in
  let benchmark = new benchmark w in
  benchmark#obus_export connection;
  let proxy = OBus_proxy.make (OBus_peer.anonymous connection) [ "obus"; "benchmark" ] in
  (perform
     loop proxy count;
     w;
     OBus_connection.close connection)

let connection_of_fds (fd_r, fd_w) =
  OBus_connection.of_transport
    (let transport = OBus_lowlevel.transport_of_channels
       (OBus_lowlevel.make_ichan (Lwt_unix.read fd_r),
        OBus_lowlevel.make_ochan (Lwt_unix.write fd_w)) in
     { transport with
         OBus_lowlevel.shutdown = fun _ ->
           Lwt.finalize transport.OBus_lowlevel.shutdown
             (fun _ ->
                Lwt_unix.close fd_r;
                Lwt_unix.close fd_w;
                return ()) })

let _ =
  let fd1_r, fd1_w = Lwt_unix.pipe () in
  let fd2_r, fd2_w = Lwt_unix.pipe () in
  if Unix.fork () = 0 then begin
    Lwt_unix.close fd1_r;
    Lwt_unix.close fd2_w;
    Lwt_unix.run (main (connection_of_fds (fd2_r, fd1_w)))
  end else begin
    Lwt_unix.close fd1_w;
    Lwt_unix.close fd2_r;
    Lwt_unix.run (main (connection_of_fds (fd1_r, fd2_w)))
  end
