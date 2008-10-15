(*
 * mQueue.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

open Lwt

type 'a queues = {
  mutable down : unit Lwt.t option;
  mutable queued : 'a Queue.t;
  mutable waiters : 'a t Queue.t;
}

type 'a state =
  | OK of 'a queues
  | Aborted of exn

type 'a t = 'a state ref

let with_ok f q = match !q with
  | OK x -> f x
  | Aborted exn -> raise exn

let lwt_with_ok f q = match !q with
  | OK x -> f x
  | Aborted exn -> fail exn

let create () = ref (OK { queued = Queue.create (); waiters = Queue.create (); down = None })

let put x = with_ok (fun q -> match Queue.is_empty q.waiters with
                       | true -> Queue.push x q.queued
                       | false -> wakeup (Queue.pop q.waiters) x)

let get r = lwt_with_ok (fun q -> match Queue.is_empty q.queued with
                           | true ->
                               let w = wait () in
                               Queue.push w q.waiters;
                               w
                           | false ->
                               let x = Queue.pop q.queued in
                               match q.down with
                                 | Some w ->
                                     w >>= (fun _ -> return x)
                                 | None -> return x) r

let clear r = with_ok (fun q ->
                         Queue.clear q.queued;
                         Queue.clear q.waiters) r

let abort r exn = match !r with
  | Aborted exn -> ()
  | OK q ->
      r := Aborted exn;
      Queue.iter (fun w -> wakeup_exn w exn) q.waiters;
      begin match q.down with
        | Some w -> wakeup_exn w exn
        | None -> ()
      end;
      Queue.clear q.queued;
      Queue.clear q.waiters

let aborted r = match !r with
  | OK _ -> None
  | Aborted exn -> Some exn

let is_up r = with_ok (fun q -> q.down = None) r
let set_up r = with_ok (fun q -> match q.down with
                          | None -> ()
                          | Some w ->
                              q.down <- None;
                              wakeup w ()) r
let set_down r = with_ok (fun q -> match q.down with
                            | Some _ -> ()
                            | None -> q.down <- Some(wait ())) r

let queued r = with_ok (fun q -> q.queued) r
let waiters r = with_ok (fun q -> q.waiters) r
