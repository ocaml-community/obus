(*
 * obus_introspect.ml
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

let recursive = ref false
let anons = ref []
let session = ref false
let system = ref false
let address = ref None
let obj_mode = ref false

let args = [
  "-rec", Arg.Set recursive, "introspect recursively all sub-nodes";
  "-session", Arg.Set session, "the service is on the session bus (the default)";
  "-system", Arg.Set system, "the service is on the system bus";
  "-address", Arg.String (fun addr -> address := Some addr), "the service is on the given message bus";
  "-objects", Arg.Set obj_mode, "list objects with interfaces they implements instead of interfaces";
]

let usage_msg = Printf.sprintf "Usage: %s <option> <destination> <path>
Introspect a D-Bus service (print only interfaces).
options are:" (Filename.basename (Sys.argv.(0)))

module Interface_map = Map.Make(struct type t = string let compare = compare end)

let rec get proxy =
  let%lwt interfaces, children = OBus_proxy.introspect proxy in
  let map = List.fold_left (fun map (name, content, annots) ->
                              Interface_map.add name (content, annots) map)
    Interface_map.empty interfaces in
  let nodes = [(proxy, List.map (fun (name, _, _) -> name) interfaces)] in
  match !recursive with
    | true ->
        List.fold_left
          (fun t1 t2 ->
             let%lwt nodes1, map1 = t1 and nodes2, map2 = t2 in
             Lwt.return (nodes1 @ nodes2, Interface_map.fold Interface_map.add map1 map2))
          (Lwt.return (nodes, map))
          (List.map
             (fun child ->
                get { proxy with OBus_proxy.path = OBus_proxy.path proxy @ [child] })
             children)
    | false ->
        Lwt.return (nodes, map)

let main service path =
  let%lwt bus = match !session, !system, !address with
    | true, true, _
    | true, _, Some _
    | _, true, Some _ ->
        prerr_endline "must specify at most one of -session, -system\n\n";
        Arg.usage args usage_msg;
        exit 1
    | false, false, None
    | true, false, None -> OBus_bus.session ()
    | false, true, None -> OBus_bus.system ()
    | false, false, Some addr -> OBus_bus.of_addresses (OBus_address.of_string addr)
  in
  let%lwt nodes, map = get (OBus_proxy.make (OBus_peer.make bus service) path) in
  begin
    match !obj_mode with
      | false ->
          OBus_introspect.output (Xmlm.make_output ~nl:true ~indent:(Some 2) (`Channel Pervasives.stdout))
            (Interface_map.fold (fun name (content, annots) acc -> (name, content, annots) :: acc) map [], [])
      | true ->
          List.iter begin fun (proxy, interfaces) ->
            print_endline (OBus_path.to_string (OBus_proxy.path proxy));
            List.iter (Printf.printf " + %s\n") interfaces;
            print_newline ();
          end nodes
  end;
  Lwt.return ()

let () =
  Arg.parse args
    (fun arg -> anons := arg :: !anons)
    usage_msg;

  let service, path = match !anons with
    | [path; service] -> (service, OBus_path.of_string path)
    | _ -> Arg.usage args usage_msg; exit 1
  in
  try
    Lwt_main.run (main service path)
  with
    | OBus_introspect.Parse_failure((line, column), msg) ->
        Lwt.ignore_result (Lwt_io.eprintlf "invalid introspection document returned by the service!:%d:%d: %s" line column msg);
        exit 1
