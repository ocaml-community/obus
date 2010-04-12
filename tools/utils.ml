(*
 * utils.ml
 * --------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Printf
open OBus_value

module IFSet = Set.Make(struct
                          type t = OBus_introspect.interface
                          let compare (n1, _, _) (n2, _, _) = String.compare n1 n2
                        end)

let parse_file fname =
  let ic = open_in fname in
  try
    let interfaces, _ = OBus_introspect.input (Xmlm.make_input ~strip:true (`Channel ic)) in
    close_in ic;
    List.fold_left (fun acc iface -> IFSet.add iface acc) IFSet.empty interfaces
  with
    | OBus_introspect.Parse_failure((line, column), msg) ->
        Printf.eprintf "%s:%d:%d: %s.\n%!" fname line column msg;
        exit 1

let file_name_of_interface_name name =
  let result = String.create (String.length name) in
  for i = 0 to String.length name - 1 do
    if name.[i] = '.' then
      result.[i] <- '_'
    else
      result.[i] <- name.[i]
  done;
  result

let paren top s = if top then s else sprintf "(%s)" s

let make_names l =
  let rec aux n = function
    | [] -> []
    | _ :: l -> sprintf "x%d" n :: aux (n + 1) l
  in
  aux 1 l

let rec convertor_single basic top = function
  | T.Basic t ->
      basic top t
  | T.Array t -> begin
      match convertor_single basic false t with
        | Some f -> Some(paren top (sprintf "List.map %s" f))
        | None -> None
    end
  | T.Dict(tk, tv) -> begin
      match basic true tk, convertor_single basic true tv with
        | None, None ->
            None
        | Some fk, None ->
            Some(paren top (sprintf "List.map (fun (k, v) -> (%s k, v))" fk))
        | None, Some fv ->
            Some(paren top (sprintf "List.map (fun (k, v) -> (k, %s v))" fv))
        | Some fk, Some fv ->
            Some(paren top (sprintf "List.map (fun (k, v) -> (%s k, %s v))" fk fv))
    end
  | T.Structure tl ->
      let l = List.map (convertor_single basic true) tl in
      if List.exists (fun f -> f <> None) l then begin
        let names = make_names tl in
        Some(sprintf "(fun (%s) -> (%s))"
               (String.concat ", " names)
               (String.concat ", " (List.map2
                                      (fun name conv ->
                                         match conv with
                                           | Some f -> sprintf "%s %s" f name
                                           | None -> name)
                                      names l)))
      end else
        None
  | T.Variant ->
      None

let convertor_send typ =
  convertor_single
    (fun top t -> match t with
       | T.Int32 | T.Uint32 -> Some "Int32.of_int"
       | T.Object_path -> Some "OBus_proxy.path"
       | _ -> None)
    true typ

let convertor_recv typ =
  convertor_single
    (fun top t -> match t with
       | T.Int32 | T.Uint32 -> Some "Int32.to_int"
       | T.Object_path -> Some(paren top ("OBus_proxy.make (OBus_context.sender context)"))
       | _ -> None)
    true typ
