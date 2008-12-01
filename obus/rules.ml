(*
 * rules.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

let to_string ?typ ?sender ?interface ?member ?path ?destination ?(args=[]) () =
  let buf = Buffer.create 42 in
  let rec coma = ref (fun _ -> coma := fun _ -> Buffer.add_char buf ',') in
  let add key value =
    !coma ();
    Buffer.add_string buf key;
    Buffer.add_string buf "='";
    Buffer.add_string buf value;
    Buffer.add_char buf '\'' in
  let add_opt key test = function
    | None -> ()
    | Some x -> match test x with
        | Some error -> raise (OBus_string.Invalid_string error)
        | None -> add key x in
  begin match typ with
    | None -> ()
    | Some t ->
        add "type"
          (match t with
             | `method_call -> "method_call"
             | `method_return -> "method_return"
             | `error -> "error"
             | `signal -> "signal")
  end;
  add_opt "sender" OBus_name.validate_bus sender;
  add_opt "interface" OBus_name.validate_interface interface;
  add_opt "member" OBus_name.validate_member member;
  begin match path with
     | None -> ()
     | Some [] ->
         !coma ();
         Buffer.add_string buf "path='/'"
     | Some p ->
         !coma ();
         Buffer.add_string buf "path='";
         List.iter
           (fun elt ->
              match OBus_path.validate_element elt with
                | Some error ->
                    raise (OBus_string.Invalid_string error)
                | None ->
                    Buffer.add_char buf '/';
                    Buffer.add_string buf elt)
           p;
         Buffer.add_char buf '\''
  end;
  add_opt "destination" OBus_name.validate_bus destination;
  List.iter (fun (n, value) -> !coma (); Printf.bprintf buf "arg%d='%s'" n value) args;
  Buffer.contents buf
