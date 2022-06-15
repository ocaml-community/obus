open Ppxlib

let rewriter_name = "ppx_obus"


let find_attr_expr s attrs =
  let expr_of_payload = function
    | PStr [{ pstr_desc = Pstr_eval (e, _); _ }] -> Some e
    | _ -> None in
  try expr_of_payload (
          let payload =
            List.find (fun attr -> attr.attr_name.txt = s) attrs
          in
          payload.attr_payload)
  with Not_found -> None


let register_obus_exception = function
  | { pstr_desc = Pstr_exception exn; pstr_loc } ->
    (match find_attr_expr "obus" exn.ptyexn_attributes with
    | Some expr ->
      let registerer typ =
        let loc = pstr_loc in
        if Filename.basename pstr_loc.loc_start.pos_fname = "oBus_error.ml" then
          [%stri
            let () =
              let module M =
                Register(struct
                  let name = [%e expr]
                  exception E of [%t typ]
                end)
              in ()
          ]
        else
          [%stri
            let () =
              let module M =
                OBus_error.Register(struct
                  let name = [%e expr]
                  exception E of [%t typ]
                end)
              in ()
          ] in
      (match exn.ptyexn_constructor.pext_kind with
      | Pext_decl (_, Pcstr_tuple [typ], None) ->
          Some (registerer typ)
      | _ ->
        Location.raise_errorf ~loc:pstr_loc
          "%s: OBus exceptions take a single string argument" rewriter_name)
    | _ ->
      None)
  | _ ->
    None


let obus_mapper = object(self)
  inherit Ast_traverse.map

  method! structure items =
    List.fold_right (fun item acc ->
        let item' = self#structure_item item in
        match register_obus_exception item with
        | Some reg ->
          item' :: reg :: acc
        | None ->
          item' :: acc)
      items []
end


let () =
  Driver.register_transformation
    ~impl:(fun structure -> obus_mapper#structure structure)
    rewriter_name
