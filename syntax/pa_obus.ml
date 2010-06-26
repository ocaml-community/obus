(*
 * pa_obus.ml
 * ----------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(* Syntactic sugars for defining D-Bus exceptions *)

open Camlp4.PreCast

let () =
  Pa_type_conv.add_generator_with_arg ~is_exn:true "obus" Syntax.expr_eoi
    (fun typ arg -> match typ, arg with
       | _, None ->
           Loc.raise (Ast.loc_of_ctyp typ) (Stream.Error "pa_obus: argument recquired for the 'obus' generator")
       | Ast.TyOf(_loc, (Ast.TyId(_, (Ast.IdUid(_, caml_name)))), _), Some dbus_name ->
           if Filename.basename (Loc.file_name _loc) = "oBus_error.ml" then
             <:str_item<
               let module M =
                 Register(struct
                            let name = $dbus_name$
                            exception E = $uid:caml_name$
                          end)
               in ()
             >>
           else
             <:str_item<
               let module M =
                 OBus_error.Register(struct
                                       let name = $dbus_name$
                                       exception E = $uid:caml_name$
                                     end)
               in ()
             >>
       | _ ->
           Loc.raise (Ast.loc_of_ctyp typ) (Stream.Error "pa_obus: ``Caml_name of string'' expected"))
