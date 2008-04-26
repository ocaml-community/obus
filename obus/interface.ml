(*
 * interface.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implemtation of dbus.
 *)

type name = string
type value = string
type annotation = Annotation of name * value
type argument = Arg of name * Values.dtype
type access = Read | Write | Read_write
type definition =
  | Method of name * (*in*) argument list * (*out*) argument list * annotation list
  | Signal of name * argument list * annotation list
  | Property of name * Values.dtype * access * annotation list
type signature = Interface of name * definition list * annotation list

type property_handlers = {
  property_set : string -> Header.recv -> string -> int -> unit;
  property_get : string -> Header.recv -> string -> int -> unit;
  property_getall : Header.recv -> string -> int -> unit;
}

type handlers = {
  method_call : Header.recv -> string -> int -> bool;
  introspecter : string -> (signature * string list) option;
  property : property_handlers option;
}

type 'a t = {
  name : name;
  handlers : handlers;
}

let make_interface _ name handlers =
  { name = name;
    handlers = handlers }
let name i = i.name
let get_handlers i = i.handlers

let print_xml buf (Interface(name, definitions, annotations)) =
  let print fmt = Printf.bprintf buf fmt in
  let print_args dir args =
    List.iter begin fun (Arg(name, dtype)) ->
      print "      <arg name=\"%s\" direction=\"%s\" type=\"%s\"/>\n" name dir (Values.signature_of_dtype dtype)
    end args
  in
  let print_annotations indent annotations =
    List.iter begin fun (Annotation(name, value)) ->
      print "    %s<annotation name=\"%s\" value=\"%s\"/>\n" indent name value
    end annotations
  in
    print "  <interface name=\"%s\">\n" name;
    List.iter begin function
      | Method(name, ins, outs, annotations) ->
          print "    <method name=\"%s\">\n" name;
          print_args "in" ins;
          print_args "out" outs;
          print_annotations "  " annotations;
          print "    </method>\n"
      | Signal(name, args, annotations) ->
          print "    <signal name=\"%s\">\n" name;
          print_args "in" args;
          print_annotations "  " annotations;
          print "    </signal>\n"
      | Property(name, dtype, access, annotations) ->
          print "    <property name=\"%s\" type=\"%s\" access=\"%s\">\n"
            name (Values.signature_of_dtype dtype)
            (match access with
               | Read -> "read"
               | Write -> "write"
               | Read_write -> "readwrite");
          print_annotations "  " annotations;
          print "    </property>"
    end definitions;
    print_annotations "" annotations;
    print "  </interface>\n"
