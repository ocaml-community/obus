%{
    open OBus_value
    open OBus_introspect_ext

    let rec get_members = function
      | [] -> []
      | `Member m :: rest -> m :: get_members rest
      | `Annotation _ :: rest -> get_members rest
      | `Symbol _ :: rest -> get_members rest

    let rec get_annotations = function
      | [] -> []
      | `Member _ :: rest -> get_annotations rest
      | `Annotation a :: rest -> a :: get_annotations rest
      | `Symbol _ :: rest ->  get_annotations rest
    
    let rec get_symbols = function
      | [] -> []
      | `Member _ :: rest -> get_symbols rest
      | `Annotation _ :: rest -> get_symbols rest
      | `Symbol s :: rest -> s :: get_symbols rest

    let parse_int typ str =
      match typ with
      | T.Byte -> V.Byte(char_of_int (int_of_string str))
      | T.Int16 -> V.Int16(int_of_string str)
      | T.Int32 -> V.Int32(Int32.of_string str)
      | T.Int64 -> V.Int64(Int64.of_string str)
      | T.Uint16 -> V.Uint16(int_of_string str)
      | T.Uint32 -> V.Uint32(Int32.of_string str)
      | T.Uint64 -> V.Uint64(Int64.of_string str)
      | _ -> assert false

    let rev = List.rev
%}

%token INTERFACE METHOD SIGNAL PROPERTY_R PROPERTY_W PROPERTY_RW
%token ANNOTATION ENUM FLAG WITH
%token COMMA PERIOD LBRACE RBRACE LPAREN RPAREN
%token EQMARK COLON PLUS MINUS ARROW UNDERSCORE STAR
%token<string> LIDENT UIDENT
%token<string> STRING
%token<string> INT
%token EOF

%start <OBus_introspect_ext.interface list> interfaces

%%

interfaces:
  | EOF { [] }
  | iface = interface; ifaces = interfaces
    { iface :: ifaces }
;

interface:
  | INTERFACE; name = name; LBRACE; members = members; RBRACE
    { (name, get_members members, get_symbols members, get_annotations members) }
;

ident:
  | n = LIDENT { n }
  | n = UIDENT { n }
;

name:
  | n = ident { n }
  | n = ident; PERIOD; rest = name
    { n ^ "." ^ rest }
;

members:
  | { [] }
  | members = members; member = member
    { member :: members }
;

member:
  | METHOD; name = ident; COLON; LPAREN; i_args = arguments; RPAREN; ARROW; LPAREN; o_args = arguments; RPAREN; annot = annotations
    { `Member (Method (name, i_args, o_args, annot)) }
  | SIGNAL; name = ident; COLON; LPAREN; args = arguments; RPAREN; annot = annotations
    { `Member (Signal (name, args, annot)) }
  | PROPERTY_R; name = ident; COLON; typ = type_term; annot = annotations
    { `Member (Property (name, typ, Read, annot)) }
  | PROPERTY_W; name = ident; COLON; typ = type_term; annot = annotations
    { `Member (Property (name, typ, Write, annot)) }
  | PROPERTY_RW; name = ident; COLON; typ = type_term; annot = annotations
    { `Member (Property (name, typ, Read_write, annot)) }
  | ANNOTATION; name = STRING; EQMARK; value = STRING
    { `Annotation (name, value) }
  | ENUM; name = ident; COLON; typ = key_type; LBRACE; values = values; RBRACE
    { `Symbol (name,
               sym_enum typ (List.map (fun (key, value) ->
                                        (parse_int typ key, value)) values)) }
  | FLAG; name = ident; COLON; typ = key_type; LBRACE; values = values; RBRACE
    { `Symbol (name,
               sym_flag typ (List.map (fun (key, value) ->
                                        (parse_int typ key, value)) values)) }
;

values:
  | { [] }
  | vals = values; v = value
    { v :: vals }
;

value:
  | key = INT; COLON; value = ident
    { (key, value) }
  | MINUS; key = INT; COLON; value = ident
    { ("-" ^ key, value) }
  | PLUS; key = INT; COLON; value = ident
    { (key, value) }
;

annotations:
  | { [] }
  | WITH; LBRACE; annots = annotations; last = annotation; RBRACE
    { last :: annots }
;

annotation:
  | name = name; EQMARK; value = STRING
    { (name, value) }
;

arguments:
  | { [] }
  | arg = argument
    { [ arg ] }
  | arg = argument; COMMA; rest = arguments
    { arg :: rest }
;

argument:
  | name = ident; COLON; typ = type_term
    { (Some name, typ) }
  | UNDERSCORE; COLON; typ = type_term
    { (None, typ) }
;

type_term:
  | id = ident
    { term id [] }
  | LPAREN; t = type_term; RPAREN
    { t }
  | LPAREN; tup = type_tuple; RPAREN
    { tuple tup }
  | t = type_term; id = ident
    { term id [t] }
  | LPAREN; args = type_args; RPAREN; id = ident
    { term id args }
;

type_tuple:
  | t = type_term; STAR; tl = type_tuple
    { t :: tl }
  | t = type_term
    { [ t ] }
;

type_args:
  | t = type_term; COMMA; tl = type_args
    { t :: tl }
  | t = type_term
    { [ t ] }
;

key_type:
  | id = LIDENT
    { match id with
      | "byte" -> T.Byte
      | "int16" -> T.Int16
      | "int32" -> T.Int32
      | "int64" -> T.Int64
      | "uint16" -> T.Uint16
      | "uint32" -> T.Uint32
      | "uint64" -> T.Uint64
      | _ -> raise (Failure(Printf.sprintf "invalid key type: %s" id))
    }
;
