# open OBus_value;;

(* Make a D-Bus value from an ocaml one: *)
# C.make_sequence (C.seq2 C.basic_int32 (C.array C.basic_string)) (42l, ["foo"; "bar"]);;
- : OBus_value.V.sequence =
[OBus_value.V.Basic (OBus_value.V.Int32 42l);
 OBus_value.V.Array (OBus_value.T.Basic OBus_value.T.String,
  [OBus_value.V.Basic (OBus_value.V.String "foo");
   OBus_value.V.Basic (OBus_value.V.String "bar")])]

(* Cast a D-Bus value to an ocaml one: *)
# C.cast_sequence (C.seq1 C.basic_string) [V.basic(V.string "foobar")];;
- : string = "foobar"

(* Try to cast a D-Bus value to an ocaml one with the wrong type: *)
# C.cast_sequence (C.seq1 C.basic_string) [V.basic(V.int32 0l)];;
Exception: OBus_value.C.Signature_mismatch.
  
