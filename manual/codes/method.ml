open OBus_member

let m_Foo = {
  Method.interface = "org.foo.bar";
  Method.member = "Foo";
  Method.i_args = C.seq1 C.basic_string;
  Method.o_args = C.seq1 C.basic_int32;
  Method.annotations = [];
}
