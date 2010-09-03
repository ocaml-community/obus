exception My_exn of string

let module M = OBus_error.Register(struct
                                     exception E = My_exn
                                     let name = "org.foo.bar.MyError"
                                   end)
in ()
