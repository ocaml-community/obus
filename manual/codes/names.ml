open Lwt

lwt () =
  lwt bus = OBus_bus.session () in

  lwt () =
    try_lwt
      (* Try to resolve a name, this may fail if nobody owns it: *)
      lwt owner = OBus_bus.get_name_owner bus "org.freedesktop.Notifications" in
      Lwt_io.printlf "The owner is %d."
    with OBus_bus.Name_has_no_owner msg ->
      Lwt_io.printlf "Cannot resolve the name: %s." msg
  in

  (* Request a name: *)
  OBus_bus.request_name bus "org.foo.bar" >>= function
    | `Primary_owner ->
        Lwt_io.printl "I own the name org.foo.bar!"
    | `In_queue ->
        Lwt_io.printl "Somebody else owns the name, i am in the queue."
    | `Exists ->
        Lwt_io.printl "Somebody else owns the name\
                       and does not want to loose it :(."
    | `Already_owner
        (* Cannot happen *)
        Lwt_io.printl "I already owns this name."
