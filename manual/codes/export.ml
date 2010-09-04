lwt () =
  lwt bus = OBus_bus.session () in

  (* Request a name: *)
  lwt _ = OBus_bus.request_name bus "org.Foo.Bar" in

  (* Create the object: *)
  let obj = OBus_object.make ~interfaces:[Foobar.Org_Foo_Bar.interface] ["plip"] in

  (* Attach it some data: *)
  OBus_object.attach obj ();

  (* Export the object on the connection *)
  OBus_object.export bus obj;

  (* Wait forever *)
  fst (wait ())
