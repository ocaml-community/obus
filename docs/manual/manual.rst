********
Overview
********

**D-Bus** is an inter-processes communication protocol, or IPC for
short, which has recently become a standard on desktop oriented
computers. It is now possible to talk to a lot application using
dbus. Moreover, it has many bindings/implementations for differents
languages, which make it easily accessible. **OBus** is a pure OCaml
implementation of this protocol. What makes it different from other
bindings/implementations is that it is the only one using
cooperative threads, which makes it very simple to fully exploit the
asynchronous nature of dbus.

The main package of the OBus distribution is the ``obus`` findlib
package, which contains the core library and some utilities for
generating OCaml client modules from arbitrary dbus services.
OBus also comes with some packages containing high level bindings to
a few well-known Freedesktop dbus services:

- ``obus.hal``
- ``obus.notification``
- ``obus.network-manager``
- ``obus.policykit``
- ``obus.udisks``
- ``obus.upower``

The low-level API is described in the section `Low-level use of OBus`_
of this manual. Note that you must have a good knowledge of dbus to
use it effectively.

It is recommended to familiarize yourself with the Lwt_ library before
using OBus.

.. _Lwt: https://ocsigen.org/lwt/

------------------------------------------

***********
Quick start
***********

This section provides simple usage examples of OBus and the utilities
it comes with. You can also look at the examples_ directory for more
concrete examples.

.. _examples: https://github.com/diml/obus/tree/master/docs/examples


Using the predefined bindings
-----------------------------

The usage of the predefined bindings is straightforward and doesn't
require any knowledge of dbus nor OBus. This is a program that opens
a popup notification::

  let () = Lwt_main.run begin
    let%lwt id = Notification.notify ~summary:"Hello, world!" () in
    Lwt.return ()
  end


Generating a client OCaml module from a running service
-------------------------------------------------------

To use a dbus service, you first have to obtain its interface through
its published introspection XML. Some applications put these files into
``/usr/share/dbus-1/interfaces/``, but you can also just directly ask a
running service::

  $ obus-introspect -rec org.foo.bar / > foo.xml

This will recursively introspect the ``org.foo.bar`` service, and dump
its interface data into ``foo.xml``

The next step is to generate an OCaml module describing its interface::

  $ obus-gen-interface foo.xml

This will generate ``foo_interfaces.ml`` and ``foo_interfaces.mli``.
The generated interfaces shouldn't be directly edited.

Now we can generate the client module::

  $ obus-gen-client foo.xml

This will generate ``foo_client.ml`` and ``foo_client.mli``.
These generated clients can be freely edited, and have to be compiled
with the ``lwt_ppx`` syntax extension.

Now we can use the ``Foo_client`` module to interact with the service.
Methods are mapped to functions returning ``Lwt.t`` wrapped values,
signals are mapped to values of type ``OBus_signal.t``, and properties
to values of type ``OBus_property.t``. For example::

  let () = Lwt_main.run begin
    (* Connect to the session bus *)
    let%lwt bus = OBus_bus.session () in

    (* Create a proxy for a remote object *)
    let proxy =
      OBus_proxy.make
        (OBus_peer.make bus "org.foo.bar")
        ["org"; "foo"; "bar"]
    in

    (* Call a method *)
    let%lwt result = Foo_client.Org_foo_bar.plop proxy ... in

    (* Connect to a signal *)
    let%lwt () =
      Lwt_react.E.notify (fun args -> ...)
      =|< OBus_signal.connect (Foo_client.Org_foo_bar.plip proxy)
    in

    (* Read the contents of a property *)
    let%lwt value = OBus_property.get (Foo_client.Org_foo_bar.plap proxy) in

    ...
  end


-----------------------------------------------

******
Basics
******

In this section we will describe the minimum you must know to use
OBus and interfaces for dbus services written with OBus (like the
ones provided in the OBus distribution: ``obus.notification``,
``obus.upower``, ...)


Connections and message buses
-----------------------------

A ``connection`` is a way of exchanging messages with another
application speaking the dbus protocol. Most of the time applications
use connection to a special application called a *message bus*.
A message bus act as a router between several applications. On a desktop
computer, there are two well-known instances: the *system* message bus,
and the user *session* message bus.

The first one is unique given a computer, and uses security
policies. The second is unique given a user session. Its goal is to
allow programs running in the same session to talk to each other.
OBus offers two function for connecting to these message buses:
``OBus_bus.session`` and ``OBus_bus.system``.

The session bus exists for the life-time of a user session. It exits
when the session is closed, and any programs using it should exit to,
that is why OBus will exit the program when the connection to the
session bus is lost. However this behavior can be changed.

On the other hand, the system bus can be restarted and programs using it
may try to reopen the connection. System-wide application should
handle the loss of the connection with the system bus.

Here is a small example which connects the session bus and prints its id::

  let () = Lwt_main.run begin
    (* Open a connection to the session message bus: *)
    let%lwt bus = OBus_bus.session () in

    (* Obtain its id: *)
    let%lwt id = OBus_bus.get_id bus in

    Lwt_io.printlf "The session bus id is %d." (OBus_uuid.to_string id)
  end


Names
-----

On a message bus, applications are referenced using names. There is a
special category of names called *unique names*. Each time an
application connects to a bus, the bus give it a unique name. Unique
name are of the form ``:1.42`` and cannot be changed. You can
think of a unique name as an *ip* (such as ``192.168.1.42``).

Once connected, the unique name can be retrieved with the function
``OBus_bus.name``. Here is a program that prints its own unique name::

  let () = Lwt_main.run begin
    (* Connects to the session bus: *)
    let%lwt bus = OBus_bus.session () in

    (* Read our unique name: *)
    let%lwt name = OBus_bus.name bus in

    Lwt_io.printlf "My unique connection name is %s." name
  end

Unique names are useful to uniquely identify an application. However,
when you want to use a specific service you may prefer using a
well-known name such as ``org.freedesktop.Notifications``. D-Bus
allows applications to own as many non-unique names as they want. You
can think of a non-unique name as an *url* (such as
``obus.forge.ocamlcore.org``).

Names can be requested or resolved using functions of the ``OBus_bus`` module.
Here is an example::

  let () = Lwt_main.run begin
    let%lwt bus = OBus_bus.session () in

    let%lwt () =
      try%lwt
        (* Try to resolve a name, this may fail if nobody owns it: *)
        let%lwt owner =
          OBus_bus.get_name_owner bus "org.freedesktop.Notifications"
        in
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
                         and does not want to lose it :(."
      | `Already_owner
          (* Cannot happen *)
          Lwt_io.printl "I already owns this name."
  end

Note that the ``OBus_resolver`` module offer a better way of resolving
names and monitoring name owners. See section `Name Tracking`_ for details.


Peers
-----

A *peer* represents an application accessible through a dbus connection.
To uniquely identify a peer one needs a connection and a name.
The module ``OBus_peer`` defines the type type of peers.
There are two requests that should be available on all peers:
``ping`` and ``get_machine_id``. The first one just pings the peer to see
if it is alive, and the second returns the id of the machine the peer
is currently running on.


Objects and proxies
-------------------

In order to export services, dbus uses the concept of *objects*.
An application may holds as many objects as it wants.
From the inside of the application, dbus objects are generally mapped to
language-native objects. From the outside, objects are refered to though
*object-paths*, which looks like ``/org/freedesktop/DBus``.
You can think of an object path as a pointer.

Objects may have members which are organized by interface (such as
``org.freedesktop.DBus``. There are three types of members:

- Methods
- Signals
- Properties

Methods act like functions which can be called by any client.

Signals are spontaneous events that may occurs at any time, which clients
may register to in order to be notified when they occur.

Properties act as variables, which can be read and/or written, and
sometimes monitored.

In order to uniquely identify an object, we need its path and the peer
that owns it. We call such a thing a *proxy*. Proxies are defined
in the module ``OBus_proxy``

Here is a simple example of how to call a method on a proxy (we will
explain the ``C.seq...`` things later)::

  open OBus_value

  let () = Lwt_main.run begin
    let%lwt bus = OBus_bus.session () in

    (* Create the peer: *)
    let%lwt peer = OBus_peer.make ~name:"org.freedesktop.DBus" ~connection:bus in

    (* Create the proxy: *)
    let%lwt proxy = OBus_proxy.make ~peer ~path:["org"; "freedesktop"; "DBus"] in

    (* Call a method: *)
    let%lwt id =
      OBus_proxy.call proxy
        ~interface:"org.freedesktop.DBus"
        ~member:"GetId"
        ~i_args:C.seq0
        ~o_args:(C.seq1 C.basic_string)
        ()
    in

    Lwt_io.printlf "The bus id is: %s" id
  end


--------------------------------------------------

*******************************************************
Interaction between the OCaml world and the D-Bus world
*******************************************************

Value mapping
-------------

D-Bus defines its own type system, which is used to serialize and
deserialize messages.  These types are defined in the module
``OBus_value.T``, and dbus values are defined in the module
``OBus_value.V``.  When a message is received, its contents are
represented as a value of type ``OBus_value.V.sequence``.
Similarly, when a message is sent, it is first converted into this
format.

Manipulating boxed dbus values is not very handy. To make the
interaction more transparent, OBus defines a set of type combinators
which allow to easily switch between the dbus representation and the
OCaml representation. These converters are defined in the module
``OBus_value.C``. Here is an example (in the toplevel)::

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


Error mapping
-------------

A call to a method may fail. In this case the service sends an error
to the caller. OCaml exceptions can be mapped to dbus errors with the
the ``OBus_error`` module by registering them with the
``OBus_error.Register`` functor. OBus provides a PPX syntax extension
to simplify this process::

  exception My_exn of string
    [@@obus "org.foo.bar.MyError"]


-----------------------------------------------------

********************
Using D-Bus services
********************

In this section we describe the canonical way of using a dbus service
with OBus.


Defining and using members
--------------------------

For all types of members (methods, signals and properties), dbus
provides types to defines them and functions to use these definitions.
A member definition contains all the information about a member.
For example, here is the definition of a method call named ``foo``
on interface ``org.foo.bar`` which takes a string and returns
an 32-bits signed integer::

  open OBus_member

  let m_Foo = {
    Method.interface = "org.foo.bar";
    Method.member = "Foo";
    Method.i_args = C.seq1 C.basic_string;
    Method.o_args = C.seq1 C.basic_int32;
    Method.annotations = [];
  }

Once a member is defined, it can be used by the corresponding modules::

  open OBus_members

  (* Definition of a method *)
  let m_GetId = {
    Method.interface = "org.freedesktop.DBus";
    Method.member = "GetId";
    Method.i_args = C.seq0;
    Method.o_args = C.seq1 C.basic_string;
    Method.annotations = [];
  }

  (* Definition of a signal *)
  let s_NameAcquired = {
    Signal.interface = "org.freedesktop.DBus";
    Signal.member = "NameAcquired";
    Signal.args = C.seq1 (C.basic C.string);
    Signal.annotations = [];
  }

  let () = Lwt_main.run begin
    let%lwt bus = OBus_bus.session () in
    let proxy =
      OBus_proxy.make
        (OBus_peer.make bus "org.freedesktop.DBus")
        ["org"; "freedesktop"; "DBus"]
    in

    (* Call the method we just defined: *)
    let%lwt id = OBus_method.call m_GetId proxy () in

    (* Register to the signal we just defined: *)
    let%lwt event = OBus_signal.connect (OBus_signal.make s_NameAcquired proxy) in

    Lwt_react.E.notify_p
      (fun name ->
         Lwt_io.printlf "name acquired: %s" name)
      event;

    Lwt_io.printlf "The message bus id is %s" id
  end

Of course, writing definitions by hand may be very boring and error-prone.
To avoid that, OBus provides a few tools to automatically convert
introspection data to OCaml definitions.


Using tools to generate member definitions
------------------------------------------

There are two tools that are useful for client-side code:
``obus-gen-interface`` and ``obus-gen-client``.
The first one converts an xml introspection document (or an IDL_ file)
into an OCaml module containing all the caml-ized definitions.
This generated file is in fact also needed for server-side code.
Note that files produced by ``obus-gen-interface`` are not meant to be
edited.

The second tool maps members to their OCaml counterpart: methods are
mapped to functions, signals to value of type ``OBus_signal.t``
and properties to values of type ``OBus_property.t``.
This generated file is meant to be edited. For example, you can edit it in
order to change the type of values taken/returned by methods.

.. _IDL:
The IDL language
----------------

Since editing XML is horrible, OBus provides a intermediate language
to write dbus interfaces. This language also allows you to
automatically converts integers to OCaml variants when needed.

The syntax is pretty simple. Here is an example, taken from the OBus
sources (file ``src/oBus_interfaces.obus``)::

  interface org.freedesktop.DBus {
    (** A method definition: *)
    method Hello : () -> (name : string)

    (** Bitwise flags definition: *)
    flag request_name_flags : uint32 {
      0b001: allow_replacement
      0b010: replace_existing
      0b100: do_not_queue
    }

    (** Definition of an enumeration: *)
    enum request_name_result : uint32 {
      1: primary_owner
      2: in_queue
      3: exists
      4: already_owner
    }

    (** A method that use newly defined types: *)
    method RequestName :
      (name : string, flags : request_name_flags)
      -> (result : request_name_result)
  }

All obus tools that accept XML files also accept IDL files. It is also
possible to convert between IDL and XML with ``obus-idl2xml``
and ``obus-xml2idl``.


Name tracking
-------------

The owner of a non-unique name may change over time, so OBus provides
the ``OBus_resolver``, which maps the name to a React signal that holds
its current owner.


-----------------------------------------------------


**********************
Writing D-Bus services
**********************

In this document we describe the canonical way of writing dbus services
with OBus.

Local dbus objects are represented by values of type ``OBus_object.t``.
The main operations on objects are: adding an interface and exporting
it on a connection. Exporting an object means making it available
to all peers reachable from the connection.

In order to add callable methods to objects you have to create
interfaces descriptions (of type ``'a OBus\_object.interface``)
and add them to objects.

The canonical way to create interfaces with OBus is to first write
its signature in an XML introspection file or in an OBus IDL file,
then convert it into an ocaml definition module with
``obus-gen-interface`` and in a template ocaml source file with
``obus-gen-server``.

Here is a small example of an interface::

  interface org.Foo.Bar {
    method GetApplicationName : () -> (name : string)
      (** Returns the name of the application *)
  }

It is converted with::

  $ obus-gen-interface foobar.obus -o foobar_interfaces
  file "foobar_interfaces.ml" written
  file "foobar_interfaces.mli" written
  $ obus-gen-server foobar.obus -o foobar
  file "foobar.ml" written

Now all that you have to do is to edit the file generated by
``obus-gen-server`` and replace the "Not implemented" errors with
your code. Once you are done, we're ready to actually create
the object, add the interface and export it::

  let () = Lwt_main.run begin
    let%lwt bus = OBus_bus.session () in

    (* Request a name: *)
    let%lwt _ = OBus_bus.request_name bus "org.Foo.Bar" in

    (* Create the object: *)
    let obj =
      OBus_object.make
        ~interfaces:[Foobar.Org_Foo_Bar.interface]
        ["plip"]
    in

    (* Attach it some data: *)
    OBus_object.attach obj ();

    (* Export the object on the connection *)
    OBus_object.export bus obj;

    (* Wait forever *)
    fst (Lwt.wait ())
  end

Note the you can attach custom data to the object with
``OBus_object.attach``.


---------------------------------------------


************************
One-to-one communication
************************

Instead of connection to a message bus, you may want to directly connects
to another application. This can be done with ``OBus_connection.of_addresses``.

If you want to allow other applications to connect to your application
you have to start a server. Starting a server is very simple, all you
have to do is to call ``OBus_server.make`` with a callback
that will receive new connections.


-----------------------------------------------------


*********************
Low-level use of OBus
*********************

This document describes the low-level part of obus.


Message filters
---------------

Message filters are function that are applied to all
incoming/outgoing messages. Filters are of type::

  type filter = OBus_message.t -> OBus_message.t option

Each filter may use and/or modify the message. If ``None`` is
returned the message is dropped.


Matching rules
--------------

When using a message bus, an application do not receive messages that
are not destined to it. In order to receive such messages, one needs to
add rules on the message bus. All messages matching a rule are sent to
the application which defined that rule.

There are two ways of adding matching rules: by using the module
``OBus_bus``, or by using ``OBus_match``.
The functions ``OBus_bus.add_match`` and ``OBus_bus.remove_match``
are directly mapped to the corresponding methods of the message bus.
The function ``OBus_match.export`` is more clever:

- it exports only one time duplicated rules,
- it exports only the most general rules.

We say that a rule ``r1`` is more general that a rule ``r2``
if all messages matched by ``r2`` are also matched by
``r1``. For example, a rule that accepts all messages with
interface field equal to ``foo.bar`` is more general that a rule
that accept all messages with interface field equal to
``foo.bar`` and with member field equal to ``plop``.

Note that you must be careful if you use both modules that
automatically manage rules (such as ``OBus_signal``, ``OBus_resolver``
or ``OBus_property``) and ``OBus_bus.add_match`` or ``OBus_bus.remove_match``.


Defining new transports
-----------------------

A transport is a way of receiving and sending messages. The
``OBus_transport`` module allows to define new transports. If you want
to create a new transport using the same serialization format as
default transport, then you can use the ``OBus_wire`` module.

By defining new transports, you can for example write an application
that forward messages over the network in very few lines of code.


Defining new authentication mechanisms
--------------------------------------

When openning a connection, before we can send and receive message
over it, dbus requires a authentication procedure. OBus implements
both client and server side authentication.  The ``OBus_auth``
allow to write new client and server side authentication mechanisms.
