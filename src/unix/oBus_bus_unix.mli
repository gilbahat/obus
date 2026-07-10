(*
 * oBus_bus_unix.mli
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Unix-specific D-Bus message bus helpers *)

type t = OBus_connection.t

val of_addresses : ?switch : Lwt_switch.t -> OBus_address.t list -> t Lwt.t
  (** Connect to a message bus at one of the given addresses and
      register the connection with the bus. *)

val session : ?switch : Lwt_switch.t -> unit -> t Lwt.t
  (** [session ?switch ()] returns a shared connection to the user's
      D-Bus session bus.  Resolves the bus address via
      {!OBus_address_unix.session} ([$DBUS_SESSION_BUS_ADDRESS],
      XDG_RUNTIME_DIR/bus, launchd).

      OBus will call [exit 1] if the session bus connection is lost;
      override this with {!OBus_connection.set_on_disconnect}. *)

val system : ?switch : Lwt_switch.t -> unit -> t Lwt.t
  (** [system ?switch ()] returns a connection to the D-Bus system bus.
      Unlike {!session}, if the connection is lost a new one is opened
      on the next call. *)
