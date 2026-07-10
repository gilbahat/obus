(*
 * oBus_address_unix.mli
 * ---------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Unix-specific D-Bus address resolution *)

val session : OBus_address.t list Lwt.t Lazy.t
  (** The session-bus address list, resolved lazily.  Resolution order:
      {ol
       {li [$DBUS_SESSION_BUS_ADDRESS]}
       {li [$XDG_RUNTIME_DIR/bus] (if owned by the current user)}
       {li [launchctl getenv DBUS_LAUNCHD_SESSION_BUS_SOCKET]}
       {li [[autolaunch:]] as a last resort}}
  *)
