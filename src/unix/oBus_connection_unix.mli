(*
 * oBus_connection_unix.mli
 * ------------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Unix-specific D-Bus connection helpers *)

val of_addresses :
  ?switch : Lwt_switch.t ->
  ?shared : bool ->
  OBus_address.t list ->
  OBus_connection.t Lwt.t
  (** [of_addresses ?switch ?shared addresses] opens a connection using Unix
      sockets (with Unix fd-passing capability enabled).

      If [shared] is [true] (the default) and a connection to a server with
      the same GUID is already live, that existing connection is returned and
      no new socket is opened. *)
