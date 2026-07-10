(*
 * oBus_transport_unix.mli
 * -----------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Unix-socket transport for D-Bus *)

val socket_of_fd :
  ?switch : Lwt_switch.t ->
  ?capabilities : OBus_auth.capability list ->
  Lwt_unix.file_descr ->
  OBus_transport.t
  (** [socket_of_fd ?switch ?capabilities fd] wraps an already-authenticated
      Unix file descriptor as an {!OBus_transport.t}.  When [`Unix_fd] is in
      [capabilities] the transport uses SCM_RIGHTS fd-passing; otherwise it
      falls back to plain byte-stream I/O. *)

val of_addresses :
  ?switch : Lwt_switch.t ->
  ?capabilities : OBus_auth.capability list ->
  ?mechanisms : OBus_auth.Client.mechanism list ->
  OBus_address.t list ->
  (OBus_address.guid * OBus_transport.t) Lwt.t
  (** [of_addresses ?switch ?capabilities ?mechanisms addrs] opens a
      connection to the first reachable address in [addrs], performs
      client-side D-Bus authentication, and returns the server GUID
      together with the ready-to-use transport.

      Supported address types: [unix], [tcp], [nonce-tcp], [launchd],
      [autolaunch]. *)
