(*
 * oBus_server.mli
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Servers for one-to-one D-Bus communication *)

type t
  (** Type of a server *)

val addresses : t -> OBus_address.t list
  (** [addresses server] returns all addresses the server is listening on.
      Pass these to clients so they can connect. *)

val shutdown : t -> unit Lwt.t
  (** [shutdown server] shuts down all listeners.  Idempotent. *)

val make :
  ?switch : Lwt_switch.t ->
  ?capabilities : OBus_auth.capability list ->
  ?mechanisms : OBus_auth.Server.mechanism list ->
  ?addresses : OBus_address.t list ->
  ?allow_anonymous : bool ->
  (t -> OBus_connection.t -> unit) -> t Lwt.t
  (** [make ?switch ?capabilities ?mechanisms ?addresses ?allow_anonymous f]
      creates a server listening on [addresses] (default: a Unix abstract
      socket under [Filename.get_temp_dir_name ()]).

      For each new client [f] is called with the server and a {b down}
      connection; call {!OBus_connection.set_up} to start dispatching. *)

val make_lowlevel :
  ?switch : Lwt_switch.t ->
  ?capabilities : OBus_auth.capability list ->
  ?mechanisms : OBus_auth.Server.mechanism list ->
  ?addresses : OBus_address.t list ->
  ?allow_anonymous : bool ->
  (t -> OBus_transport.t -> unit) -> t Lwt.t
  (** Like {!make} but [f] receives the raw transport rather than a
      connection, so the caller controls connection creation. *)
