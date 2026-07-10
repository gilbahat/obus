(*
 * oBus_transport.mli
 * ------------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Low-level transporting of messages *)

(** A raw bidirectional byte stream — the caller provides this from
    whatever transport substrate is available (Mirage TCP flow, Unix
    socket, etc.). *)
type connection = {
  read : int -> string Lwt.t;
  (** [read n] reads exactly [n] bytes; raises [End_of_file] when the
      connection closes before [n] bytes are available. *)
  write : string -> unit Lwt.t;
  (** [write s] writes the entire string [s]. *)
  close : unit -> unit Lwt.t;
  (** [close ()] closes the connection. *)
}

type t
  (** Type of message transport *)

val recv : t -> OBus_message.t Lwt.t
  (** [recv tr] receives one message from the given transport *)

val send : t -> OBus_message.t -> unit Lwt.t
  (** [send tr msg] sends [msg] over the transport [tr]. *)

val capabilities : t -> OBus_auth.capability list
  (** Returns the capabilities of the transport *)

val shutdown : t -> unit Lwt.t
  (** [shutdown tr] frees resources allocated by the given transport *)

val make :
  ?switch : Lwt_switch.t ->
  recv : (unit -> OBus_message.t Lwt.t) ->
  send : (OBus_message.t -> unit Lwt.t) ->
  ?capabilities : OBus_auth.capability list ->
  shutdown : (unit -> unit Lwt.t) -> unit -> t
  (** [make ?switch ~recv ~send ~shutdown ()] creates a new transport
      from the given functions. *)

val loopback : unit -> t
  (** Loopback transport: each message sent is received on the same
      transport *)

val of_connection :
  ?switch : Lwt_switch.t ->
  ?capabilities : OBus_auth.capability list ->
  connection ->
  t
  (** [of_connection ?switch ?capabilities conn] wraps an already-
      authenticated byte-stream connection as a message transport.
      The caller is responsible for having completed D-Bus
      authentication before calling this. *)

val connect :
  ?switch : Lwt_switch.t ->
  ?capabilities : OBus_auth.capability list ->
  ?mechanisms : OBus_auth.Client.mechanism list ->
  connection ->
  (OBus_address.guid * t) Lwt.t
  (** [connect ?switch ?capabilities ?mechanisms conn] sends the D-Bus
      initial null byte, performs client authentication using
      [mechanisms] (defaults to
      {!OBus_auth.Client.default_mechanisms}), and returns the server
      GUID and a ready-to-use transport.

      On MirageOS, build [conn] from a Mirage TCP flow:
      {[
        let recv n = (* read exactly n bytes from flow *) in
        let send s = (* write s to flow *) in
        OBus_transport.connect { recv; send; close = ... } >>= ...
      ]} *)
