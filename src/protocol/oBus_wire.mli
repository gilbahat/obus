(*
 * oBus_lowlevel.mli
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Message serialization/deserialization *)

exception Data_error of string
  (** Exception raised when a message can not be sent. The parameter is an
      error message.

      Possible reasons are: the message is too big or contains arrays
      that are too big. *)

exception Protocol_error of string
  (** Exception raised when a received message is not valid.

      Possible reasons are:

      - a size limit is exceeded
      - a name/string/object-path is not valid
      - a boolean value is other than 0 or 1
      - ... *)

type byte_order = Little_endian | Big_endian

val system_byte_order : byte_order
  (** Native byte order of the host. *)

val read_message : (int -> string Lwt.t) -> OBus_message.t Lwt.t
  (** [read_message recv] deserializes a message. [recv n] must read
      exactly [n] bytes and return them as a string, or raise
      [End_of_file] if the connection closes. *)

val write_message : (string -> unit Lwt.t) -> ?byte_order:byte_order -> OBus_message.t -> unit Lwt.t
  (** [write_message send ?byte_order msg] serializes [msg] and passes
      the result to [send]. Fails if the message contains file
      descriptors (only possible over Unix sockets, never over TCP). *)

val message_of_string : string -> int array -> OBus_message.t
  (** [message_of_string buf fds] returns a message from a string. [fds]
      is used to resolve file descriptors the message may contain. Over
      TCP this array is always empty. *)

val string_of_message : ?byte_order:byte_order -> OBus_message.t -> string * int array
  (** Marshal a message into a string. Returns also the (always-empty
      over TCP) array of file descriptors that would need to be sent. *)

