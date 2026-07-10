(*
 * oBus_wire_unix.mli
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Unix-specific D-Bus wire layer with file-descriptor passing *)

(** {6 Writer} *)

type writer
  (** Wraps a Unix file descriptor for message writing with FD passing. *)

val writer : Lwt_unix.file_descr -> writer
val close_writer : writer -> unit Lwt.t

val write_message_with_fds :
  writer -> ?byte_order:OBus_wire.byte_order -> OBus_message.t -> unit Lwt.t
  (** Serialise [msg] and send it (with any embedded file descriptors)
      via the SCM_RIGHTS ancillary-data mechanism. *)

(** {6 Reader} *)

type reader
  (** Wraps a Unix file descriptor for message reading with FD passing. *)

val reader : Lwt_unix.file_descr -> reader
val close_reader : reader -> unit Lwt.t

val read_message_with_fds : reader -> OBus_message.t Lwt.t
  (** Receive the next message, collecting any file descriptors that
      arrived via [recv_msg], and embed them as {!OBus_value.V.Unix_fd}
      values inside the returned message. *)

(** {6 Lwt_io wrappers} *)

val read_message_of_ic : Lwt_io.input_channel -> OBus_message.t Lwt.t
  (** Read a message from an [Lwt_io] input channel.
      File-descriptor passing is not supported on channels. *)

val write_message_of_oc :
  Lwt_io.output_channel -> ?byte_order:OBus_wire.byte_order ->
  OBus_message.t -> unit Lwt.t
  (** Write a message to an [Lwt_io] output channel.
      Fails if the message carries file descriptors. *)
