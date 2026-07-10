(*
 * oBus_auth_unix.mli
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(** Unix-specific D-Bus authentication helpers *)

(** {6 Cookie keyring} *)

module Cookie : sig
  type t
  val id : t -> int32
  val time : t -> int64
  val cookie : t -> string
end

module Keyring : sig
  type context = string
  val load : context -> Cookie.t list Lwt.t
  val save : context -> Cookie.t list -> unit Lwt.t
end

(** {6 Unix auth streams} *)

val stream_of_channels :
  Lwt_io.input_channel * Lwt_io.output_channel -> OBus_auth.stream
  (** Build an auth stream from a pair of [Lwt_io] channels. *)

val stream_of_fd : Lwt_unix.file_descr -> OBus_auth.stream
  (** Build an auth stream directly from a Unix file descriptor. *)

(** {6 Unix client mechanisms} *)

val mech_external_unix : OBus_auth.Client.mechanism
  (** [EXTERNAL] mechanism using the current process's UID. *)

val mech_dbus_cookie_sha1_unix : OBus_auth.Client.mechanism
  (** [DBUS_COOKIE_SHA1] mechanism backed by the on-disk keyring. *)

val default_mechanisms_unix : OBus_auth.Client.mechanism list
  (** [EXTERNAL; DBUS_COOKIE_SHA1; ANONYMOUS] — the standard Unix set. *)
