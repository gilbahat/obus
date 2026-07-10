(*
 * oBus_util_unix.mli
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

val homedir : string Lwt.t Lazy.t
  (** The home directory, determined from [$HOME] or [getpwuid]. *)

val fill_random : bytes -> int -> int -> unit
  (** [fill_random buf ofs len] fills [buf[ofs..ofs+len-1]] with bytes
      from [/dev/urandom], falling back to the pseudo-random generator. *)
