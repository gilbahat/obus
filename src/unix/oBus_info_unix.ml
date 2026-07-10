(*
 * oBus_info_unix.ml
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt.Infix

let section = Lwt_log.Section.make "obus(info)"

let read_uuid_file file =
  try%lwt
    let%lwt line = Lwt_io.with_file ~mode:Lwt_io.input file Lwt_io.read_line in
    Lwt.return (OBus_uuid.of_string line)
  with exn ->
    let%lwt () = Lwt_log.error_f ~section ~exn
      "failed to read the local machine uuid from file %S" file in
    Lwt.fail exn

(* Try the D-Bus machine-id file, then the systemd one. *)
let machine_uuid = lazy(
  try%lwt
    read_uuid_file OBus_config.machine_uuid_file
  with _ ->
    try%lwt
      read_uuid_file "/etc/machine-id"
    with exn ->
      let%lwt () = Lwt_log.warning ~section ~exn
        "could not read machine UUID from disk; generating a transient one" in
      Lwt.return (OBus_uuid.generate ())
)
