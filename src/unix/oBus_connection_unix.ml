(*
 * oBus_connection_unix.ml
 * -----------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt.Infix

let capabilities = [`Unix_fd]

let of_addresses ?switch ?(shared=true) addresses =
  Lwt_switch.check switch;
  let%lwt guid, transport =
    OBus_transport_unix.of_addresses ?switch ~capabilities addresses
  in
  match shared with
    | false ->
        Lwt.return (OBus_connection.of_transport ?switch transport)
    | true ->
        Lwt.return (OBus_connection.of_transport ?switch ~guid transport)
