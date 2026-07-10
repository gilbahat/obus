(*
 * oBus_info.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

module Lwt_log = Lwt_log_core

let version = OBus_config.version

let protocol_version = 1
let max_name_length = OBus_protocol.max_name_length
let max_message_size = OBus_protocol.max_message_size

let machine_uuid = lazy(Lwt.return (OBus_uuid.generate ()))
