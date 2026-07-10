(*
 * oBus_transport.ml
 * -----------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

module Lwt_log = Lwt_log_core
let section = Lwt_log.Section.make "obus(transport)"

open Lwt.Infix

(* +-----------------------------------------------------------------+
   | Raw byte-stream connection                                      |
   +-----------------------------------------------------------------+ *)

type connection = {
  read : int -> string Lwt.t;
  (* [read n] reads exactly [n] bytes; raises End_of_file at EOF *)
  write : string -> unit Lwt.t;
  (* [write s] sends the full string [s] *)
  close : unit -> unit Lwt.t;
}

(* +-----------------------------------------------------------------+
   | Message transport                                               |
   +-----------------------------------------------------------------+ *)

type t = {
  recv : unit -> OBus_message.t Lwt.t;
  send : OBus_message.t -> unit Lwt.t;
  capabilities : OBus_auth.capability list;
  shutdown : unit -> unit Lwt.t;
}

let make ?switch ~recv ~send ?(capabilities=[]) ~shutdown () =
  let transport = {
    recv = recv;
    send = send;
    capabilities = capabilities;
    shutdown = shutdown;
  } in
  Lwt_switch.add_hook switch transport.shutdown;
  transport

let recv t = t.recv ()
let send t message = t.send message
let capabilities t = t.capabilities
let shutdown t = t.shutdown ()

(* +-----------------------------------------------------------------+
   | Connection-based transport (Mirage-compatible)                  |
   +-----------------------------------------------------------------+ *)

let of_connection ?switch ?(capabilities=[]) conn =
  let transport = {
    recv = (fun _ -> OBus_wire.read_message conn.read);
    send = (fun msg -> OBus_wire.write_message conn.write msg);
    capabilities = capabilities;
    shutdown = (fun _ -> conn.close ());
  } in
  Lwt_switch.add_hook switch transport.shutdown;
  transport

let connect ?switch ?(capabilities=OBus_auth.capabilities) ?mechanisms conn =
  try%lwt
    (* Send initial null byte required by D-Bus spec *)
    let%lwt () = conn.write "\x00" in
    let recv_byte () =
      conn.read 1 >>= fun s ->
      if String.length s = 0 then Lwt.fail End_of_file
      else Lwt.return s.[0]
    in
    let send_line line = conn.write line in
    let%lwt guid, caps =
      OBus_auth.Client.authenticate
        ~capabilities:(List.filter (function `Unix_fd -> false) capabilities)
        ?mechanisms
        ~stream:(OBus_auth.stream_of_fns ~recv_byte ~send:send_line)
        ()
    in
    Lwt.return (guid, of_connection ?switch ~capabilities:caps conn)
  with exn ->
    let%lwt () = conn.close () in
    Lwt.fail exn

(* +-----------------------------------------------------------------+
   | Loopback transport                                              |
   +-----------------------------------------------------------------+ *)

let loopback () =
  let mvar = Lwt_mvar.create_empty () in
  { recv = (fun _ -> Lwt_mvar.take mvar);
    send = (fun m -> Lwt_mvar.put mvar { m with OBus_message.body = OBus_value.V.sequence_dup (OBus_message.body m) });
    capabilities = [`Unix_fd];
    shutdown = Lwt.return }
