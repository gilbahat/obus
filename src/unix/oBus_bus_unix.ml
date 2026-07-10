(*
 * oBus_bus_unix.ml
 * ----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt_react
open Lwt.Infix

let section = Lwt_log.Section.make "obus(bus)"

let exit_on_disconnect exn =
  ignore (Lwt_log.error_f ~section ~exn "the D-Bus connection was lost");
  exit 1

let of_addresses ?switch addresses =
  let%lwt bus = OBus_connection_unix.of_addresses ?switch addresses ~shared:true in
  let%lwt () = OBus_bus.register_connection bus in
  Lwt.return bus

(* +-----------------------------------------------------------------+
   | Session bus                                                     |
   +-----------------------------------------------------------------+ *)

let session_bus = lazy(
  try%lwt
    let%lwt addrs = Lazy.force OBus_address_unix.session in
    let%lwt bus = of_addresses addrs in
    OBus_connection.set_on_disconnect bus exit_on_disconnect;
    Lwt.return bus
  with exn ->
    let%lwt () = Lwt_log.warning ~exn ~section
      "Failed to open a connection to the session bus" in
    Lwt.fail exn
)

let session ?switch () =
  Lwt_switch.check switch;
  let%lwt bus = Lazy.force session_bus in
  let%lwt () = Lwt_switch.add_hook_or_exec switch (fun () -> OBus_connection.close bus) in
  Lwt.return bus

(* +-----------------------------------------------------------------+
   | System bus                                                      |
   +-----------------------------------------------------------------+ *)

let system_bus_state = ref None
let system_bus_mutex = Lwt_mutex.create ()

let system ?switch () =
  Lwt_switch.check switch;
  let%lwt bus =
    Lwt_mutex.with_lock system_bus_mutex
      (fun () ->
         match !system_bus_state with
           | Some bus when S.value (OBus_connection.active bus) ->
               Lwt.return bus
           | _ ->
               try%lwt
                 let%lwt addrs = Lazy.force OBus_address.system in
                 let%lwt bus = of_addresses addrs in
                 system_bus_state := Some bus;
                 Lwt.return bus
               with exn ->
                 let%lwt () = Lwt_log.warning ~exn ~section
                   "Failed to open a connection to the system bus" in
                 Lwt.fail exn)
  in
  let%lwt () = Lwt_switch.add_hook_or_exec switch (fun () -> OBus_connection.close bus) in
  Lwt.return bus
