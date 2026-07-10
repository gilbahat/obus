(*
 * oBus_address_unix.ml
 * --------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt.Infix

let section = Lwt_log.Section.make "obus(address)"

let xdg_runtime_dir_variable = "XDG_RUNTIME_DIR"
let session_bus_variable = "DBUS_SESSION_BUS_ADDRESS"
let default_session = [{ OBus_address.name = "autolaunch"; args = [] }]

let xdg_fallback_session () =
  match (try Some (Sys.getenv xdg_runtime_dir_variable) with Not_found -> None) with
    | None ->
        Lwt.return_none
    | Some path ->
        Lwt.catch (fun () ->
          let sock_path = Filename.concat path "bus" in
          let%lwt stat = Lwt_unix.stat sock_path in
          let uid = Unix.getuid () in
          if stat.st_uid = uid && stat.st_kind = Unix.S_SOCK then
            Lwt.return_some [{ OBus_address.name = "unix"; args = [("path", sock_path)] }]
          else
            Lwt.return_none)
          (fun _ -> Lwt.return_none)

let session = lazy(
  match (try Some (Sys.getenv session_bus_variable) with Not_found -> None) with
    | Some line ->
        Lwt.return (OBus_address.of_string line)
    | None ->
        let%lwt () = Lwt_log.info_f ~section
          "environment variable %s not found, trying %s/bus"
          session_bus_variable xdg_runtime_dir_variable
        in
        let%lwt xdg = xdg_fallback_session () in
        match xdg with
          | Some addrs ->
              Lwt.return addrs
          | None ->
              let%lwt () = Lwt_log.info_f ~section
                "failed to connect to %s/bus, trying launchd"
                xdg_runtime_dir_variable
              in
              (try%lwt
                 let%lwt path =
                   Lwt_process.pread_line
                     ("launchctl",
                      [|"launchctl"; "getenv"; "DBUS_LAUNCHD_SESSION_BUS_SOCKET"|])
                 in
                 Lwt.return [{ OBus_address.name = "unix"; args = [("path", path)] }]
               with exn ->
                 let%lwt () = Lwt_log.info_f ~exn ~section
                   "failed to get session bus address from launchd, using internal default"
                 in
                 Lwt.return default_session)
)
