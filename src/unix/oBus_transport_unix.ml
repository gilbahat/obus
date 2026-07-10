(*
 * oBus_transport_unix.ml
 * ----------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Unix
open Lwt.Infix

let section = Lwt_log.Section.make "obus(transport)"

(* +-----------------------------------------------------------------+
   | Socket → OBus_transport.connection                              |
   +-----------------------------------------------------------------+ *)

let connection_of_fd fd =
  let ic = Lwt_io.of_fd ~mode:Lwt_io.input  ~close:Lwt.return fd
  and oc = Lwt_io.of_fd ~mode:Lwt_io.output ~close:Lwt.return fd in
  {
    OBus_transport.read = (fun n ->
      let buf = Bytes.create n in
      let%lwt () = Lwt_io.read_into_exactly ic buf 0 n in
      Lwt.return (Bytes.unsafe_to_string buf));
    write = (fun s -> Lwt_io.write oc s);
    close = (fun () ->
      let%lwt () = Lwt_io.close ic <&> Lwt_io.close oc in
      Lwt_unix.shutdown fd SHUTDOWN_ALL;
      Lwt_unix.close fd);
  }

(* +-----------------------------------------------------------------+
   | socket_of_fd: transport from a connected Unix fd                |
   +-----------------------------------------------------------------+ *)

(* Creates a transport that supports Unix fd-passing when `Unix_fd is
   in capabilities, otherwise falls back to plain byte-stream I/O. *)
let socket_of_fd ?switch ?(capabilities=[]) fd =
  if List.mem `Unix_fd capabilities then begin
    let r = OBus_wire_unix.reader fd
    and w = OBus_wire_unix.writer fd in
    OBus_transport.make ?switch
      ~recv:(fun _ -> OBus_wire_unix.read_message_with_fds r)
      ~send:(fun msg -> OBus_wire_unix.write_message_with_fds w msg)
      ~capabilities
      ~shutdown:(fun _ ->
        let%lwt () =
          OBus_wire_unix.close_reader r <&> OBus_wire_unix.close_writer w
        in
        Lwt_unix.shutdown fd SHUTDOWN_ALL;
        Lwt_unix.close fd)
      ()
  end else begin
    let conn = connection_of_fd fd in
    OBus_transport.of_connection ?switch ~capabilities conn
  end

(* +-----------------------------------------------------------------+
   | Low-level socket creation                                       |
   +-----------------------------------------------------------------+ *)

let make_socket domain typ addr =
  let fd = Lwt_unix.socket domain typ 0 in
  (try Lwt_unix.set_close_on_exec fd with _ -> ());
  try%lwt
    let%lwt () = Lwt_unix.connect fd addr in
    Lwt.return (fd, domain)
  with exn ->
    let%lwt () = Lwt_unix.close fd in
    Lwt.fail exn

let rec write_nonce fd nonce pos len =
  Lwt_unix.write_string fd nonce pos len >>= function
    | 0 ->
        Lwt.fail (Failure "OBus_transport_unix.connect: failed to send the nonce to the server")
    | n ->
        if n = len then
          Lwt.return ()
        else
          write_nonce fd nonce (pos + n) (len - n)

let make_socket_nonce nonce_file domain typ addr =
  match nonce_file with
    | None ->
        Lwt.fail (Invalid_argument "OBus_transport_unix.connect: missing 'noncefile' parameter")
    | Some file_name ->
        let%lwt nonce =
          try%lwt
            Lwt_io.with_file ~mode:Lwt_io.input file_name (Lwt_io.read ~count:16)
          with
            | Unix.Unix_error(err, _, _) ->
                Lwt.fail (Failure(Printf.sprintf
                  "failed to read the nonce file '%s': %s"
                  file_name (Unix.error_message err)))
            | End_of_file ->
                Lwt.fail (Failure(Printf.sprintf
                  "OBus_transport_unix.connect: '%s' is an invalid nonce-file"
                  file_name))
        in
        if String.length nonce <> 16 then
          Lwt.fail (Failure(Printf.sprintf
            "OBus_transport_unix.connect: '%s' is an invalid nonce-file" file_name))
        else begin
          let%lwt fd, domain = make_socket domain typ addr in
          let%lwt () = write_nonce fd nonce 0 16 in
          Lwt.return (fd, domain)
        end

let rec connect_address address =
  match OBus_address.name address with
    | "unix" -> begin
        match (OBus_address.arg "path" address,
               OBus_address.arg "abstract" address,
               OBus_address.arg "tmpdir" address) with
          | Some path, None, None ->
              make_socket PF_UNIX SOCK_STREAM (ADDR_UNIX path)
          | None, Some abst, None ->
              make_socket PF_UNIX SOCK_STREAM (ADDR_UNIX("\x00" ^ abst))
          | None, None, Some _ ->
              Lwt.fail (Invalid_argument
                "OBus_transport_unix.connect: unix tmpdir can only be used as a listening address")
          | _ ->
              Lwt.fail (Invalid_argument
                "OBus_transport_unix.connect: invalid unix address, \
                 must supply exactly one of 'path', 'abstract', 'tmpdir'")
      end
    | ("tcp" | "nonce-tcp") as name -> begin
        let host = Option.value (OBus_address.arg "host" address) ~default:""
        and port = Option.value (OBus_address.arg "port" address) ~default:"0" in
        let opts = [AI_SOCKTYPE SOCK_STREAM] in
        let opts = match OBus_address.arg "family" address with
          | Some "ipv4" -> AI_FAMILY PF_INET  :: opts
          | Some "ipv6" -> AI_FAMILY PF_INET6 :: opts
          | Some f ->
              Printf.ksprintf invalid_arg
                "OBus_transport_unix.connect: unknown address family '%s'" f
          | None -> opts
        in
        Lwt_unix.getaddrinfo host port opts >>= function
          | [] ->
              Lwt.fail (Failure (Printf.sprintf
                "OBus_transport_unix.connect: no address info for host=%s port=%s%s"
                host port
                (match OBus_address.arg "family" address with
                   | None -> ""
                   | Some f -> " family=" ^ f)))
          | ai :: rest ->
              let try_connect =
                if name = "nonce-tcp" then
                  make_socket_nonce (OBus_address.arg "noncefile" address)
                else
                  make_socket
              in
              (try%lwt
                 try_connect ai.ai_family ai.ai_socktype ai.ai_addr
               with exn ->
                 let rec find = function
                   | [] -> Lwt.fail exn
                   | ai :: ais ->
                       (try%lwt
                          try_connect ai.ai_family ai.ai_socktype ai.ai_addr
                        with _ -> find ais)
                 in
                 find rest)
      end
    | "launchd" -> begin
        match OBus_address.arg "env" address with
          | Some env ->
              let%lwt path =
                try%lwt
                  Lwt_process.pread_line ("launchctl", [|"launchctl"; "getenv"; env|])
                with exn ->
                  let%lwt () = Lwt_log.error_f ~exn ~section "launchctl failed" in
                  Lwt.fail exn
              in
              make_socket PF_UNIX SOCK_STREAM (ADDR_UNIX path)
          | None ->
              Lwt.fail (Invalid_argument
                "OBus_transport_unix.connect: missing 'env' in launchd address")
      end
    | "autolaunch" ->
        let%lwt uuid = Lazy.force OBus_info_unix.machine_uuid in
        let%lwt line =
          try%lwt
            Lwt_process.pread_line
              ("dbus-launch",
               [|"dbus-launch"; "--autolaunch"; OBus_uuid.to_string uuid; "--binary-syntax"|])
          with exn ->
            let%lwt () = Lwt_log.error_f ~exn ~section "autolaunch failed" in
            Lwt.fail exn
        in
        let line = try String.sub line 0 (String.index line '\000') with _ -> line in
        let addresses =
          try OBus_address.of_string line
          with OBus_address.Parse_failure(addr, pos, reason) as exn ->
            ignore (Lwt_log.error_f ~section
              "autolaunch returned an invalid address %S, at position %d: %s"
              addr pos reason);
            raise exn
        in
        (match addresses with
           | [] ->
               Lwt.fail (Failure "'autolaunch' returned no addresses")
           | addr :: rest ->
               (try%lwt
                  connect_address addr
                with exn ->
                  let rec find = function
                    | [] -> Lwt.fail exn
                    | a :: rest ->
                        (try%lwt connect_address a with _ -> find rest)
                  in
                  find rest))
    | name ->
        Lwt.fail (Failure ("unknown transport type: " ^ name))

(* +-----------------------------------------------------------------+
   | of_addresses                                                    |
   +-----------------------------------------------------------------+ *)

let of_addresses ?switch ?(capabilities=OBus_auth.capabilities) ?mechanisms addresses =
  Lwt_switch.check switch;
  match addresses with
    | [] ->
        Lwt.fail (Invalid_argument "OBus_transport_unix.of_addresses: no address given")
    | first :: rest ->
        let%lwt fd, domain =
          try%lwt
            connect_address first
          with exn ->
            let rec find = function
              | [] -> Lwt.fail exn
              | addr :: rest ->
                  (try%lwt connect_address addr with _ -> find rest)
            in
            find rest
        in
        try%lwt
          Lwt_unix.write_string fd "\x00" 0 1 >>= function
            | 0 ->
                Lwt.fail (OBus_auth.Auth_failure
                  "failed to send the initial null byte")
            | 1 ->
                let unix_fd_ok = (domain = PF_UNIX) in
                let%lwt guid, caps =
                  OBus_auth.Client.authenticate
                    ~capabilities:(List.filter
                      (function `Unix_fd -> unix_fd_ok) capabilities)
                    ?mechanisms
                    ~stream:(OBus_auth_unix.stream_of_fd fd)
                    ()
                in
                let transport = socket_of_fd ?switch ~capabilities:caps fd in
                Lwt.return (guid, transport)
            | _ ->
                assert false
        with exn ->
          Lwt_unix.shutdown fd SHUTDOWN_ALL;
          let%lwt () = Lwt_unix.close fd in
          Lwt.fail exn
