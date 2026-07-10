(*
 * oBus_auth_unix.ml
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Printf
open Lwt.Infix

let section = Lwt_log.Section.make "obus(auth)"

let max_line_length = OBus_auth.max_line_length

(* +-----------------------------------------------------------------+
   | Keyring for the DBUS_COOKIE_SHA1 method                        |
   +-----------------------------------------------------------------+ *)

module Cookie =
struct
  type t = {
    id : int32;
    time : int64;
    cookie : string;
  }

  let id c = c.id
  let time c = c.time
  let cookie c = c.cookie
end

module Keyring : sig
  type context = string
  val load : context -> Cookie.t list Lwt.t
  val save : context -> Cookie.t list -> unit Lwt.t
end = struct

  type context = string

  let keyring_directory = lazy(
    let%lwt homedir = Lazy.force OBus_util_unix.homedir in
    Lwt.return (Filename.concat homedir ".dbus-keyrings")
  )

  let keyring_file_name context =
    let%lwt dir = Lazy.force keyring_directory in
    Lwt.return (Filename.concat dir context)

  let parse_line line =
    Scanf.sscanf line "%ld %Ld %[a-fA-F0-9]"
      (fun id time cookie -> { Cookie.id = id; Cookie.time = time; Cookie.cookie = cookie })

  let print_line cookie =
    sprintf "%ld %Ld %s" (Cookie.id cookie) (Cookie.time cookie) (Cookie.cookie cookie)

  let load context =
    let%lwt fname = keyring_file_name context in
    if Sys.file_exists fname then
      try%lwt
        Lwt_stream.get_while (fun _ -> true)
          (Lwt_stream.map parse_line (Lwt_io.lines_of_file fname))
      with exn ->
        let%lwt fname = keyring_file_name context in
        let%lwt () = Lwt_log.error_f ~exn ~section "failed to load cookie file %s" fname in
        Lwt.fail exn
    else
      Lwt.return []

  let lock_file fname =
    let really_lock () =
      Lwt_unix.openfile fname
        [Unix.O_WRONLY; Unix.O_EXCL; Unix.O_CREAT] 0o600
      >>= Lwt_unix.close
    in
    let rec aux = function
      | 0 ->
          let%lwt () =
            try%lwt
              let%lwt () = Lwt_unix.unlink fname in
              Lwt_log.info_f ~section "stale lock file %s removed" fname
            with Unix.Unix_error(error, _, _) as exn ->
              let%lwt () = Lwt_log.error_f ~section "failed to remove stale lock file %s: %s"
                fname (Unix.error_message error) in
              Lwt.fail exn
          in
          (try%lwt
             really_lock ()
           with Unix.Unix_error(error, _, _) as exn ->
             let%lwt () = Lwt_log.error_f ~section "failed to lock file %s after removing it: %s"
               fname (Unix.error_message error) in
             Lwt.fail exn)
      | n ->
          try%lwt
            really_lock ()
          with exn ->
            let%lwt () = Lwt_log.info_f ~section "waiting for lock file (%d) %s" n fname in
            let%lwt () = Lwt_unix.sleep 0.250 in
            aux (n - 1)
    in
    aux 32

  let unlock_file fname =
    try%lwt
      Lwt_unix.unlink fname
    with Unix.Unix_error(error, _, _) as exn ->
      let%lwt () = Lwt_log.error_f ~section "failed to unlink file %s: %s"
        fname (Unix.error_message error) in
      Lwt.fail exn

  let save context cookies =
    let%lwt fname = keyring_file_name context in
    let tmp_fname = fname ^ "." ^ OBus_util.hex_encode (OBus_util.random_string 8) in
    let lock_fname = fname ^ ".lock" in
    let%lwt dir = Lazy.force keyring_directory in
    let%lwt () =
      if not (Sys.file_exists dir) then begin
        try%lwt
          Lwt_unix.mkdir dir 0o700
        with Unix.Unix_error(error, _, _) as exn ->
          let%lwt () = Lwt_log.error_f ~section
            "failed to create directory %s with permissions 0700: %s"
            dir (Unix.error_message error) in
          Lwt.fail exn
      end else
        Lwt.return ()
    in
    let%lwt () = lock_file lock_fname in begin
      let%lwt () =
        try%lwt
          Lwt_io.lines_to_file tmp_fname
            (Lwt_stream.map print_line (Lwt_stream.of_list cookies))
        with exn ->
          let%lwt () = Lwt_log.error_f ~exn ~section
            "unable to write temporary keyring file %s" tmp_fname in
          Lwt.fail exn
      in
      try
        Lwt_unix.rename tmp_fname fname
      with Unix.Unix_error(error, _, _) as exn ->
        let%lwt () = Lwt_log.error_f ~section "unable to rename file %s to %s: %s"
          tmp_fname fname (Unix.error_message error) in
        Lwt.fail exn
    end
    [%lwt.finally unlock_file lock_fname]
end

(* +-----------------------------------------------------------------+
   | Unix streams                                                    |
   +-----------------------------------------------------------------+ *)

let stream_of_channels (ic, oc) =
  OBus_auth.make_stream
    ~recv:(fun () ->
      let buf = Buffer.create 42 in
      let rec loop last =
        if Buffer.length buf > max_line_length then
          Lwt.fail (OBus_auth.Auth_failure "input: line too long")
        else
          Lwt_io.read_char_opt ic >>= function
            | None ->
                Lwt.fail (OBus_auth.Auth_failure "input: premature end of input")
            | Some ch ->
                Buffer.add_char buf ch;
                if last = '\r' && ch = '\n' then
                  Lwt.return (Buffer.contents buf)
                else
                  loop ch
      in
      loop '\x00')
    ~send:(fun line ->
      let%lwt () = Lwt_io.write oc line in
      Lwt_io.flush oc)

let stream_of_fd fd =
  OBus_auth.make_stream
    ~recv:(fun () ->
      let buf = Buffer.create 42 and tmp = Bytes.create 1 in
      let rec loop last =
        if Buffer.length buf > max_line_length then
          Lwt.fail (OBus_auth.Auth_failure "input: line too long")
        else
          Lwt_unix.read fd tmp 0 1 >>= function
            | 0 ->
                Lwt.fail (OBus_auth.Auth_failure "input: premature end of input")
            | 1 ->
                let ch = Bytes.get tmp 0 in
                Buffer.add_char buf ch;
                if last = '\r' && ch = '\n' then
                  Lwt.return (Buffer.contents buf)
                else
                  loop ch
            | _ ->
                assert false
      in
      loop '\x00')
    ~send:(fun line ->
      let rec loop ofs len =
        if len = 0 then
          Lwt.return ()
        else
          Lwt_unix.write_string fd line ofs len >>= function
            | 0 ->
                Lwt.fail (OBus_auth.Auth_failure "output: zero byte written")
            | n ->
                assert (n > 0 && n <= len);
                loop (ofs + n) (len - n)
      in
      loop 0 (String.length line))

(* +-----------------------------------------------------------------+
   | Unix client mechanisms                                          |
   +-----------------------------------------------------------------+ *)

let mech_external_unix =
  OBus_auth.Client.mech_external ~uid:(string_of_int (Unix.getuid ())) ()

let mech_dbus_cookie_sha1_unix = {
  OBus_auth.Client.mech_name = "DBUS_COOKIE_SHA1";
  mech_exec = (fun () ->
    let uid = string_of_int (Unix.getuid ()) in
    object
      inherit OBus_auth.Client.mechanism_handler
      method init = Lwt.return (OBus_auth.Client.Mech_continue uid)
      method data chal =
        let context, id, server_rand =
          Scanf.sscanf chal "%[^/\\ \n\r.] %ld %[a-fA-F0-9]%!"
            (fun ctx i r -> (ctx, i, r))
        in
        let%lwt keyring = Keyring.load context in
        let cookie =
          try List.find (fun c -> c.Cookie.id = id) keyring
          with Not_found ->
            ksprintf failwith "cookie %ld not found in context %S" id context
        in
        let client_rand = OBus_util.hex_encode (OBus_util.random_string 16) in
        let resp =
          sprintf "%s %s" client_rand
            (OBus_util.hex_encode
               (OBus_util.sha_1
                  (sprintf "%s:%s:%s" server_rand client_rand cookie.Cookie.cookie)))
        in
        Lwt.return (OBus_auth.Client.Mech_ok resp)
      method abort = ()
    end);
}

let default_mechanisms_unix =
  [mech_external_unix; mech_dbus_cookie_sha1_unix; OBus_auth.Client.mech_anonymous]
