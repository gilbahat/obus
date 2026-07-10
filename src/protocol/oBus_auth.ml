(*
 * oBus_auth.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

module Lwt_log = Lwt_log_core
let section = Lwt_log.Section.make "obus(auth)"

open Printf
open Lwt.Infix

type capability = [ `Unix_fd ]

let capabilities = [`Unix_fd]

(* Maximum line length, if line greated are received, authentication
   will fail *)
let max_line_length = 42 * 1024

(* Maximum number of reject, if a client is rejected more than that,
   authentication will fail *)
let max_reject = 42

exception Auth_failure of string
let auth_failure fmt = ksprintf (fun msg -> Lwt.fail (Auth_failure msg)) fmt

let () =
  Printexc.register_printer
    (function
       | Auth_failure msg ->
           Some(Printf.sprintf "D-Bus authentication failed: %s" msg)
       | _ ->
           None)

let hex_encode = OBus_util.hex_encode
let hex_decode str =
  try
    OBus_util.hex_decode str
  with
    | Invalid_argument _ -> failwith "invalid hex-encoded data"

type data = string

type client_command =
  | Client_auth of (string * data option) option
  | Client_cancel
  | Client_begin
  | Client_data of data
  | Client_error of string
  | Client_negotiate_unix_fd

type server_command =
  | Server_rejected of string list
  | Server_ok of OBus_address.guid
  | Server_data of data
  | Server_error of string
  | Server_agree_unix_fd

(* +-----------------------------------------------------------------+
   | Communication                                                   |
   +-----------------------------------------------------------------+ *)

type stream = {
  recv : unit -> string Lwt.t;
  send : string -> unit Lwt.t;
}

let make_stream ~recv ~send = {
  recv = (fun () ->
            try%lwt
              recv ()
            with
              | Auth_failure _ as exn ->
                  Lwt.fail exn
              | End_of_file ->
                  Lwt.fail (Auth_failure("input: premature end of input"))
              | exn ->
                  Lwt.fail (Auth_failure("input: " ^ Printexc.to_string exn)));
  send = (fun line ->
            try%lwt
              send line
            with
              | Auth_failure _ as exn ->
                  Lwt.fail exn
              | exn ->
                  Lwt.fail (Auth_failure("output: " ^ Printexc.to_string exn)));
}

let stream_of_fns ~recv_byte ~send =
  make_stream
    ~recv:(fun () ->
             let buf = Buffer.create 42 in
             let rec loop last =
               if Buffer.length buf > max_line_length then
                 Lwt.fail (Auth_failure "input: line too long")
               else
                 recv_byte () >>= fun ch ->
                 Buffer.add_char buf ch;
                 if last = '\r' && ch = '\n' then
                   Lwt.return (Buffer.contents buf)
                 else
                   loop ch
             in
             loop '\x00')
    ~send

let send_line mode stream line =
  ignore (Lwt_log.debug_f ~section "%s: sending: %S" mode line);
  stream.send (line ^ "\r\n")

let rec recv_line stream =
  let%lwt line = stream.recv () in
  let len = String.length line in
  if len < 2 || not (line.[len - 2] = '\r' && line.[len - 1] = '\n') then
    Lwt.fail (Auth_failure("input: invalid line received"))
  else
    Lwt.return (String.sub line 0 (len - 2))

let rec first f str pos =
  if pos = String.length str then
    pos
  else match f str.[pos] with
    | true -> pos
    | false -> first f str (pos + 1)

let rec last f str pos =
  if pos = 0 then
    pos
  else match f str.[pos - 1] with
    | true -> pos
    | false -> first f str (pos - 1)

let blank ch = ch = ' ' || ch = '\t'
let not_blank ch = not (blank ch)

let sub_strip str i j =
  let i = first not_blank str i in
  let j = last not_blank str j in
  if i < j then String.sub str i (j - i) else ""

let split str =
  let rec aux i =
    let i = first not_blank str i in
    if i = String.length str then
      []
    else
      let j = first blank str i in
      String.sub str i (j - i) :: aux j
  in
  aux 0

let preprocess_line line =
  (* Check for ascii-only *)
  String.iter (function
                 | '\x01'..'\x7f' -> ()
                 | _ -> failwith "non-ascii characters in command") line;
  (* Extract the command *)
  let i = first blank line 0 in
  if i = 0 then failwith "empty command";
  (String.sub line 0 i, sub_strip line i (String.length line))

let rec recv mode command_parser stream =
  let%lwt line = recv_line stream in
  let%lwt () = Lwt_log.debug_f ~section "%s: received: %S" mode line in

  (* If a parse failure occur, return an error and try again *)
  match
    try
      let command, args = preprocess_line line in
      `Success(command_parser command args)
    with exn ->
      `Failure(exn)
  with
    | `Success x -> Lwt.return x
    | `Failure(Failure msg) ->
        let%lwt () = send_line mode stream ("ERROR \"" ^ msg ^ "\"") in
        recv mode command_parser stream
    | `Failure exn -> Lwt.fail exn

let client_recv = recv "client"
  (fun command args -> match command with
     | "REJECTED" -> Server_rejected (split args)
     | "OK" -> Server_ok(try OBus_uuid.of_string args with _ -> failwith "invalid hex-encoded guid")
     | "DATA" -> Server_data(hex_decode args)
     | "ERROR" -> Server_error args
     | "AGREE_UNIX_FD" -> Server_agree_unix_fd
     | _ -> failwith "invalid command")

let server_recv = recv "server"
  (fun command args -> match command with
     | "AUTH" -> Client_auth(match split args with
                               | [] -> None
                               | [mech] -> Some(mech, None)
                               | [mech; data] -> Some(mech, Some(hex_decode data))
                               | _ -> failwith "too many arguments")
     | "CANCEL" -> Client_cancel
     | "BEGIN" -> Client_begin
     | "DATA" -> Client_data(hex_decode args)
     | "ERROR" -> Client_error args
     | "NEGOTIATE_UNIX_FD" -> Client_negotiate_unix_fd
     | _ -> failwith "invalid command")

let client_send chans cmd = send_line "client" chans
  (match cmd with
     | Client_auth None -> "AUTH"
     | Client_auth(Some(mechanism, None)) -> sprintf "AUTH %s" mechanism
     | Client_auth(Some(mechanism, Some data)) -> sprintf "AUTH %s %s" mechanism (hex_encode data)
     | Client_cancel -> "CANCEL"
     | Client_begin -> "BEGIN"
     | Client_data data -> sprintf "DATA %s" (hex_encode data)
     | Client_error msg -> sprintf "ERROR \"%s\"" msg
     | Client_negotiate_unix_fd -> "NEGOTIATE_UNIX_FD")

let server_send chans cmd = send_line "server" chans
  (match cmd with
     | Server_rejected mechs -> String.concat " " ("REJECTED" :: mechs)
     | Server_ok guid -> sprintf "OK %s" (OBus_uuid.to_string guid)
     | Server_data data -> sprintf "DATA %s" (hex_encode data)
     | Server_error msg -> sprintf "ERROR \"%s\"" msg
     | Server_agree_unix_fd -> "AGREE_UNIX_FD")

(* +-----------------------------------------------------------------+
   | Client side authentication                                      |
   +-----------------------------------------------------------------+ *)

module Client =
struct

  type mechanism_return =
    | Mech_continue of data
    | Mech_ok of data
    | Mech_error of string

  class virtual mechanism_handler = object
    method virtual init : mechanism_return Lwt.t
    method data (chall : data) = Lwt.return (Mech_error("no data expected for this mechanism"))
    method abort = ()
  end

  type mechanism = {
    mech_name : string;
    mech_exec : unit -> mechanism_handler;
  }

  let mech_name m = m.mech_name
  let mech_exec m = m.mech_exec

  (* +---------------------------------------------------------------+
     | Predefined client mechanisms                                  |
     +---------------------------------------------------------------+ *)

  class mech_external_handler uid = object
    inherit mechanism_handler
    method init = Lwt.return (Mech_ok uid)
  end

  class mech_anonymous_handler = object
    inherit mechanism_handler
    method init = Lwt.return (Mech_ok("obus " ^ OBus_info.version))
  end

  class mech_dbus_cookie_sha1_handler ~uid ~cookie = object
    inherit mechanism_handler
    method init = Lwt.return (Mech_continue uid)
    method data chal =
      let%lwt () = Lwt_log.debug_f ~section "client: dbus_cookie_sha1: chal: %s" chal in
      let _context, _id, server_rand =
        Scanf.sscanf chal "%[^/\\ \n\r.] %ld %[a-fA-F0-9]%!"
          (fun context id r -> (context, id, r))
      in
      let client_rand = hex_encode (OBus_util.random_string 16) in
      let resp = sprintf "%s %s" client_rand
        (hex_encode (OBus_util.sha_1 (sprintf "%s:%s:%s" server_rand client_rand cookie))) in
      let%lwt () = Lwt_log.debug_f ~section "client: dbus_cookie_sha1: resp: %s" resp in
      Lwt.return (Mech_ok resp)
    method abort = ()
  end

  let mech_external ?(uid="") () = {
    mech_name = "EXTERNAL";
    mech_exec = (fun () -> new mech_external_handler uid);
  }
  let mech_anonymous = {
    mech_name = "ANONYMOUS";
    mech_exec = (fun () -> new mech_anonymous_handler);
  }
  let mech_dbus_cookie_sha1 ?(uid="") ~cookie () = {
    mech_name = "DBUS_COOKIE_SHA1";
    mech_exec = (fun () -> new mech_dbus_cookie_sha1_handler ~uid ~cookie);
  }

  let default_mechanisms = [mech_external (); mech_anonymous]

  (* +---------------------------------------------------------------+
     | Client-side protocol                                          |
     +---------------------------------------------------------------+ *)

  type state =
    | Waiting_for_data of mechanism_handler
    | Waiting_for_ok
    | Waiting_for_reject

  type transition =
    | Transition of client_command * state * mechanism list
    | Success of OBus_address.guid
    | Failure

  (* Try to find a mechanism that can be initialised *)
  let find_working_mech implemented_mechanisms available_mechanisms =
    let rec aux = function
      | [] ->
          Lwt.return Failure
      | { mech_name = name; mech_exec =  f } :: mechs ->
          match available_mechanisms with
            | Some l when not (List.mem name l) ->
                aux mechs
            | _ ->
                let mech = f () in
                try%lwt
                  mech#init >>= function
                    | Mech_continue resp ->
                        Lwt.return (Transition(Client_auth(Some (name, Some resp)),
                                               Waiting_for_data mech,
                                               mechs))
                    | Mech_ok resp ->
                        Lwt.return (Transition(Client_auth(Some (name, Some resp)),
                                               Waiting_for_ok,
                                               mechs))
                    | Mech_error msg ->
                        aux mechs
                with exn ->
                  aux mechs
    in
    aux implemented_mechanisms

  let initial mechs = find_working_mech mechs None
  let next mechs available = find_working_mech mechs (Some available)

  let transition mechs state cmd = match state with
    | Waiting_for_data mech -> begin match cmd with
        | Server_data chal ->
            begin
              try%lwt
                mech#data chal >>= function
                  | Mech_continue resp ->
                      Lwt.return (Transition(Client_data resp,
                                             Waiting_for_data mech,
                                             mechs))
                  | Mech_ok resp ->
                      Lwt.return (Transition(Client_data resp,
                                             Waiting_for_ok,
                                             mechs))
                  | Mech_error msg ->
                      Lwt.return (Transition(Client_error msg,
                                             Waiting_for_data mech,
                                             mechs))
              with exn ->
                Lwt.return (Transition(Client_error(Printexc.to_string exn),
                                       Waiting_for_data mech,
                                       mechs))
            end
        | Server_rejected am ->
            mech#abort;
            next mechs am
        | Server_error _ ->
            mech#abort;
            Lwt.return (Transition(Client_cancel,
                                   Waiting_for_reject,
                                   mechs))
        | Server_ok guid ->
            mech#abort;
            Lwt.return (Success guid)
        | Server_agree_unix_fd ->
            mech#abort;
            Lwt.return (Transition(Client_error "command not expected here",
                                   Waiting_for_data mech,
                                   mechs))
      end

    | Waiting_for_ok -> begin match cmd with
        | Server_ok guid ->
            Lwt.return (Success guid)
        | Server_rejected am ->
            next mechs am
        | Server_data _
        | Server_error _ ->
            Lwt.return (Transition(Client_cancel,
                                   Waiting_for_reject,
                                   mechs))
        | Server_agree_unix_fd ->
            Lwt.return (Transition(Client_error "command not expected here",
                                   Waiting_for_ok,
                                   mechs))
      end

    | Waiting_for_reject -> begin match cmd with
        | Server_rejected am -> next mechs am
        | _ -> Lwt.return Failure
      end

  let authenticate ?(capabilities=[]) ?(mechanisms=default_mechanisms) ~stream () =
    let rec loop = function
      | Transition(cmd, state, mechs) ->
          let%lwt () = client_send stream cmd in
          let%lwt cmd = client_recv stream in
          transition mechs state cmd >>= loop
      | Success guid ->
          let%lwt caps =
            if List.mem `Unix_fd capabilities then
              let%lwt () = client_send stream Client_negotiate_unix_fd in
              client_recv stream >>= function
                | Server_agree_unix_fd ->
                    Lwt.return [`Unix_fd]
                | Server_error _ ->
                    Lwt.return []
                | _ ->
                    (* This case is not covered by the
                       specification *)
                    Lwt.return []
            else
              Lwt.return []
          in
          let%lwt () = client_send stream Client_begin in
          Lwt.return (guid, caps)
      | Failure ->
          auth_failure "authentication failure"
    in
    initial mechanisms >>= loop
end

(* +-----------------------------------------------------------------+
   | Server-side authentication                                      |
   +-----------------------------------------------------------------+ *)

module Server =
struct

  type mechanism_return =
    | Mech_continue of data
    | Mech_ok of int option
    | Mech_reject

  class virtual mechanism_handler = object
    method init = Lwt.return (None : data option)
    method virtual data : data -> mechanism_return Lwt.t
    method abort = ()
  end

  type mechanism = {
    mech_name : string;
    mech_exec : int option -> mechanism_handler;
  }

  let mech_name m = m.mech_name
  let mech_exec m = m.mech_exec

  (* +---------------------------------------------------------------+
     | Predefined server mechanisms                                  |
     +---------------------------------------------------------------+ *)

  class mech_external_handler user_id = object
    inherit mechanism_handler
    method data data =
      match user_id, try Some(int_of_string data) with _ -> None with
        | Some user_id, Some user_id' when user_id = user_id' ->
            Lwt.return (Mech_ok(Some user_id))
        | _ ->
            Lwt.return Mech_reject
  end

  class mech_anonymous_handler = object
    inherit mechanism_handler
    method data _ = Lwt.return (Mech_ok None)
  end

  class mech_dbus_cookie_sha1_handler ~context ~id ~cookie = object
    inherit mechanism_handler

    val mutable state = `State1
    val mutable user_id = None

    method data resp =
      try%lwt
        let%lwt () = Lwt_log.debug_f ~section "server: dbus_cookie_sha1: resp: %s" resp in
        match state with
          | `State1 ->
              user_id <- (try Some(int_of_string resp) with _ -> None);
              let rand = hex_encode (OBus_util.random_string 16) in
              let chal = sprintf "%s %ld %s" context id rand in
              let%lwt () = Lwt_log.debug_f ~section "server: dbus_cookie_sha1: chal: %s" chal in
              state <- `State2(cookie, rand);
              Lwt.return (Mech_continue chal)

          | `State2(cookie, my_rand) ->
              Scanf.sscanf resp "%s %s"
                (fun its_rand comp_sha1 ->
                   if OBus_util.sha_1 (sprintf "%s:%s:%s" my_rand its_rand cookie) = hex_decode comp_sha1 then
                     Lwt.return (Mech_ok user_id)
                   else
                     Lwt.return Mech_reject)

      with _ ->
        Lwt.return Mech_reject

    method abort = ()
  end

  let mech_anonymous = {
    mech_name = "ANONYMOUS";
    mech_exec = (fun _uid -> new mech_anonymous_handler);
  }
  let mech_external = {
    mech_name = "EXTERNAL";
    mech_exec = (fun uid -> new mech_external_handler uid);
  }
  let mech_dbus_cookie_sha1 ?(context="org_freedesktop_general") ~id ~cookie () = {
    mech_name = "DBUS_COOKIE_SHA1";
    mech_exec = (fun _uid -> new mech_dbus_cookie_sha1_handler ~context ~id ~cookie);
  }

  let default_mechanisms = [mech_external; mech_anonymous]

  (* +---------------------------------------------------------------+
     | Server-side protocol                                          |
     +---------------------------------------------------------------+ *)

  type state =
    | Waiting_for_auth
    | Waiting_for_data of mechanism_handler
    | Waiting_for_begin of int option * capability list

  type server_machine_transition =
    | Transition of server_command * state
    | Accept of int option * capability list
    | Failure

  let reject mechs =
    Lwt.return (Transition(Server_rejected (List.map mech_name mechs),
                           Waiting_for_auth))

  let error msg =
    Lwt.return (Transition(Server_error msg,
                           Waiting_for_auth))

  let transition user_id guid capabilities mechs state cmd = match state with
    | Waiting_for_auth -> begin match cmd with
        | Client_auth None ->
            reject mechs
        | Client_auth(Some(name, resp)) ->
            begin match OBus_util.find_map (fun m -> if m.mech_name = name then Some m.mech_exec else None) mechs with
              | None ->
                  reject mechs
              | Some f ->
                  let mech = f user_id in
                  try%lwt
                    let%lwt init = mech#init in
                    match init, resp with
                      | None, None ->
                          Lwt.return (Transition(Server_data "",
                                                 Waiting_for_data mech))
                      | Some chal, None ->
                          Lwt.return (Transition(Server_data chal,
                                                 Waiting_for_data mech))
                      | Some chal, Some rest ->
                          reject mechs
                      | None, Some resp ->
                          mech#data resp >>= function
                            | Mech_continue chal ->
                                Lwt.return (Transition(Server_data chal,
                                                       Waiting_for_data mech))
                            | Mech_ok uid ->
                                Lwt.return (Transition(Server_ok guid,
                                                       Waiting_for_begin(uid, [])))
                            | Mech_reject ->
                                reject mechs
                  with exn ->
                    reject mechs
            end
        | Client_begin -> Lwt.return Failure
        | Client_error msg -> reject mechs
        | _ -> error "AUTH command expected"
      end

    | Waiting_for_data mech -> begin match cmd with
        | Client_data "" ->
            Lwt.return (Transition(Server_data "",
                                   Waiting_for_data mech))
        | Client_data resp -> begin
            try%lwt
              mech#data resp >>= function
                | Mech_continue chal ->
                    Lwt.return (Transition(Server_data chal,
                                           Waiting_for_data mech))
                | Mech_ok uid ->
                    Lwt.return (Transition(Server_ok guid,
                                           Waiting_for_begin(uid, [])))
                | Mech_reject ->
                    reject mechs
            with exn ->
              reject mechs
          end
        | Client_begin -> mech#abort; Lwt.return Failure
        | Client_cancel -> mech#abort; reject mechs
        | Client_error _ -> mech#abort; reject mechs
        | _ -> mech#abort; error "DATA command expected"
      end

    | Waiting_for_begin(uid, caps) -> begin match cmd with
        | Client_begin ->
            Lwt.return (Accept(uid, caps))
        | Client_cancel ->
            reject mechs
        | Client_error _ ->
            reject mechs
        | Client_negotiate_unix_fd ->
            if List.mem `Unix_fd capabilities then
              Lwt.return(Transition(Server_agree_unix_fd,
                                    Waiting_for_begin(uid,
                                                      if List.mem `Unix_fd caps then
                                                        caps
                                                      else
                                                        `Unix_fd :: caps)))
            else
              Lwt.return(Transition(Server_error "Unix fd passing is not supported by this server",
                                    Waiting_for_begin(uid, caps)))
        | _ ->
            error "BEGIN command expected"
      end

  let authenticate ?(capabilities=[]) ?(mechanisms=default_mechanisms) ?user_id ~guid ~stream () =
    let rec loop state count =
      let%lwt cmd = server_recv stream in
      transition user_id guid capabilities mechanisms state cmd >>= function
        | Transition(cmd, state) ->
            let count =
              match cmd with
                | Server_rejected _ -> count + 1
                | _ -> count
            in
            (* Specification do not specify a limit for rejected, so
               we choose one arbitrary *)
            if count >= max_reject then
              auth_failure "too many reject"
            else
              let%lwt () = server_send stream cmd in
              loop state count
        | Accept(uid, caps) ->
            Lwt.return (uid, caps)
        | Failure ->
            auth_failure "authentication failure"
    in
    loop Waiting_for_auth 0
end
