(* Verify OBus_transport.connect works with a callback-based connection.
   This simulates the MirageOS use case: on MirageOS the caller implements
   recv/send using a Mirage TCP flow; here we use Lwt_unix for the test. *)

open Lwt.Infix

let () =
  Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)

let host = "host.docker.internal"
let port = 44444

(* Build an OBus_transport.connection from a Lwt_unix file descriptor.
   On MirageOS this would use Mirage_flow instead. *)
let connection_of_fd fd =
  let recv n =
    let buf = Bytes.create n in
    let rec loop got =
      if got = n then Lwt.return (Bytes.unsafe_to_string buf)
      else
        Lwt_unix.read fd buf got (n - got) >>= fun k ->
        if k = 0 then Lwt.fail End_of_file
        else loop (got + k)
    in
    loop 0
  in
  let send s =
    let buf = Bytes.unsafe_of_string s in
    let len = String.length s in
    let rec loop sent =
      if sent = len then Lwt.return ()
      else
        Lwt_unix.write fd buf sent (len - sent) >>= fun k ->
        loop (sent + k)
    in
    loop 0
  in
  let close () = Lwt_unix.close fd in
  ({ read = recv; write = send; close } : OBus_transport.connection)

let () =
  Lwt_main.run begin
    Printf.printf "Resolving %s...\n%!" host;
    Lwt_unix.getaddrinfo host (string_of_int port)
      [Unix.AI_SOCKTYPE Unix.SOCK_STREAM; Unix.AI_FAMILY Unix.PF_INET] >>= fun addrs ->
    match addrs with
    | [] -> Lwt.fail_with (Printf.sprintf "Cannot resolve %s" host)
    | ai :: _ ->
      let fd = Lwt_unix.socket (Unix.domain_of_sockaddr ai.Unix.ai_addr)
                 Unix.SOCK_STREAM 0 in
      Printf.printf "Connecting to %s:%d...\n%!" host port;
      Lwt_unix.connect fd ai.Unix.ai_addr >>= fun () ->
      Printf.printf "TCP connected. Running D-Bus auth (ANONYMOUS)...\n%!";
      let conn = connection_of_fd fd in
      OBus_transport.connect
        ~mechanisms:[OBus_auth.Client.mech_anonymous]
        conn >>= fun (guid, transport) ->
      Printf.printf "D-Bus auth succeeded!\nServer GUID: %s\n%!"
        (OBus_uuid.to_string guid);
      OBus_transport.shutdown transport >>= fun () ->
      Printf.printf "Transport shut down cleanly.\n%!";
      Lwt.return ()
  end
