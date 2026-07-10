(*
 * oBus_wire_unix.ml
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt.Infix

let section = Lwt_log.Section.make "obus(wire)"

external fd_to_int : Unix.file_descr -> int = "%identity"
external int_to_fd : int -> Unix.file_descr = "%identity"

(* +-----------------------------------------------------------------+
   | Writers with Unix fd passing                                    |
   +-----------------------------------------------------------------+ *)

type writer = {
  w_channel : Lwt_io.output_channel;
  w_file_descr : Lwt_unix.file_descr;
}

let writer fd = {
  w_channel = Lwt_io.of_fd ~mode:Lwt_io.output ~close:Lwt.return fd;
  w_file_descr = fd;
}

let close_writer writer = Lwt_io.close writer.w_channel

let write_message_with_fds writer ?byte_order msg =
  match OBus_wire.string_of_message ?byte_order msg with
    | str, [||] ->
        Lwt_io.write writer.w_channel str
    | str, fds ->
        Lwt_io.atomic begin fun oc ->
          let%lwt () = Lwt_io.flush oc in
          let len = String.length str in
          let vec = Lwt_unix.IO_vectors.create () in
          Lwt_unix.IO_vectors.append_bytes vec (Bytes.unsafe_of_string str) 0 len;
          let unix_fds = Array.to_list (Array.map int_to_fd fds) in
          let%lwt n = Lwt_unix.Versioned.send_msg_2 writer.w_file_descr vec unix_fds in
          assert (n >= 0 && n <= len);
          Lwt_io.write_from_string_exactly oc str n (len - n)
        end writer.w_channel

(* +-----------------------------------------------------------------+
   | Readers with Unix fd passing                                    |
   +-----------------------------------------------------------------+ *)

type reader = {
  r_channel : Lwt_io.input_channel;
  r_pending_fds : Unix.file_descr Queue.t;
}

let reader fd =
  let pending_fds = Queue.create () in
  {
    r_channel = Lwt_io.make ~mode:Lwt_io.input
      (fun buf ofs len ->
         let%lwt n, fds =
           Lwt_bytes.recv_msg fd [Lwt_bytes.io_vector buf ofs len]
         in
         List.iter (fun received_fd ->
           (try Unix.set_close_on_exec received_fd with _ -> ());
           Queue.push received_fd pending_fds) fds;
         Lwt.return n);
    r_pending_fds = pending_fds;
  }

let close_reader reader =
  let fds = Queue.fold (fun acc fd -> fd :: acc) [] reader.r_pending_fds in
  Queue.clear reader.r_pending_fds;
  let%lwt () =
    Lwt_list.iter_p
      (fun fd ->
         try
           Lwt_unix.close (Lwt_unix.of_unix_file_descr ~set_flags:false fd)
         with Unix.Unix_error(err, _, _) ->
           Lwt_log.error_f ~section "cannot close file descriptor: %s"
             (Unix.error_message err))
      fds
  in
  Lwt_io.close reader.r_channel

(* Read a full D-Bus message from the reader's channel, collecting any
   Unix file descriptors that arrived alongside the message bytes.
   Uses [OBus_wire.message_of_string] for actual parsing. *)
let read_message_with_fds reader =
  try%lwt
    Lwt_io.atomic begin fun ic ->
      (* Fixed 16-byte header: endian(1) type(1) flags(1) version(1)
         body_length(4) serial(4) fields_length(4) *)
      let hdr = Bytes.create 16 in
      let%lwt () = Lwt_io.read_into_exactly ic hdr 0 16 in
      let hdr_s = Bytes.unsafe_to_string hdr in
      let get_u32 s ofs =
        match s.[0] with
          | 'l' ->
              Char.code s.[ofs] lor
              (Char.code s.[ofs+1] lsl 8) lor
              (Char.code s.[ofs+2] lsl 16) lor
              (Char.code s.[ofs+3] lsl 24)
          | 'B' ->
              (Char.code s.[ofs] lsl 24) lor
              (Char.code s.[ofs+1] lsl 16) lor
              (Char.code s.[ofs+2] lsl 8) lor
              Char.code s.[ofs+3]
          | c ->
              raise (OBus_wire.Protocol_error
                       (Printf.sprintf "invalid byte order char: %C" c))
      in
      let body_length   = get_u32 hdr_s 4 in
      let fields_length = get_u32 hdr_s 12 in
      (* Header fields start at byte 16; body starts on the next
         8-byte boundary after the fields array. *)
      let header_end = ((16 + fields_length + 7) lsr 3) lsl 3 in
      let extra = header_end - 16 in
      let rest = Bytes.create (extra + body_length) in
      let%lwt () = Lwt_io.read_into_exactly ic rest 0 (extra + body_length) in
      let full_buf = hdr_s ^ Bytes.unsafe_to_string rest in
      (* Collect all FDs that arrived during this read *)
      let fds =
        Array.init (Queue.length reader.r_pending_fds)
          (fun _ -> fd_to_int (Queue.pop reader.r_pending_fds))
      in
      Lwt.return (OBus_wire.message_of_string full_buf fds)
    end reader.r_channel
  with exn ->
    Lwt.fail exn

(* +-----------------------------------------------------------------+
   | Lwt_io channel wrappers (backward-compat helpers)               |
   +-----------------------------------------------------------------+ *)

let read_message_of_ic ic =
  let recv n =
    let buf = Bytes.create n in
    let%lwt () = Lwt_io.read_into_exactly ic buf 0 n in
    Lwt.return (Bytes.unsafe_to_string buf)
  in
  OBus_wire.read_message recv

let write_message_of_oc oc ?byte_order msg =
  OBus_wire.write_message (Lwt_io.write oc) ?byte_order msg
