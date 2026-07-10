(*
 * oBus_util_unix.ml
 * -----------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Lwt.Infix

let section = Lwt_log.Section.make "obus(util)"

let homedir = lazy(
  try
    Lwt.return (Sys.getenv "HOME")
  with Not_found ->
    let%lwt pwd = Lwt_unix.getpwuid (Unix.getuid ()) in
    Lwt.return pwd.Unix.pw_dir
)

let init_pseudo = Lazy.from_fun Random.self_init

let fill_pseudo buffer pos len =
  ignore (Lwt_log.warning ~section "using pseudo-random generator");
  Lazy.force init_pseudo;
  for i = pos to pos + len - 1 do
    Bytes.unsafe_set buffer i (char_of_int (Random.int 256))
  done

let fill_random buffer pos len =
  try
    let ic = open_in "/dev/urandom" in
    let n = input ic buffer pos len in
    if n < len then fill_pseudo buffer (pos + n) (len - n);
    close_in ic
  with exn ->
    ignore (Lwt_log.warning_f ~exn ~section "failed to get random data from /dev/urandom");
    fill_pseudo buffer pos len
