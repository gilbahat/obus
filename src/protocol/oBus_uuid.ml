(*
 * oBus_uuid.ml
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

type t = string

let of_string str =
  let fail _ = raise (Invalid_argument (Printf.sprintf "OBus_uuid.of_string(%S)" str)) in
  if String.length str <> 32 then fail ();
  try OBus_util.hex_decode str
  with _ -> fail ()

let to_string = OBus_util.hex_encode

let generate () = OBus_util.random_string 16
