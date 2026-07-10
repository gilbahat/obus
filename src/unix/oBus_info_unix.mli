(*
 * oBus_info_unix.mli
 * ------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

val machine_uuid : OBus_uuid.t Lwt.t Lazy.t
  (** The machine UUID, read from the D-Bus machine-id file
      ([OBus_config.machine_uuid_file]) or [/etc/machine-id].  Falls
      back to a freshly generated UUID if neither file is readable. *)
