(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(** Counter *)
module Counter : sig
  val count_next : unit -> int
  val genid : string -> string
end
