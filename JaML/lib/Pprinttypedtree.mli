(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type mode =
  | Brief
  | Complete

(** Pretty printer for typed expession *)
val pp_texpr : Format.formatter -> Typedtree.texpr -> unit

(** Pretty printer for typed expession *)
val pp_tbinding : Format.formatter -> Typedtree.tbinding -> unit

(** Pretty printer for typed statements *)
val pp_statements
  :  (unit, Format.formatter, unit) format
  -> mode
  -> Format.formatter
  -> Typedtree.tbinding list
  -> unit
