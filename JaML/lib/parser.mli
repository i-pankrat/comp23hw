(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type error

val pp_error : Format.formatter -> error -> unit
val parse : string -> (Ast.statements, error) result
