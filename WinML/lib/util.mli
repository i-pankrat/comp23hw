(** Copyright 2023-2024, Vyacheslav Buchin and Artur Gagin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

exception Syntax_error of ((int * int) option * string)

val get_lexing_position : Lexing.lexbuf -> int * int
