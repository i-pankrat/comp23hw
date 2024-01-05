(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type error

(** Pretty print for error *)
val pp_error : Format.formatter -> error -> unit

(** Parser of constant *)
val const_p : Ast.const Angstrom.t

(** Parser of pattern *)
val patt_p : Ast.pattern Angstrom.t

(** Parser of expression *)
val expr_p : Ast.expr Angstrom.t

(** Parser of statements *)
val statements_p : Ast.statements Angstrom.t

(** Parser of bindings *)
val bindings_p : Ast.bindings Angstrom.t

(** Mini language parser *)
val parse : string -> (Ast.statements, error) result
