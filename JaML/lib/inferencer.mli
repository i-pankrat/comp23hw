(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(** Type inferencer errors *)
type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of Typetree.ty * Typetree.ty
  ]

type occurs_check_mode =
  | Enable
  | Disable

(** Pretty printer for errors *)
val pp_error : Format.formatter -> error -> unit

(** Infer type of statements *)
val infer : Ast.statements -> occurs_check_mode -> (Typedtree.tbinding list, error) result
