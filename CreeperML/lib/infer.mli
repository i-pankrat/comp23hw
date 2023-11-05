(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Infer : sig
  open Type_ast.InferType
  open Parser_ast.ParserAst
  open Type_ast.TypeAst
  open Monad.Result

  (* inferencers that gets parser ast and returns typed ast *)

  (* inferencer for expressions *)
  val top_expr_infer : env -> loc_expr -> ty typ_expr t

  (* inferencer for let bindings *)
  val top_let_infer : env -> loc_let_binding -> ty typ_let_binding t

  (* high level inferencer *)
  val top_infer : env -> program -> ty typ_program t
end
