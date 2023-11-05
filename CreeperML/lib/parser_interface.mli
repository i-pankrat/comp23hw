(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module ParserInterface : sig
  (* parse code from given string *)
  val from_string : string -> Parser_ast.ParserAst.program Monad.Result.t

  (* parse code from given channel *)
  val from_channel : in_channel -> Parser_ast.ParserAst.program Monad.Result.t

  (* parse code from file by given filename *)
  val from_file : string -> Parser_ast.ParserAst.program Monad.Result.t
end
