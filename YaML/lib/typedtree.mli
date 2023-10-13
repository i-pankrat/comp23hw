(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Typetree
open Ast

type arg = Arg of string * ty (** Typed function argument *)

(** Typed expession type *)
type texpr =
  | TConst of const * ty (** Typed expression for the constant *)
  | TVar of string * ty (** Typed expression for the variables *)
  | TBinop of bin_op * texpr * texpr * ty
  (** Typed expression for the binary operations *)
  | TApp of texpr * texpr * ty
  (** Typed expression for the function application to the arguments *)
  | TIfThenElse of texpr * texpr * texpr
  (** An expression for condition statement: if expr then expr else expr *)
  | TLet of string * texpr * ty (** Typed expression for let declaration *)
  | TLetRec of string * texpr * ty (** Typed expression for let rec declaration*)
  | TLetIn of string * texpr * texpr * ty (** Typed expression for let in declaration *)
  | TLetRecIn of string * texpr * texpr * ty
  (** Typed expression for let rec in declaration *)
  | TFun of arg * texpr * ty (** Typed expression for function *)

(* Constructors for typed expressions *)

val tconst : Ast.const -> Typetree.ty -> texpr
val tvar : string -> Typetree.ty -> texpr
val tbinop : Ast.bin_op -> texpr -> texpr -> Typetree.ty -> texpr
val tapp : texpr -> texpr -> Typetree.ty -> texpr
val tifthenelse : texpr -> texpr -> texpr -> texpr
val tlet : string -> texpr -> Typetree.ty -> texpr
val tletrec : string -> texpr -> Typetree.ty -> texpr
val tletin : string -> texpr -> texpr -> Typetree.ty -> texpr
val tletrecin : string -> texpr -> texpr -> Typetree.ty -> texpr
val tfun : string -> Typetree.ty -> texpr -> Typetree.ty -> texpr
