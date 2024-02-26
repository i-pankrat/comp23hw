(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Ast

(** Typed patterns *)
type tpattern =
  | TPVar of string * Ty.ty
  | TPWildcard of Ty.ty
  | TPTuple of tpattern list * Ty.ty

type typed_name = string * Ty.ty
type typed_binop = bin_op * Ty.ty

(** Typed expression type *)
type texpr =
  | TConst of const * Ty.ty (** Typed expression for the constant *)
  | TVar of string * Ty.ty (** Typed expression for the variables *)
  | TBinop of typed_binop * texpr * texpr
  (** Typed expression for the binary operations *)
  | TApp of texpr * texpr * Ty.ty
  (** Typed expression for the function application to the arguments *)
  | TIfThenElse of texpr * texpr * texpr * Ty.ty
  (** Typed expression for condition statement *)
  | TLetIn of tpattern * texpr * texpr (** Typed expression for let in declaration *)
  | TLetRecIn of typed_name * texpr * texpr
  (** Typed expression for let rec in declaration *)
  | TFun of tpattern * texpr * Ty.ty (** Typed expression for function *)
  | TTuple of texpr list * Ty.ty (** Typed expression for the tuples *)

(** Typed binding type *)
type tbinding =
  | TLet of tpattern * texpr (** Typed expression for let declaration *)
  | TLetRec of typed_name * texpr (** Typed expression for let rec declaration *)

(** Typed statements type *)
type tstatements = tbinding list

(** Constructors for typed expressions *)

val tconst : Ast.const -> Ty.ty -> texpr
val tvar : string -> Ty.ty -> texpr
val tbinop : Ast.bin_op -> Ty.ty -> texpr -> texpr -> texpr
val tapp : texpr -> texpr -> Ty.ty -> texpr
val tifthenelse : texpr -> texpr -> texpr -> Ty.ty -> texpr
val tletin : tpattern -> texpr -> texpr -> texpr
val tletrecin : string -> texpr -> texpr -> Ty.ty -> texpr
val tfun : tpattern -> texpr -> Ty.ty -> texpr
val ttuple : texpr list -> Ty.ty -> texpr

(** tbindings constructors *)

val tlet : tpattern -> texpr -> tbinding
val tletrec : string -> Ty.ty -> texpr -> tbinding
