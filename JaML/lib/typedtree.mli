(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Ast

(** Typed patterns *)
type tpattern = pattern * Ty.ty

(** Typed expession type *)
type texpr =
  | TConst of const * Ty.ty (** Typed expression for the constant *)
  | TVar of string * Ty.ty (** Typed expression for the variables *)
  | TBinop of bin_op * texpr * texpr * Ty.ty
  (** Typed expression for the binary operations *)
  | TApp of texpr * texpr * Ty.ty
  (** Typed expression for the function application to the arguments *)
  | TIfThenElse of texpr * texpr * texpr * Ty.ty
  (** Typed expression for condition statement *)
  | TLetIn of pattern * texpr * texpr * Ty.ty
  (** Typed expression for let in declaration *)
  | TLetRecIn of string * texpr * texpr * Ty.ty
  (** Typed expression for let rec in declaration *)
  | TFun of tpattern * texpr * Ty.ty (** Typed expression for function *)
  | TTuple of texpr list * Ty.ty (** Typed expression for the tuples *)

(** Typed binding type *)
type tbinding =
  | TLet of pattern * texpr * Ty.ty (** Typed expression for let declaration *)
  | TLetRec of string * texpr * Ty.ty (** Typed expression for let rec declaration *)

(** Typed statements type *)
type tstatements = tbinding list

(** Constructors for typed expressions *)

val tconst : Ast.const -> Ty.ty -> texpr
val tvar : string -> Ty.ty -> texpr
val tbinop : Ast.bin_op -> texpr -> texpr -> Ty.ty -> texpr
val tapp : texpr -> texpr -> Ty.ty -> texpr
val tifthenelse : texpr -> texpr -> texpr -> Ty.ty -> texpr
val tletin : pattern -> texpr -> texpr -> Ty.ty -> texpr
val tletrecin : string -> texpr -> texpr -> Ty.ty -> texpr
val tfun : tpattern -> texpr -> Ty.ty -> texpr
val ttuple : texpr list -> Ty.ty -> texpr

(** tbindings constructors *)

val tlet : pattern -> texpr -> Ty.ty -> tbinding
val tletrec : string -> texpr -> Ty.ty -> tbinding
