(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Ast
open Ty

(** Typed patterns *)
type tpattern =
  | TPVar of string * ty
  | TPWildcard of ty
  | TPTuple of tpattern list * ty

type typed_name = string * ty (** Typed function name *)
type typed_binop = bin_op * ty (** Typed sign of a binary operation *)

(** Typed expression type *)
type texpr =
  | TConst of const * ty (** Typed expression for the constant *)
  | TVar of string * ty (** Typed expression for the variables *)
  | TBinop of typed_binop * texpr * texpr
  (** Typed expression for the binary operations *)
  | TApp of texpr * texpr * ty
  (** Typed expression for the function application to the arguments *)
  | TIfThenElse of texpr * texpr * texpr * ty
  (** Typed expression for condition statement *)
  | TLetIn of tpattern * texpr * texpr (** Typed expression for let in declaration *)
  | TLetRecIn of typed_name * texpr * texpr
  (** Typed expression for let rec in declaration *)
  | TFun of tpattern * texpr * ty (** Typed expression for function *)
  | TTuple of texpr list * ty (** Typed expression for the tuples *)

(** Typed binding type *)
type tbinding =
  | TLet of tpattern * texpr (** Typed expression for let declaration *)
  | TLetRec of typed_name * texpr (** Typed expression for let rec declaration *)

(** Typed statements type *)
type tstatements = tbinding list

(** Typed texpr constructors *)

let tconst c t = TConst (c, t)
let tvar s t = TVar (s, t)
let tbinop b t e1 e2 = TBinop ((b, t), e1, e2)
let tapp f a t = TApp (f, a, t)
let tifthenelse i t e ty = TIfThenElse (i, t, e, ty)
let tletin t a b = TLetIn (t, a, b)
let tletrecin s a b t = TLetRecIn ((s, t), a, b)
let tfun p a t = TFun (p, a, t)
let ttuple t ty = TTuple (t, ty)

(** tbinding constructors *)

let tlet tpat e = TLet (tpat, e)
let tletrec n t e = TLetRec ((n, t), e)
