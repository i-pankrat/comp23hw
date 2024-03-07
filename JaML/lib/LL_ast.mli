(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Ty
open Ast

(** Almost the same ast as Typedtree.
    Changes: Arguments in let and let rec are represented by a list of arguments.
    There are no TFuns, so anonymous functions are not expressible. *)

(** Typed function name *)
type typed_id = string * ty

(** Typed argument to separate the arguments used and no *)
type typed_arg =
  | Used of string * ty (** Represent usual argument *)
  | Unused of ty (** Represent unused arguments (wildcard for example) *)

(** Typed sign of a binary operation *)
type typed_binop = bin_op * ty

(** Typed expression type *)
type llexpr =
  | LConst of const * ty (** Typed expression for the constant *)
  | LVar of string * ty (** Typed expression for the variables *)
  | LTuple of llexpr list * ty (** Typed expression for the tuples *)
  | LBinop of typed_binop * llexpr * llexpr
  (** Typed expression for the binary operations *)
  | LApp of llexpr * llexpr * ty
  (** Typed expression for the function application to the arguments *)
  | LIfThenElse of llexpr * llexpr * llexpr * ty
  (** Typed expression for condition statement *)
  | LLetIn of typed_id * llexpr * llexpr (** Typed expression for let in declaration *)
  | LTake of llexpr * int
  (** A constructor for getting rid of tuples as a matchmaking pattern.
      By index in the list. *)

(** Typed binding type *)
type llbinding =
  | LLet of typed_id * typed_arg list * llexpr (** Typed expression for let declaration *)
  | LLetRec of typed_id * typed_arg list * llexpr
  (** Typed expression for let rec declaration *)

(** Typed statements type *)
type llstatements = llbinding list
