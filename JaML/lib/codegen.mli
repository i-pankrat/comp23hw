(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Anf
open Llvm

type error

val md : llmodule
val compile : anfexpr list -> (llvalue list, error) result
