(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(** Closure conversion of statements *)
val closure : Typedtree.tbinding list -> Typedtree.tbinding list
