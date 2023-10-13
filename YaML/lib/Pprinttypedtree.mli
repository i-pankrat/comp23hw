(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(** Pretty printer for typed expesstion *)
val pp_texpr : Format.formatter -> Typedtree.texpr -> unit
