(** Copyright 2023-2024, Anton Kraev and Polina Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let () = KREML_lib.Inferencer.parse_and_infer (Stdio.In_channel.input_all stdin)
