(** Copyright 2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Jaml_lib
open Jaml_lib.Parser
open Result
module Codegen = Codegen

let run_llvm_tests test_case =
  let fmt = Format.std_formatter in
  match parse test_case with
  | Error err -> pp_error fmt err
  | Ok commands ->
    (match Inferencer.infer Inferencer.Enable commands with
     | Error err -> Inferencer.pp_error fmt err
     | Ok typed_commands ->
       let anf =
         Closure.closure typed_commands |> Lambdalift.lambda_lift |> Anfconv.anf
       in
       (match Codegen.compile anf with
        | Ok llvalue_list ->
          Base.List.iter llvalue_list ~f:(fun f ->
            Stdlib.Format.printf "%s\n" (Llvm.string_of_llvalue f))
        | Error _ -> Stdlib.Format.printf "Error"))
;;

let () = run_llvm_tests (Stdio.In_channel.input_all stdin)
