(** Copyright 2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Anf
open Base
open Stdlib
open Monads.CompilerMonad

(* open Sys *)
open Format

let extern_function = [ "print_int"; "print_bool" ]
let pp_bool b = sprintf "%d" (if b then 1 else 0)

let assembly_immexpr = function
  | ImmNum num -> return @@ sprintf "%d" num
  | ImmBool boolean -> return @@ sprintf "%s" @@ pp_bool boolean
  | ImmId id ->
    let* offset = get_var_stack_offset id in
    return @@ sprintf "qword [rbp -%s]" @@ pp_variable_stack_offset offset
  | _ -> failwith "TODO tuples"
;;

let assembly_cexpr = function
  | CPlus (l, r) ->
    let* l = assembly_immexpr l in
    let* r = assembly_immexpr r in
    return @@ sprintf {|mov rdx, %s
      mov rax, %s
      add rax, rdx|} l r
  | CMinus (l, r) ->
    let* l = assembly_immexpr l in
    let* r = assembly_immexpr r in
    return @@ sprintf {|
      mov rdx, %s
      mov rax, %s
      sub rax, rdx|} l r
  | CDivide (l, r) ->
    let* l = assembly_immexpr l in
    let* r = assembly_immexpr r in
    return @@ sprintf {|
      mov rdx, %s
      mov rax, %s
      idiv rax, rdx|} l r
  | CMultiply (l, r) ->
    let* l = assembly_immexpr l in
    let* r = assembly_immexpr r in
    return @@ sprintf {|
      mov rdx, %s
      mov rax, %s
      imul rax, rdx|} l r
  | CXor (l, r) ->
    let* l = assembly_immexpr l in
    let* r = assembly_immexpr r in
    return @@ sprintf {|
      mov rdx, %s
      mov rax, %s
      xor rax, rdx|} l r
  | CAnd (l, r) ->
    let* l = assembly_immexpr l in
    let* r = assembly_immexpr r in
    return @@ sprintf {|
      mov rdx, %s
      mov rax, %s
      and rax, rdx|} l r
  | COr (l, r) ->
    let* l = assembly_immexpr l in
    let* r = assembly_immexpr r in
    return @@ sprintf {|
      mov rdx, %s
      mov rax, %s
      and rax, rdx|} l r
  | CEq (l, r) ->
    let* l = assembly_immexpr l in
    let* r = assembly_immexpr r in
    return
    @@ sprintf
         {|
      mov rdx, %s
      mov rax, %s
      cmp rax, rdx
      sete rax|}
         (* Check that sete rax is valid *)
         l
         r
  | CNeq (l, r) ->
    let* l = assembly_immexpr l in
    let* r = assembly_immexpr r in
    return
    @@ sprintf
         {|
      mov rdx, %s
      mov rax, %s
      cmp rax, rdx
      setne rax|}
         (* Check that setne rax is valid *)
         l
         r
  | CGt (l, r) ->
    let* l = assembly_immexpr l in
    let* r = assembly_immexpr r in
    return
    @@ sprintf
         {|
      mov rdx, %s
      mov rax, %s
      cmp rax, rdx
      setg rax|}
         (* Check that setg rax is valid *)
         l
         r
  | CLt (l, r) ->
    let* l = assembly_immexpr l in
    let* r = assembly_immexpr r in
    return
    @@ sprintf
         {|
      mov rdx, %s
      mov rax, %s
      cmp rax, rdx
      setl rax|}
         (* Check that setl rax is valid *)
         l
         r
  | CGte (l, r) ->
    let* l = assembly_immexpr l in
    let* r = assembly_immexpr r in
    return
    @@ sprintf
         {|
      mov rdx, %s
      mov rax, %s
      cmp rax, rdx
      setge rax|}
         (* Check that setge rax is valid *)
         l
         r
  | CLte (l, r) ->
    let* l = assembly_immexpr l in
    let* r = assembly_immexpr r in
    return
    @@ sprintf
         {|
      mov rdx, %s
      mov rax, %s
      cmp rax, rdx
      setle rax|}
         (* Check that setle rax is valid *)
         l
         r
  | CApp (func, arg_list) -> failwith "TODO appication"
  | CTake (tuple, n) -> failwith "TODO take"
  | CMakeClosure (func, a, b, args) -> failwith "TODO closure"
  | CImmExpr imm -> assembly_immexpr imm
;;

let rec assembly_aexpr = function
  | ACEexpr cexpr -> assembly_cexpr cexpr
  | ALet (name, cexrp, aexpr) ->
    let* body = assembly_cexpr cexrp in
    let* _ = push_stack name in
    let* aexpr = assembly_aexpr aexpr in
    return @@ sprintf {|  %s
      push rax
      %s
    |} body aexpr
  | _ -> failwith "TODO assembly_aexpr"
;;

let assembly_anfexpr = function
  | AnfLetVar (name, aexpr) ->
    let* _ = push_stack name in
    let* value = assembly_aexpr aexpr in
    return @@ sprintf {|%s
      push rax|} value
  | _ -> failwith "TODO assembly_anfexpr"
;;

let assembly l =
  let* program =
    match l with
    | hd :: _ -> assembly_anfexpr hd
    | _ -> failwith "TODO"
  in
  return @@ sprintf {|
  main:
  %s|} program
;;

let assembly l = run (assembly l)

(* Test file *)
open Result
open Parser

let run_assembly_test test_case =
  let fmt = Format.std_formatter in
  match parse test_case with
  | Error err -> pp_error fmt err
  | Ok commands ->
    (match Inferencer.infer Inferencer.Enable commands with
     | Error err -> Inferencer.pp_error fmt err
     | Ok typed_commands ->
       (match Closure.closure typed_commands |> Lambdalift.lambda_lift |> Anfconv.anf with
        | Error err -> Format.printf "Anf error: %s%!" err
        | Ok anfstatements ->
          (match assembly anfstatements with
           | Ok a -> Format.printf "%s" a
           | Error _ -> failwith "No erorr occure yet")))
;;

let run_assembly_test2 test_case =
  let test_case = [ AnfLetVar ("x", ACEexpr (CPlus (ImmNum 1, ImmNum 2))) ] in
  match assembly test_case with
  | Ok a -> Format.printf "%s" a
  | Error _ -> failwith "No erorr occure yet"
;;

let%expect_test _ =
  let _ =
    let e = {|
        let x = 1 + 2
      |} in
    run_assembly_test e
  in
  [%expect
    {|
    main:

        mov rdx, 1
        mov rax, 2
        add rax, rdx
        push rax
 |}]
;;
