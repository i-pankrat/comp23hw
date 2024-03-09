(** Copyright 2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Jaml_lib

let run_assembly_test test_case =
  let open Format in
  let fmt = std_formatter in
  match Parser.parse test_case with
  | Error err -> Parser.pp_error fmt err
  | Ok commands ->
    (match Inferencer.infer Inferencer.Enable commands with
     | Error err -> Inferencer.pp_error fmt err
     | Ok typed_commands ->
       Closure.closure typed_commands
       |> Lambdalift.lambda_lift
       |> Anfconv.anf
       |> fun anfstatements ->
       (match X86_64.assembly anfstatements with
        | Ok a -> printf "%a" Monads.CompilerMonad.pp_commands a
        | Error e -> printf "%a" Monads.CompilerMonad.pp_error e))
;;

let%expect_test _ =
  let _ =
    let e = {|
      |} in
    run_assembly_test e
  in
  [%expect {|
    extern make_pa
    extern add_args_to_pa
    extern tuple_make
    extern tuple_take
    extern print_int
    extern print_bool
    global main
      |}]
;;

(* let%expect_test _ =
  let _ =
    let e =
      {|
  let id x = x
  let factorial_preparation n = if n = 1 then n else n * id (n - 1)
  let main = factorial_preparation 10
      |}
    in
    run_assembly_test e
  in
  [%expect {|
    global main
    id:
    	push rbp
    	mov rbp, rsp
    	mov rax, rdi
    	pop rbp
    	ret
    factorial_preparation:
    	push rbp
    	mov rbp, rsp
    	mov rax, rdi
    	mov rdx, 1
    	cmp rax, rdx
    	mov rax, 0
    	sete al
    	mov qword [rbp -8], rax
    	mov rax, qword [rbp -8]
    	cmp rax, 1
    	jne else_if1
    	mov rax, rdi
    	jmp end_if2
    else_if1:
    	mov rax, rdi
    	mov rdx, 1
    	sub rax, rdx
    	mov qword [rbp -16], rax
    	mov rdi, qword [rbp -16]
    	call id
    	mov qword [rbp -24], rax
    	mov rax, rdi
    	mov rdx, qword [rbp -24]
    	imul rax, rdx
    	mov qword [rbp -32], rax
    	mov rax, qword [rbp -32]
    end_if2:
    	mov qword [rbp -40], rax
    	mov rax, qword [rbp -40]
    	pop rbp
    	ret
    main:
    	push rbp
    	mov rbp, rsp
    	mov rdi, 10
    	call factorial_preparation
    	mov qword [rbp -8], rax
    	mov rax, qword [rbp -8]
    	pop rbp
    	ret
      |}]
;; *)

(* let%expect_test _ =
  let _ =
    let e =
      {|
		let rec fact n = if n = 1 then 1 else n * fact (n - 1) 
		let main = fact 5
      |}
    in
    run_assembly_test e
  in
  [%expect
    {|
    global main
    fact:
    	push rbp
    	mov rbp, rsp
    	mov rax, rdi
    	mov rdx, 1
    	cmp rax, rdx
    	mov rax, 0
    	sete al
    	mov qword [rbp -8], rax
    	mov rax, qword [rbp -8]
    	cmp rax, 1
    	jne else_if1:
    	mov rax, 1
    	jmp end_if2:
    else_if1:
    	mov rax, rdi
    	mov rdx, 1
    	sub rax, rdx
    	mov qword [rbp -16], rax
    	mov rdi, qword [rbp -16]
    	call fact
    	mov qword [rbp -24], rax
    	mov rax, rdi
    	mov rdx, qword [rbp -24]
    	imul rax, rdx
    	mov qword [rbp -32], rax
    	mov rax, qword [rbp -32]
    end_if2:
    	mov qword [rbp -40], rax
    	mov rax, qword [rbp -40]
    	pop rbp
    	ret
    main:
    	push rbp
    	mov rbp, rsp
    	mov rdi, 5
    	call fact
    	mov qword [rbp -8], rax
    	mov rax, qword [rbp -8]
    	pop rbp
    	ret
      |}]
;; *)

(* let%expect_test _ =
  let _ =
    let e = {|
        let id a = a
      |} in
    run_assembly_test e
  in
  [%expect
    {|
    global main
    id:
    	push rbp
    	mov rbp, rsp
    	mov rax, rdi
    	pop rbp
    	ret
 |}]
;;

let%expect_test _ =
  let _ =
    let e = {|
        let main = 1 + 2 + 3 + 4 + 5
      |} in
    run_assembly_test e
  in
  [%expect
    {|
    global main
    main:
    	push rbp
    	mov rbp, rsp
    	mov rax, 1
    	mov rdx, 2
    	add rax, rdx
    	mov qword [rbp -8], rax
    	mov rax, qword [rbp -8]
    	mov rdx, 3
    	add rax, rdx
    	mov qword [rbp -16], rax
    	mov rax, qword [rbp -16]
    	mov rdx, 4
    	add rax, rdx
    	mov qword [rbp -24], rax
    	mov rax, qword [rbp -24]
    	mov rdx, 5
    	add rax, rdx
    	mov qword [rbp -32], rax
    	mov rax, qword [rbp -32]
    	pop rbp
    	ret
 |}]
;;

(*
   TODO: есть подозрения, что передача регистров через стек плохо работает.
   Пока лень вчитываться в ассемблер...
*)
let%expect_test _ =
  let _ =
    let e =
      {|
        let sum8 a1 a2 a3 a4 a5 a6 a7 a8 = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8
        let res = sum8 1 2 3 4 5 6 7 8 
      |}
    in
    run_assembly_test e
  in
  [%expect
    {|
    global main
    sum8:
    	push rbp
    	mov rbp, rsp
    	mov rax, rdi
    	mov qword [rbp -8], rdx; move argument a3 from reg rdx on stack to avoid losing argument
    	mov rdx, rsi
    	add rax, rdx
    	mov qword [rbp -16], rax
    	mov rax, qword [rbp -16]
    	mov rdx, qword [rbp -8]
    	add rax, rdx
    	mov qword [rbp -24], rax
    	mov rax, qword [rbp -24]
    	mov rdx, rcx
    	add rax, rdx
    	mov qword [rbp -32], rax
    	mov rax, qword [rbp -32]
    	mov rdx, r8
    	add rax, rdx
    	mov qword [rbp -40], rax
    	mov rax, qword [rbp -40]
    	mov rdx, r9
    	add rax, rdx
    	mov qword [rbp -48], rax
    	mov rax, qword [rbp -48]
    	mov rdx, qword [rbp +24]
    	add rax, rdx
    	mov qword [rbp -56], rax
    	mov rax, qword [rbp -56]
    	mov rdx, qword [rbp +32]
    	add rax, rdx
    	mov qword [rbp -64], rax
    	mov rax, qword [rbp -64]
    	pop rbp
    	ret
    res:
    	push rbp
    	mov rbp, rsp
    	mov r9, 6
    	mov r8, 5
    	mov rcx, 4
    	mov rdx, 3
    	mov rsi, 2
    	mov rdi, 1
    	sub rsp, 16; restore RSP to the valid value and add alignment if necessary
    	mov qword [rbp -16], 8
    	mov qword [rbp -8], 7
    	call sum8
    	mov rsp, rbp; return RSP to RBP
    	mov qword [rbp -8], rax
    	mov rax, qword [rbp -8]
    	mov qword [rbp -16], rax
    	mov rax, qword [rbp -16]
    	pop rbp
    	ret
 |}]
;;

let%expect_test _ =
  let _ =
    let e = {|
        let int_to_bool n = if n = 0 then false else true
      |} in
    run_assembly_test e
  in
  [%expect
    {|
    global main
    int_to_bool:
    	push rbp
    	mov rbp, rsp
    	mov rax, rdi
    	mov rdx, 0
    	cmp rax, rdx
    	mov rax, 0
    	sete al
    	mov qword [rbp -8], rax
    	mov rax, qword [rbp -8]
    	cmp rax, 1
    	jne else_if1
    	mov rax, 0
    	jmp end_if2
    else_if1:
    	mov rax, 1
    end_if2:
    	mov qword [rbp -16], rax
    	mov rax, qword [rbp -16]
    	pop rbp
    	ret
 |}]
;;

(* TODO: Fix the test by adding extern function for runtime *)
let%expect_test _ =
  let _ =
    let e = {|
        let test a b = (a, b)
      |} in
    run_assembly_test e
  in
  [%expect {| Label OR variable 'make_tuple' is unbound |}]
;; *)

(* let%expect_test _ =
  let _ =
    let e =
      {|
		let id1 x = x
		let id2 y = id1 y
		let id3 z = id2 z
		let id4 y = id3 y
		let id5 k = id4 k 
		let main = id5 1
      |}
    in
    run_assembly_test e
  in
  [%expect
    {|
    global main:

    id1:
    	push rbp
    	mov rbp, rsp
    	mov rax, rdi
    	pop rbp
    	ret
    id2:
    	push rbp
    	mov rbp, rsp
    	mov rdi, rdi
    	call id1
    	mov qword [rbp -8], rax
    	mov rax, qword [rbp -8]
    	pop rbp
    	ret
    id3:
    	push rbp
    	mov rbp, rsp
    	mov rdi, rdi
    	call id2
    	mov qword [rbp -8], rax
    	mov rax, qword [rbp -8]
    	pop rbp
    	ret
    id4:
    	push rbp
    	mov rbp, rsp
    	mov rdi, rdi
    	call id3
    	mov qword [rbp -8], rax
    	mov rax, qword [rbp -8]
    	pop rbp
    	ret
    id5:
    	push rbp
    	mov rbp, rsp
    	mov rdi, rdi
    	call id4
    	mov qword [rbp -8], rax
    	mov rax, qword [rbp -8]
    	pop rbp
    	ret
    main:
    	push rbp
    	mov rbp, rsp
    	mov rdi, 1
    	call id5
    	mov qword [rbp -8], rax
    	mov rax, qword [rbp -8]
    	pop rbp
    	ret
      |}]
;; *)

(* global main
   print_int:
   push rbp
   mov rbp, rsp
   mov rax, 4
   mov rbx, 0
   mov rcx, qword [rbp +8]
   mov rdx, 8
   int 0x80
   ret
   main:
   push rbp
   mov rbp, rsp
   sub rsp, 8; restore RSP to the valid value and add alignment if necessary
   push 1
   call print_int
   mov rsp, rbp; return RSP to RBP
   mov qword [rbp -8], rax
   mov rax, qword [rbp -8]
   mov qword [rbp -16], rax
   mov rax, qword [rbp -16]
   pop rbp
   ret *)

(* let%expect_test _ =
  let _ =
    let e = {|
  let rec fact n = if n = 1 then 1 else n * fact (n - 1)
      |} in
    run_assembly_test e
  in
  [%expect
    {|
    global main
    fact:
    	push rbp
    	mov rbp, rsp
    	mov rax, rdi
    	mov rdx, 1
    	cmp rax, rdx
    	mov rax, 0
    	sete al
    	mov qword [rbp -8], rax
    	mov rax, qword [rbp -8]
    	cmp rax, 1
    	jne else_if1
    	mov rax, 1
    	jmp end_if2
    else_if1:
    	mov rax, rdi
    	mov rdx, 1
    	sub rax, rdx
    	mov qword [rbp -16], rax
    	mov rdi, qword [rbp -16]
    	sub rsp, 16; restore RSP to the valid value and add alignment if necessary
    	call fact
    	mov rsp, rbp; return RSP to RBP
    	mov qword [rbp -24], rax
    	mov rax, qword [rbp -24]
    	mov qword [rbp -32], rax
    	mov rax, rdi
    	mov rdx, qword [rbp -32]
    	imul rax, rdx
    	mov qword [rbp -40], rax
    	mov rax, qword [rbp -40]
    end_if2:
    	mov qword [rbp -48], rax
    	mov rax, qword [rbp -48]
    	pop rbp
    	ret |}]
;; *)
