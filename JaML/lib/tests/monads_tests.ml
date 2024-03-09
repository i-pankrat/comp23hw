(** Copyright 2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Jaml_lib.Monads.CompilerMonad

(* let run_monads_data res =
  match run res with
  | Ok commands -> Format.printf "%a" pp_commands commands
  | Error e -> Format.printf "%a" pp_error e
;;

let overwrittingArguments =
  let args = [ "a1"; "a2"; "a3"; "a4"; "a5"; "a6" ] in
  let* f_start = make_func_preambula __FUNCTION__ args in
  let arg_regs = [ R.rdi; R.rsi; R.rdx; R.rcx; R.r8; R.r9 ] in
  let* try_to_overwrite_args =
    monad_map arg_regs ~f:(fun reg ->
      let* one = make_integer 1 in
      mov_cmd reg one)
  in
  let* comment =
    make_comment
      "Let's count the sum of all arguments to check that they are on the stack"
  in
  let* sum_args =
    monad_map args ~f:(fun arg ->
      let* arg = get_id arg in
      mov_cmd R.rax arg)
  in
  let* f_end = make_func_ret in
  return
  @@ f_start
  @ List.concat try_to_overwrite_args
  @ (comment :: List.concat sum_args)
  @ f_end
;;

let%expect_test _ =
  run_monads_data @@ overwrittingArguments;
  [%expect
    {|
    Tests__Monads_tests.overwrittingArguments:
    	push rbp
    	mov rbp, rsp
    	mov qword [rbp -8], rdi; move argument a1 from reg rdi on stack to avoid losing argument
    	mov rdi, 1
    	mov qword [rbp -16], rsi; move argument a2 from reg rsi on stack to avoid losing argument
    	mov rsi, 1
    	mov qword [rbp -24], rdx; move argument a3 from reg rdx on stack to avoid losing argument
    	mov rdx, 1
    	mov qword [rbp -32], rcx; move argument a4 from reg rcx on stack to avoid losing argument
    	mov rcx, 1
    	mov qword [rbp -40], r8; move argument a5 from reg r8 on stack to avoid losing argument
    	mov r8, 1
    	mov qword [rbp -48], r9; move argument a6 from reg r9 on stack to avoid losing argument
    	mov r9, 1
    	; Let's count the sum of all arguments to check that they are on the stack
    	mov rax, qword [rbp -8]
    	mov rax, qword [rbp -16]
    	mov rax, qword [rbp -24]
    	mov rax, qword [rbp -32]
    	mov rax, qword [rbp -40]
    	mov rax, qword [rbp -48]
    	pop rbp
    	ret
 |}]
;;

let doubleRet =
  let* f_start = make_func_preambula __FUNCTION__ [] in
  let* f_end1 = make_func_ret in
  let* f_end2 = make_func_ret in
  return @@ List.concat [ f_start; f_end1; f_end2 ]
;;

let%expect_test _ =
  run_monads_data @@ doubleRet;
  [%expect {| Can not return from function before function creation |}]
;;

let _RSPOffsetDuringCall =
  (*
     When funciton is created frame size is 16.
     0x0
     ---
     old_rbp
     ret_address
     ---
     0x100

     So if we add three local variables, we will have frame_size = 40. 40 % 16 != 0.
     So before call any other function stack must be aligned by 16.

     0x0
     ---
     local_var3 - 40
     local_var2 - 32
     local_var1 - 24
     old_rbp - 16
     ret_address - 8
     ---
     0x100

     Note:
     Creating a local variable is done by successive calls of two commands:
     1. mov_cmd R.rax SOME_VALUE
     2. save_return_value VARIABLE_NAME
  *)
  (* Make test function which does nothing *)
  let* test_start = make_func_preambula "test" [] in
  let* test_end = make_func_ret in
  let* f_start = make_func_preambula __FUNCTION__ [] in
  let* one = make_integer 1 in
  let* c1 = mov_cmd R.rax one in
  let* c2 = save_return_value "local1" in
  let* c3 = mov_cmd R.rax one in
  let* c4 = save_return_value "local2" in
  let* c5 = mov_cmd R.rax one in
  let* c6 = save_return_value "local3" in
  let* test_func_label = get_id "test" in
  let* app = make_func_call test_func_label [] in
  let* comment = make_comment "Checking that variable allocation to the stack is valid" in
  let* c7 = save_return_value "local4" in
  let* f_end = make_func_ret in
  return
  @@ List.concat
       [ test_start
       ; test_end
       ; f_start
       ; c1
       ; c2 :: c3
       ; c4 :: c5
       ; c6 :: app
       ; comment :: c7 :: f_end
       ]
;;

let%expect_test _ =
  run_monads_data @@ _RSPOffsetDuringCall;
  [%expect
    {|
    test:
    	push rbp
    	mov rbp, rsp
    	pop rbp
    	ret
    Tests__Monads_tests._RSPOffsetDuringCall:
    	push rbp
    	mov rbp, rsp
    	mov rax, 1
    	mov qword [rbp -8], rax
    	mov rax, 1
    	mov qword [rbp -16], rax
    	mov rax, 1
    	mov qword [rbp -24], rax
    	sub rsp, 32; restore RSP to the valid value and add alignment if necessary
    	call test
    	mov rsp, rbp; return RSP to RBP
    	; Checking that variable allocation to the stack is valid
    	mov qword [rbp -32], rax
    	pop rbp
    	ret |}]
;;

let _RSPOffsetDuringCallFuncWithTwoArguments =
  (*
     When funciton is created frame size is 16.
     0x0
     ---
     old_rbp
     ret_address
     ---
     0x100

     So if we add one local variable, we will have frame_size = 24. 24 % 16 != 0.
     Then we call a function that takes 8 arguments.
     Six of these arguments will be placed in registers, and the other two will be placed on the stack.

     Thus, the final frame size will be 40, and to align it to 16 you need to increase the stack by 8.

     0x0
     ---
     arg8 -- 40
     arg7 -- 32
     local_var1 -- 24
     old_rbp -- 16
     ret_address -- 8
     ---
     0x100

     Note:
     Creating a local variable is done by successive calls of two commands:
     1. mov_cmd R.rax SOME_VALUE
     2. save_return_value VARIABLE_NAME
  *)
  (* Make test function which does nothing *)
  let* test_start =
    make_func_preambula "sum_two_last" [ "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7"; "a8" ]
  in
  let* zero = make_integer 0 in
  let* mov_zero_to_rax = mov_cmd R.rax zero in
  let* a7 = get_id "a7" in
  let* add_a7 = add_cmd R.rax a7 in
  let* a8 = get_id "a8" in
  let* add_a8 = add_cmd R.rax a8 in
  let* test_end = make_func_ret in
  let* f_start = make_func_preambula __FUNCTION__ [] in
  let* one = make_integer 1 in
  let* c1 = mov_cmd R.rax one in
  let* c2 = save_return_value "local1" in
  let* c3 = mov_cmd R.rax one in
  let* test_func_label = get_id "sum_two_last" in
  let* app =
    make_func_call test_func_label [ zero; zero; zero; zero; zero; zero; zero; zero ]
  in
  let* comment = make_comment "Checking that variable allocation to the stack is valid" in
  let* c7 = save_return_value "local2" in
  let* f_end = make_func_ret in
  return
  @@ List.concat
       [ test_start
       ; mov_zero_to_rax
       ; add_a7
       ; add_a8
       ; test_end
       ; f_start
       ; c1
       ; c2 :: c3
       ; app
       ; comment :: c7 :: f_end
       ]
;;

let%expect_test _ =
  run_monads_data @@ _RSPOffsetDuringCallFuncWithTwoArguments;
  (*
     Stack when sum_two_last is called:

     ----
     old_rbp - 16 <- RSP, RBP
     ret_adr - 8
     ---- frame of sum_two_last
     extra_space_allocated_for_alignment - 48
     arg7 - 40 = []
     arg8 - 32
     local1 - 24
     old_rbp - 16
     ret_adr - 8
     ----
  *)
  [%expect
    {|
    sum_two_last:
    	push rbp
    	mov rbp, rsp
    	mov rax, 0
    	add rax, qword [rbp +24]
    	add rax, qword [rbp +32]
    	pop rbp
    	ret
    Tests__Monads_tests._RSPOffsetDuringCallFuncWithTwoArguments:
    	push rbp
    	mov rbp, rsp
    	mov rax, 1
    	mov qword [rbp -8], rax
    	mov rax, 1
    	mov r9, 0
    	mov r8, 0
    	mov rcx, 0
    	mov rdx, 0
    	mov rsi, 0
    	mov rdi, 0
    	sub rsp, 32; restore RSP to the valid value and add alignment if necessary
    	mov qword [rbp -24], 0
    	mov qword [rbp -16], 0
    	call sum_two_last
    	mov rsp, rbp; return RSP to RBP
    	; Checking that variable allocation to the stack is valid
    	mov qword [rbp -16], rax
    	pop rbp
    	ret |}]
;; *)
