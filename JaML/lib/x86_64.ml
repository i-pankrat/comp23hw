(** Copyright 2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Anf
open Monads.CompilerMonad

let extern_function = [ "print_int"; "print_bool" ]

let op_to_assembly op =
  let open Ast in
  let* zero = make_integer 0 in
  let* zrax = mov_cmd R.rax zero in
  let rdx = reg_to_asmvalue R.rdx in
  match op with
  | Add ->
    let* c = add_cmd R.rax rdx in
    return c
  | Sub ->
    let* c = sub_cmd R.rax rdx in
    return c
  | Div ->
    let* cqp = cqo_cmd ~comment:None in
    let* c = idiv_cmd rdx in
    return (cqp :: c)
  | Mul ->
    let* c = imul_cmd R.rax rdx in
    return c
  | Xor ->
    let* c = xor_cmd R.rax rdx in
    return c
  | And ->
    let* c = and_cmd R.rax rdx in
    return c
  | Or ->
    let* c = or_cmd R.rax rdx in
    return c
  | Eq ->
    let* c1 = cmp_cmd R.rax rdx in
    let* c2 = sete_cmd R.rax in
    return (c1 @ zrax @ c2)
  | Neq ->
    let* c1 = cmp_cmd R.rax rdx in
    let* c2 = setne_cmd R.rax in
    return (c1 @ zrax @ c2)
  | Gt ->
    let* c1 = cmp_cmd R.rax rdx in
    let* c2 = setg_cmd R.rax in
    return (c1 @ zrax @ c2)
  | Lt ->
    let* c1 = cmp_cmd R.rax rdx in
    let* c2 = setl_cmd R.rax in
    return (c1 @ zrax @ c2)
  | Gte ->
    let* c1 = cmp_cmd R.rax rdx in
    let* c2 = setge_cmd R.rax in
    return (c1 @ zrax @ c2)
  | Lte ->
    let* c1 = cmp_cmd R.rax rdx in
    let* c2 = setle_cmd R.rax in
    return (c1 @ zrax @ c2)
;;

let assembly_immexpr = function
  | ImmNum num -> make_integer num
  | ImmBool boolean -> make_bool boolean
  | ImmId id -> get_data_from_environemnt id
;;

let rec assembly_aexpr = function
  | ACEexpr cexpr -> assembly_cexpr cexpr
  | ALet (name, cexrp, aexpr) ->
    let* body_cmd = assembly_cexpr cexrp in
    let* save_cmd = save_return_value name in
    let* aexpr_cmds = assembly_aexpr aexpr in
    return @@ List.concat [ body_cmd; save_cmd :: aexpr_cmds ]

and assembly_cexpr = function
  | CBinOp (op, l, r) ->
    let* l = assembly_immexpr l in
    let* r = assembly_immexpr r in
    let* mov_l = mov_cmd R.rax l in
    let* mov_r = mov_cmd R.rdx r in
    let* op_cmds = op_to_assembly op in
    return (mov_l @ mov_r @ op_cmds)
  | CApp (f, args) ->
    let* func_name = assembly_immexpr f in
    let* func_args = monad_map args ~f:assembly_immexpr in
    make_func_call func_name func_args
  | CTuple elems ->
    let* args = monad_map elems ~f:assembly_immexpr in
    let* arg_num = make_integer (List.length args) in
    let* make_tuple = get_data_from_environemnt "tuple_make" in
    make_func_call make_tuple (arg_num :: args)
  | CTake (tuple, n) ->
    let* tuple = assembly_immexpr tuple in
    let* n = make_integer n in
    let* take = get_data_from_environemnt "tuple_take" in
    make_func_call take [ tuple; n ]
  | CMakeClosure (f, max_args, n, args) ->
    let* f = assembly_immexpr f in
    let* max_args = make_integer max_args in
    let* n = make_integer n in
    let* args = monad_map ~f:assembly_immexpr args in
    let* make_closure = get_data_from_environemnt "make_pa" in
    make_func_call make_closure (f :: max_args :: n :: args)
  | CAddArgsToClosure (f, n, args) ->
    let* f = assembly_immexpr f in
    let* n = make_integer n in
    let* args = monad_map ~f:assembly_immexpr args in
    let* make_closure = get_data_from_environemnt "add_args_to_pa" in
    make_func_call make_closure (f :: n :: args)
  | CImmExpr imm ->
    let* imm = assembly_immexpr imm in
    let* c = mov_cmd R.rax imm in
    return c
  | CIfThenElse (i, t, e) ->
    let* else_if = fresh "else_if" in
    let* enf_if = fresh "end_if" in
    let* else_label = make_label else_if in
    let* end_label = make_label enf_if in
    let* if_value = assembly_immexpr i in
    let* cmd_mov = mov_cmd R.rax if_value in
    let* one = make_integer 1 in
    let* cmd_cmp = cmp_cmd R.rax one in
    let* cmd_jne = jne_cmd else_label in
    let* _ = start_branch in
    let* then_cmds = assembly_aexpr t in
    let* cmd_jmp = jmp_cmd end_label in
    let* _ = end_branch in
    let* _ = start_branch in
    let* else_cmds = assembly_aexpr e in
    let* _ = end_branch in
    let* else_label = make_label_cmd else_label in
    let* end_label = make_label_cmd end_label in
    return
      ((cmd_mov @ cmd_cmp @ (cmd_jne :: then_cmds) @ [ cmd_jmp ])
       @ (else_label :: else_cmds)
       @ [ end_label ])
;;

let assembly_anfexpr (AnfLetFun (name, args, body)) =
  let args =
    Base.List.map
      ~f:(function
        | Used s -> s
        | _ -> "_unused")
      args
  in
  let* cmds1 = make_func_preambula name args in
  let* cmds2 = assembly_aexpr body in
  let* cmds3 = make_func_ret in
  return @@ List.concat [ cmds1; cmds2; cmds3 ]
;;

let assembly l =
  let runtime_functions = [ "make_pa"; "add_args_to_pa"; "tuple_make"; "tuple_take" ] in
  let* runtime =
    monad_map
      ~f:(fun f ->
        let* label = make_label f in
        add_extern label)
      runtime_functions
  in
  let* externs =
    monad_map
      ~f:(fun (f, _, _) ->
        let* label = make_label f in
        add_extern label)
      Jamlstdlib.stdlib_functions
  in
  let* main_label = make_label "main" in
  let* global = add_global main_label in
  let* functions = monad_map l ~f:assembly_anfexpr in
  return @@ runtime @ externs @ (global :: List.concat functions)
;;

let assembly l = run (assembly l)
