(** Copyright 2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Llvm
open Anf
open Base

let ctx = create_context ()
let md = create_module ctx "Jaml"
let builder = builder ctx
let i64 = i64_type ctx
let func_ty = function_type i64 [| i64 |]
let func2_ty = function_type i64 [| i64; i64 |]

(* Vararg function type *)
let va_func_ty arr = var_arg_function_type i64 arr
let named_values = Hashtbl.create (module String)
let ( let* ) = Stdlib.Result.bind
let return = Result.return
let fail = Result.fail

type error = Not_Implemented

let rev lst = Array.rev @@ Array.of_list lst

let rec codegen_imm_args args =
  List.fold
    ~f:(fun acc arg ->
      let* arg' = codegen_imm [] arg in
      let* acc = acc in
      return (arg' :: acc))
    ~init:(return [])
    args

and codegen_imm args = function
  | ImmNum i -> return @@ const_int i64 i
  | ImmBool b -> return @@ const_int i64 (Bool.to_int b)
  | ImmId id ->
    (match lookup_function id md with
     | Some v ->
       if Array.is_empty @@ params v
       then
         return
         @@ build_call
              (function_type i64 [||])
              (Stdlib.Option.get @@ lookup_function id md)
              [||]
              "global_var"
              builder
       else if Array.length @@ params v = List.length args
       then return v
       else
         let* args = codegen_imm_args args in
         return
         @@ build_call
              func2_ty
              (Stdlib.Option.get @@ lookup_function "make_pa" md)
              (Array.append
                 [| build_pointercast v i64 "pointer_to_int" builder
                  ; const_int i64 @@ Array.length @@ params v
                  ; const_int i64 (List.length args)
                 |]
                 (rev args))
              "make_pa"
              builder
     | None ->
       return
       @@ build_load i64 (Stdlib.Option.get @@ Hashtbl.find named_values id) id builder)
;;

let bin_op op l' r' =
  let open Ast in
  match op with
  | Add -> build_add l' r' "add" builder
  | Sub -> build_sub l' r' "sub" builder
  | Div -> build_sdiv l' r' "div" builder
  | Mul -> build_mul l' r' "mul" builder
  | Xor -> build_xor l' r' "xor" builder
  | And -> build_and l' r' "and" builder
  | Or -> build_or l' r' "or" builder
  | Eq -> build_icmp Icmp.Eq l' r' "eq" builder
  | Neq -> build_icmp Icmp.Ne l' r' "neq" builder
  | Gt -> build_icmp Icmp.Sgt l' r' "gt" builder
  | Lt -> build_icmp Icmp.Slt l' r' "lt" builder
  | Gte -> build_icmp Icmp.Sge l' r' "gte" builder
  | Lte -> build_icmp Icmp.Sle l' r' "lte" builder
;;

let rec codegen_cexpr cexpr =
  let binop_imm l r =
    let* l = codegen_imm [] l in
    let* r = codegen_imm [] r in
    return (l, r)
  in
  match cexpr with
  | CBinOp (s, l, r) ->
    let* l', r' = binop_imm l r in
    return @@ build_zext (bin_op s l' r') i64 "to_int" builder
  | CImmExpr imm -> codegen_imm [] imm
  | CApp (callee, args) ->
    let* callee = codegen_imm args callee in
    if List.is_empty args
    then return callee
    else
      let* args, args_types =
        List.fold
          ~f:(fun acc arg ->
            let* arg' = codegen_imm [] arg in
            let* args, types = acc in
            let arg_type = Fn.const i64 arg' in
            return (arg' :: args, arg_type :: types))
          ~init:(return ([], []))
          args
      in
      let fnty = function_type i64 (rev args_types) in
      return @@ build_call fnty callee (rev args) "apply_n" builder
  | CMakeClosure (callee, args) ->
    let* callee = codegen_imm args callee in
    return @@ callee
  | CAddArgsToClosure (callee, args) ->
    let* callee = codegen_imm [] callee in
    let* args = codegen_imm_args args in
    return
    @@ build_call
         func2_ty
         (Stdlib.Option.get @@ lookup_function "add_args_to_pa" md)
         (Array.append [| callee; const_int i64 (List.length args) |] (rev args))
         "add_args_to_pa"
         builder
  | CTuple immexpr ->
    let* immlst = codegen_imm_args immexpr in
    return
    @@ build_call
         func_ty
         (Stdlib.Option.get @@ lookup_function "tuple_make" md)
         (Array.append [| const_int i64 @@ List.length immexpr |] (rev immlst))
         "tuple_make"
         builder
  | CTake (immexpr, index) ->
    let* immexpr = codegen_imm [] immexpr in
    return
    @@ build_call
         func2_ty
         (Stdlib.Option.get @@ lookup_function "tuple_take" md)
         [| immexpr; const_int i64 index |]
         "tuple_take"
         builder
  | CIfThenElse (cond, th, el) ->
    let condition = codegen_imm [] cond in
    let* condition = condition in
    let zero = const_int i64 0 in
    let cond_val = build_icmp Icmp.Ne condition zero "if_cond" builder in
    let start_bb = insertion_block builder in
    let func = block_parent start_bb in
    let then_bb = append_block ctx "then" func in
    position_at_end then_bb builder;
    let* then_val = codegen_aexpr th in
    let new_then_bb = insertion_block builder in
    let else_bb = append_block ctx "else" func in
    position_at_end else_bb builder;
    let* else_val = codegen_aexpr el in
    let new_else_bb = insertion_block builder in
    let merge_bb = append_block ctx "if_ctx" func in
    position_at_end merge_bb builder;
    let incoming = [ then_val, new_then_bb; else_val, new_else_bb ] in
    let phi = build_phi incoming "if_phi" builder in
    position_at_end start_bb builder;
    build_cond_br cond_val then_bb else_bb builder |> ignore;
    position_at_end new_then_bb builder;
    build_br merge_bb builder |> ignore;
    position_at_end new_else_bb builder;
    build_br merge_bb builder |> ignore;
    position_at_end merge_bb builder;
    return phi

and codegen_aexpr = function
  | ACEexpr cexpr -> codegen_cexpr cexpr
  | ALet (name, cexpr, aexpr) ->
    let alloca = build_alloca i64 name builder in
    let* cexpr = codegen_cexpr cexpr in
    build_store cexpr alloca builder |> ignore;
    Hashtbl.add_exn named_values ~key:name ~data:alloca;
    codegen_aexpr aexpr
;;

let codegen_anfexpr = function
  | AnfLetFun (name, args, aexpr) ->
    Hashtbl.clear named_values;
    let arg_arr = List.map ~f:(Fn.const i64) args in
    let func2_type = function_type i64 (Array.of_list arg_arr) in
    let func_val = declare_function name func2_type md in
    let entry = append_block ctx "entry" func_val in
    position_at_end entry builder;
    Array.iteri
      ~f:(fun i arg ->
        match Stdlib.Option.get @@ List.nth args i with
        | Used name' ->
          let alloca = build_alloca i64 name' builder in
          build_store arg alloca builder |> ignore;
          set_value_name name' arg;
          Hashtbl.set named_values ~key:name' ~data:alloca
        | Unused -> ())
      (params func_val);
    let* aexpr = codegen_aexpr aexpr in
    build_ret aexpr builder |> ignore;
    return func_val
;;

let compile f =
  let runtime =
    [ declare_function "tuple_make" (va_func_ty [| i64 |]) md
    ; declare_function "tuple_take" func2_ty md
    ; declare_function "make_pa" (va_func_ty [| i64; i64; i64 |]) md
    ; declare_function "add_args_to_pa" (va_func_ty [| i64; i64 |]) md
    ; declare_function "print_int" func_ty md
    ; declare_function "print_bool" func_ty md
    ]
  in
  let* compiled =
    List.fold
      ~f:(fun acc func ->
        let* expr = codegen_anfexpr func in
        let* acc = acc in
        return (expr :: acc))
      ~init:(return runtime)
      f
  in
  return @@ List.rev compiled
;;
