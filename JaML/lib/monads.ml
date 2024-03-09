(** Copyright 2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base

module VariableNameGeneratorMonad = struct
  type 'a t = int -> 'a * int

  let return a : 'a t = fun state -> a, state

  let bind (t : 'a t) ~(f : 'a -> 'b t) : 'b t =
    fun state ->
    let a, new_state = t state in
    let b, final_state = f a new_state in
    b, final_state
  ;;

  let ( let* ) x f = bind x ~f
  let fresh name : string t = fun state -> name ^ string_of_int state, state + 1
  let run (m : 'a t) = fst (m 1)

  let monad_fold ~init ~f l : 'a t =
    List.fold
      l
      ~f:(fun acc x ->
        let* acc = acc in
        f acc x)
      ~init:(return init)
  ;;
end

module CompilerMonad = struct
  (* Used modules *)
  open Format
  module Labels = Stdlib.Set.Make (String)
  module LocalVariables = Stdlib.Map.Make (String)

  module Stack = struct
    type 'a t = 'a list

    exception Empty_stack

    let empty = []
    let push a stack = a :: stack

    let pop = function
      | [] -> raise Empty_stack
      | hd :: tl -> hd, tl
    ;;
  end

  (* Environment types *)

  type locals = string LocalVariables.t

  module Frame = struct
    type t =
      { locals : locals
      ; rbp_pointer : int
      ; rsp_pointer : int
      }

    let with_locals locals = { locals; rbp_pointer = 16; rsp_pointer = 16 }
    let empty = with_locals LocalVariables.empty

    let pop_rsp_n n frame =
      { locals = frame.locals
      ; rbp_pointer = frame.rbp_pointer
      ; rsp_pointer = frame.rsp_pointer - (n * 8)
      }
    ;;

    let pop_rsp = pop_rsp_n 1

    let push_rsp_n n frame =
      { locals = frame.locals
      ; rbp_pointer = frame.rbp_pointer
      ; rsp_pointer = frame.rsp_pointer + (n * 8)
      }
    ;;

    let push_rsp = push_rsp_n 1

    let add_local local binding frame =
      { locals = LocalVariables.add local binding frame.locals
      ; rbp_pointer = frame.rbp_pointer
      ; rsp_pointer = frame.rsp_pointer
      }
    ;;

    let is_stack_will_be_aligned n frame = (frame.rsp_pointer + (n * 8)) % 16 == 0
    let get_local local frame = LocalVariables.find_opt local frame.locals

    let exist_local_bind_to_reg reg frame =
      LocalVariables.fold
        (fun key data acc ->
          match acc with
          | Some _ -> acc
          | None -> if String.equal reg data then Some key else None)
        frame.locals
        None
    ;;

    let get_diff_rsp_rbp frame = frame.rsp_pointer - frame.rbp_pointer
  end

  module State = struct
    type frame_stack = Frame.t Stack.t

    type t =
      { labels : Labels.t
      ; frame : Frame.t
      ; stack : frame_stack
      ; counter : int
      }

    let empty =
      { labels = Labels.empty; frame = Frame.empty; stack = Stack.empty; counter = 1 }
    ;;

    let push_new_frame state =
      { labels = state.labels
      ; frame = Frame.empty
      ; stack = Stack.push state.frame state.stack
      ; counter = state.counter
      }
    ;;

    let push_if_frame state =
      { labels = state.labels
      ; frame = state.frame
      ; stack = Stack.push state.frame state.stack
      ; counter = state.counter
      }
    ;;

    let pop_if_frame state =
      let frame, stack = Stack.pop state.stack in
      { labels = state.labels; frame; stack; counter = state.counter }
    ;;

    let pop_old_frame state =
      let old_frame, stack = Stack.pop state.stack in
      ( state.frame
      , { labels = state.labels; frame = old_frame; stack; counter = state.counter } )
    ;;

    let is_label_exist label state =
      match Labels.find_opt label state.labels with
      | None -> false
      | _ -> true
    ;;

    (** Add new label to the environment.
        Return new environment. *)
    let add_label label state =
      { labels = Labels.add label state.labels
      ; frame = state.frame
      ; stack = state.stack
      ; counter = state.counter
      }
    ;;

    (* Aliases for frame *)

    (** Этот метод увеличивает значение rsp на 8 * n и возвращает смещение относительно rsp,
        от котрого нужно отнять это смещение, чтобы положить этот аргумент *)
    let push_rsp_n n state =
      if n == 0
      then state.frame.rsp_pointer - state.frame.rbp_pointer, state
      else (
        let frame = Frame.push_rsp_n n state.frame in
        ( state.frame.rsp_pointer - frame.rbp_pointer + 8
        , { labels = state.labels; frame; stack = state.stack; counter = state.counter } ))
    ;;

    let push_rsp state = push_rsp_n 1 state

    let pop_rsp_n n state =
      let frame = Frame.pop_rsp_n n state.frame in
      ( state.frame.rsp_pointer - frame.rbp_pointer
      , { labels = state.labels; frame; stack = state.stack; counter = state.counter } )
    ;;

    let pop_rsp = pop_rsp_n 1

    (** Add new local variable with binding to the environment.
        Returns new state which include new binding. *)
    let add_local local binding state =
      { labels = state.labels
      ; frame = Frame.add_local local binding state.frame
      ; stack = state.stack
      ; counter = state.counter
      }
    ;;

    let get_local local state = Frame.get_local local state.frame

    (** Search whether there is a variable that is bind to reg.
        If exists then returns (Some var) else returns None *)
    let exist_local_bind_to_reg reg state = Frame.exist_local_bind_to_reg reg state.frame

    let is_stack_will_be_aligned n state = Frame.is_stack_will_be_aligned n state.frame
    let get_diff_rsp_rbp state = Frame.get_diff_rsp_rbp state.frame
  end

  (* Asm types *)

  type register = string
  type label = string
  type stack_memory = string
  type int_constant = string
  type func_ptr = string

  type asm_value =
    | Reg of register
    | Label of label
    | StackMem of stack_memory
    | Int of int_constant

  type command = string

  type error =
    | TryToStoreInRegsMoreThanSixArguments
    | UnboundLabel of string
    | UnboundLocalVar of string
    | UnboundValue of string
    | UnboundArgument of string
    | MakeRetFromFunctionMoreThanFunctionCreations
    | Reg64DoesNotSupportReg8 of register
    | CanNotPushLabelOnStack of label
    | CanNotMakeCallTo of asm_value

  (* Type converters *)

  let asm_value_to_str = function
    | Reg s | Label s | StackMem s | Int s -> s
  ;;

  (* Pretty printers *)

  let pp_error ppf = function
    | TryToStoreInRegsMoreThanSixArguments ->
      fprintf
        ppf
        "%s"
        "System V ABI does not support stoting more than 6 arguments in registers"
    | UnboundLabel l -> fprintf ppf "Label '%s' is unbound" l
    | UnboundLocalVar l -> fprintf ppf "Local variable '%s' is unbound" l
    | UnboundValue a -> fprintf ppf "Value '%s' is unbound" a
    | UnboundArgument a -> fprintf ppf "Argument '%s' is unbound" a
    | MakeRetFromFunctionMoreThanFunctionCreations ->
      fprintf ppf "Can not return from function before function creation"
    | Reg64DoesNotSupportReg8 reg ->
      fprintf ppf "Register %s does not support convertsion to reg8" reg
    | CanNotPushLabelOnStack l -> fprintf ppf "Cannot push label %s on stack" l
    | _ -> failwith "TODO: pp_error"
  ;;

  let pp_asmvalue ppf = function
    | Reg s -> fprintf ppf "reg(%s)" s
    | Label l -> fprintf ppf "label(%s)" l
    | Int i -> fprintf ppf "%s" i
    | StackMem sm -> fprintf ppf "%s" sm
    | _ -> failwith "TODO: pp_asmvalue"
  ;;

  (* Monad heart *)

  type ('a, 'error) t = State.t -> ('a, 'error) Result.t * State.t

  (* Standard monad functions *)

  let return : 'a -> ('a, 'error) t = fun x state -> Result.return x, state
  let fail : 'error -> ('a, 'error) t = fun e state -> Result.fail e, state

  let bind (t : ('a, 'error) t) (f : 'a -> ('b, 'error) t) : ('b, 'error) t =
    fun state ->
    let a, state = t state in
    match a with
    | Result.Error x -> Error x, state
    | Ok x -> f x state
  ;;

  let ( let* ) x f = bind x f

  let fresh name : (string, 'error) t =
    fun state ->
    return
      (name ^ string_of_int state.counter)
      { labels = state.labels
      ; frame = state.frame
      ; stack = state.stack
      ; counter = state.counter + 1
      }
  ;;

  let monad_map l ~f =
    let rec helper acc = function
      | [] -> return (List.rev acc)
      | hd :: tl ->
        let* res = f hd in
        helper (res :: acc) tl
    in
    helper [] l
  ;;

  let run (m : ('a, 'error) t) =
    let r, _ = m State.empty in
    r
  ;;

  (* Environment *)

  let save_frame : (Frame.t, 'error) t =
    fun state ->
    let new_state = State.push_new_frame state in
    return state.frame new_state
  ;;

  let restore_frame : (Frame.t, 'error) t =
    fun state ->
    match State.pop_old_frame state with
    | frame, state -> return frame state
    | exception Stack.Empty_stack ->
      fail MakeRetFromFunctionMoreThanFunctionCreations state
  ;;

  (* Constants *)

  let empty_cmd = ""
  let singleton_cmd x = [ x ]

  module R = struct
    (* Reg64 *)
    let rax = "rax"
    let rbx = "rbx"
    let rcx = "rcx"
    let rdx = "rdx"
    let rsp = "rsp"
    let rbp = "rbp"
    let rsi = "rsi"
    let rdi = "rdi"
    let r8 = "r8"
    let r9 = "r9"
    let r10 = "r10"
    let r11 = "r11"
    let r12 = "r12"
    let r13 = "r13"
    let r14 = "r14"
    let r15 = "r15"

    (* Reg8 *)
    let al = "al"
    let bl = "bl"
    let cl = "cl"
    let dl = "dl"
    let spl = "spl"
    let bpl = "bpl"
    let sil = "sil"
    let dil = "dil"
    let r8b = "r8b"
    let r9b = "r9b"
    let r10b = "r10b"
    let r11b = "r11b"
    let r12b = "r12b"
    let r13b = "r13b"
    let r14b = "r14b"
    let r15b = "r15b"

    let all_regs64 =
      [ rax; rbx; rcx; rdx; rsp; rbp; rsi; rdi; r8; r9; r10; r11; r12; r12; r14; r15 ]
    ;;

    (* We dont save rax...*)
    let caller_saved = [ (*rax;*) rcx; rdx; rsi; rdi; r8; r9; r10; r11 ]
    let callee_saved = [ rbx; rbp; r12; r13; r14; r15 ]
    let regs_for_args = [ rdi; rsi; rdx; rcx; r8; r9 ]
    let number_of_args_saved_to_regs = 6
    let is_reg reg = List.exists all_regs64 ~f:(fun l_reg -> reg == l_reg)
    let is_reg_for_args reg = List.exists regs_for_args ~f:(fun l_reg -> reg == l_reg)

    let reg64_to_reg8 = function
      | "rax" -> return al
      | "rbx" -> return bl
      | "rcx" -> return cl
      | "rdx" -> return dl
      | "rsp" -> return spl
      | "rbp" -> return bpl
      | "rsi" -> return sil
      | "rdi" -> return dil
      | "r8" -> return r8b
      | "r9" -> return r9b
      | "r10" -> return r10b
      | "r11" -> return r11b
      | "r12" -> return r12b
      | "r13" -> return r13b
      | "r14" -> return r14b
      | "r15" -> return r15b
      | _ as reg -> fail (Reg64DoesNotSupportReg8 reg)
    ;;
  end

  (* General functions *)

  let make_cmd c = sprintf "\t%s\n" c
  let make_cmd_with_comment cmd com = sprintf "\t%s; %s\n" cmd com

  let make_label : string -> (label, 'error) t =
    fun l state -> return l (State.add_label l state)
  ;;

  let make_label_cmd : label -> (command, 'error) t =
    fun l state -> return (sprintf "%s:\n" l) state
  ;;

  let make_comment : string -> (command, 'error) t =
    fun s state -> return (sprintf "\t; %s\n" s) state
  ;;

  (* Unsave commands which does not care about data in the stored in registers *)

  let make_cmd2_unsafe comment f a b =
    return
    @@
    match comment with
    | Some com -> make_cmd_with_comment (sprintf f a b) com
    | _ -> make_cmd @@ sprintf f a b
  ;;

  let make_cmd1_unsafe comment f a =
    return
    @@
    match comment with
    | Some com -> make_cmd_with_comment (sprintf f a) com
    | _ -> make_cmd @@ sprintf f a
  ;;

  let make_cmd0_unsafe comment f =
    return
    @@
    match comment with
    | Some com -> make_cmd_with_comment f com
    | _ -> make_cmd @@ f
  ;;

  (* Unsafe commands. They should not be used by a monad user *)

  let mov_cmd_unsave ?(comment = None) a b =
    make_cmd2_unsafe comment "mov %s, %s" a (asm_value_to_str b)
  ;;

  let add_cmd_unsave ?(comment = None) a b =
    make_cmd2_unsafe comment "add %s, %s" a (asm_value_to_str b)
  ;;

  let sub_cmd_unsave ?(comment = None) a b =
    make_cmd2_unsafe comment "sub %s, %s" a (asm_value_to_str b)
  ;;

  let save_from_reg_to_var : ?comment:string -> string -> string -> (string, 'error) t =
    fun ?comment reg var state ->
    (* printf
       "save_from_reg_to_var: rsp=%d rbp=%d\n"
       state.frame.rsp_pointer
       state.frame.rbp_pointer; *)
    let mov_cmd a b = mov_cmd_unsave ~comment a b in
    let rsp, state = State.push_rsp state in
    (* printf "save_from_reg_to_var: rsp = %d\n" rsp; *)
    let bind_to_stack = sprintf "qword [%s -%d]" R.rbp rsp in
    let state = State.add_local var bind_to_stack state in
    mov_cmd bind_to_stack (Reg reg) state
  ;;

  let check_reg : string -> (command, 'error) t =
    fun reg state ->
    match State.exist_local_bind_to_reg reg state with
    | Some var ->
      let comm =
        sprintf "move argument %s from reg %s on stack to avoid losing argument" var reg
      in
      save_from_reg_to_var ~comment:comm reg var state
    | None -> return empty_cmd state
  ;;

  let observe : string -> (command, 'error) t =
    fun reg state ->
    if R.is_reg_for_args reg then check_reg reg state else return empty_cmd state
  ;;

  (*
     Save commands do care about some local variables stored in the registers.margin
     They (save commands) try to check if some local variable lies in a register, if so, they save it to the stack.
  *)

  let make_cmd2
    :  string option -> (command -> 'a -> command, unit, command) format -> string
    -> string -> (command list, 'error) t
    =
    fun comment f a b ->
    let* cmd1 = observe a in
    let* cmd2 = make_cmd2_unsafe comment f a b in
    return [ cmd1; cmd2 ]
  ;;

  let make_cmd1
    :  string option -> (command -> command, unit, command) format -> string
    -> (command list, 'error) t
    =
    fun comment f a ->
    let* cmd1 = observe a in
    let* cmd2 = make_cmd1_unsafe comment f a in
    return [ cmd1; cmd2 ]
  ;;

  let mov_cmd ?(comment = None) a b =
    make_cmd2 comment "mov %s, %s" a (asm_value_to_str b)
  ;;

  let add_cmd ?(comment = None) a b =
    make_cmd2 comment "add %s, %s" a (asm_value_to_str b)
  ;;

  let sub_cmd ?(comment = None) a b =
    make_cmd2 comment "sub %s, %s" a (asm_value_to_str b)
  ;;

  let idiv_cmd ?(comment = None) a = make_cmd1 comment "idiv %s" (asm_value_to_str a)

  let imul_cmd ?(comment = None) a b =
    make_cmd2 comment "imul %s, %s" a (asm_value_to_str b)
  ;;

  let xor_cmd ?(comment = None) a b =
    make_cmd2 comment "xor %s, %s" a (asm_value_to_str b)
  ;;

  let and_cmd ?(comment = None) a b =
    make_cmd2 comment "and %s, %s" a (asm_value_to_str b)
  ;;

  let or_cmd ?(comment = None) a b = make_cmd2 comment "or %s, %s" a (asm_value_to_str b)

  let cmp_cmd ?(comment = None) a b =
    make_cmd2 comment "cmp %s, %s" a (asm_value_to_str b)
  ;;

  (* Comands with one argument *)

  let reg64_to_r8_cmd comment f a =
    let* a = R.reg64_to_reg8 a in
    make_cmd1 comment f a
  ;;

  let sete_cmd ?(comment = None) a = reg64_to_r8_cmd comment "sete %s" a
  let setne_cmd ?(comment = None) a = reg64_to_r8_cmd comment "setne %s" a
  let setg_cmd ?(comment = None) a = reg64_to_r8_cmd comment "setg %s" a
  let setl_cmd ?(comment = None) a = reg64_to_r8_cmd comment "setl %s" a
  let setge_cmd ?(comment = None) a = reg64_to_r8_cmd comment "setge %s" a
  let setle_cmd ?(comment = None) a = reg64_to_r8_cmd comment "setle %s" a
  let jmp_cmd ?(comment = None) a = make_cmd1_unsafe comment "jmp %s" a
  let jne_cmd ?(comment = None) a = make_cmd1_unsafe comment "jne %s" a
  let je_cmd ?(comment = None) a = make_cmd1_unsafe comment "je %s" a

  (* Unavaliable for user *)
  let call_cmd ?(comment = None) a = make_cmd1_unsafe comment "call %s" a
  let push_cmd ?(comment = None) a = make_cmd1_unsafe comment "push %s" a
  let pop_cmd ?(comment = None) a = make_cmd1_unsafe comment "pop %s" a

  (* Comands with no arguments *)
  let ret_cmd ?(comment = None) = make_cmd0_unsafe comment "ret"
  let cqo_cmd ?(comment = None) = make_cmd0_unsafe comment "cqo"

  let add_global : label -> (command, error) t =
    fun s -> return @@ sprintf "global %s\n" s
  ;;

  let add_extern : label -> (command, error) t =
    fun s -> return @@ sprintf "extern %s\n" s
  ;;

  (* Get some data from environment *)

  let get_variable : string -> (asm_value, error) t =
    fun var_name state ->
    match State.get_local var_name state with
    | Some x -> if R.is_reg x then return (Reg x) state else return (StackMem x) state
    | _ -> fail (UnboundLocalVar var_name) state
  ;;

  let get_label : string -> (asm_value, error) t =
    fun label_name state ->
    if State.is_label_exist label_name state
    then return (Label label_name) state
    else fail (UnboundLabel label_name) state
  ;;

  (* PUBLIC *)
  let get_data_from_environemnt : string -> (asm_value, error) t =
    fun id state ->
    match get_variable id state with
    | (Ok _, _) as good -> good
    | _ -> get_label id state
  ;;

  (* Working with stack *)

  let save_rbp : (command, 'error) t = push_cmd R.rbp
  let pop_rbp : (command, 'error) t = pop_cmd R.rbp

  let increase_rsp : (command, 'error) t =
    fun state ->
    let diff = State.get_diff_rsp_rbp state in
    if diff > 0
    then
      sub_cmd_unsave
        ~comment:(Some "restore RSP to the valid value and add alignment if necessary")
        R.rsp
        (Int (string_of_int diff))
        state
    else return empty_cmd state
  ;;

  let restore_rsp : int -> (command, 'a) t =
    fun prev_rsp state ->
    let n = (state.frame.rsp_pointer - prev_rsp) / 8 in
    let _, state = State.pop_rsp_n n state in
    mov_cmd_unsave ~comment:(Some "return RSP to RBP") R.rsp (Reg R.rbp) state
  ;;

  (** Увеличивает стэк для того, чтобы смочь на него положить все аргументы, возвращает команду по увеличению и значение, с которого нужно
      начинать класть аргументы на стек *)
  let prepare_stack : int -> (int * int * command, 'error) t =
    fun n init_state ->
    (* printf
       "prepare_stack: rsp %d rbp %d n = %d\n"
       init_state.frame.rsp_pointer
       init_state.frame.rbp_pointer
       n; *)
    if State.is_stack_will_be_aligned n init_state
    then (
      let offset, state = State.push_rsp_n n init_state in
      let cmd, state = increase_rsp state in
      (* printf
         "prepare_stack1: rsp %d rbp %d\n"
         state.frame.rsp_pointer
         state.frame.rbp_pointer; *)
      return (init_state.frame.rsp_pointer, offset, Result.ok_exn cmd) state)
    else (
      let offset, state = State.push_rsp_n (n + 1) init_state in
      (* printf
         "prepare_stack2: rsp %d rbp %d\n"
         state.frame.rsp_pointer
         state.frame.rbp_pointer; *)
      let cmd, state = increase_rsp state in
      (* printf
         "prepare_stack3: rsp %d rbp %d offset %d\n"
         state.frame.rsp_pointer
         state.frame.rbp_pointer
         offset; *)
      return (init_state.frame.rsp_pointer, offset + 8, Result.ok_exn cmd) state)
  ;;

  let push_arg : int -> asm_value -> (command, 'error) t =
    fun offset arg state ->
    let bind_to_stack = sprintf "qword [%s -%d]" R.rbp offset in
    match arg with
    | Reg _ | Int _ -> mov_cmd_unsave bind_to_stack arg state
    | Label _ -> failwith "TODO: push_arg"
    | StackMem _ -> failwith "TODO: push_arg"
  ;;

  let push_reg : int -> register -> (command, 'error) t =
    fun offset reg ->
    let bind_to_stack = sprintf "qword [%s -%d]" R.rbp offset in
    mov_cmd_unsave bind_to_stack (Reg reg)
  ;;

  let pop_reg : int -> register -> (command, 'error) t =
    fun offset reg ->
    let bind_to_stack = sprintf "qword [%s -%d]" R.rbp offset in
    mov_cmd_unsave reg (StackMem bind_to_stack)
  ;;

  (* Working with functions *)

  let push_args_to_stack init_offset args =
    (* printf "push_args_to_stack init_offest: %d\n" init_offset; *)
    List.fold
      ~init:(return (init_offset, []))
      ~f:(fun acc arg ->
        let* offset, acc = acc in
        let* p = push_arg offset arg in
        return (offset + 8, p :: acc))
      (List.rev args)
  ;;

  let mov_args_to_regs args =
    let rec helper l1 l2 acc =
      match l1, l2 with
      | [], _ -> return acc
      | h1 :: t1, h2 :: t2 ->
        let* mov = mov_cmd h2 h1 in
        (* На самом деле здесь ОЧЕНЬ важен порядок складывания, он должео быть именно такой *)
        helper t1 t2 (acc @ mov)
      | _ :: _, [] -> fail @@ TryToStoreInRegsMoreThanSixArguments
    in
    helper args R.regs_for_args []
  ;;

  let save_caller_regs : (command list, 'error) t =
    fun state ->
    let rec helper cmds state = function
      | [] ->
        let state = state in
        return cmds state
      | hd :: tl ->
        let offset, state = State.push_rsp state in
        (* printf "(%d) rsp = %d rbp = %d\n" (List.length cmds) rsp state.frame.rbp_pointer; *)
        let cmd, state = push_reg offset hd state in
        helper (Result.ok_exn cmd :: cmds) state tl
    in
    let res, state = helper [] state R.caller_saved in
    (* printf "save_caller_regs: rsp=%d\n" state.frame.rsp_pointer; *)
    res, state
  ;;

  let restore_caller_regs : (command list, 'error) t =
    fun state ->
    let rec helper cmds state = function
      | [] -> return cmds state
      | hd :: tl ->
        let offset, state = State.pop_rsp state in
        (* printf "(%d) rsp = %d rbp = %d\n" (List.length cmds) rsp state.frame.rbp_pointer; *)
        let cmd, state = pop_reg offset hd state in
        helper (Result.ok_exn cmd :: cmds) state tl
    in
    helper [] state (List.rev R.caller_saved)
  ;;

  (* PUBLIC *)
  let make_func_call f_label arguments =
    match f_label with
    | Label f_label ->
      let to_regs, to_stack = List.split_n arguments R.number_of_args_saved_to_regs in
      let* cmds_to_regs = mov_args_to_regs to_regs in
      let num_of_args = List.length to_stack in
      let* comm1 = make_comment "Start. Caller save register" in
      let* cmds_caller_save = save_caller_regs in
      let* comm2 = make_comment "End. Caller save register" in
      let* previouts_rsp, from_where_do_push_args, cmds_prepare_stack =
        prepare_stack num_of_args
      in
      let* _, cmds_to_stack = push_args_to_stack from_where_do_push_args to_stack in
      let* cmd_call = call_cmd f_label in
      let* cmd_restore_stack = restore_rsp previouts_rsp in
      let* comm3 = make_comment "Start. Restore caller save registers" in
      let* cmds_caller_restore = restore_caller_regs in
      let* comm4 = make_comment "End. Restore caller save registers" in
      return
      @@ List.concat
           [ cmds_to_regs
           ; comm1 :: cmds_caller_save
           ; comm2 :: cmds_prepare_stack :: cmds_to_stack
           ; [ cmd_call; cmd_restore_stack ]
           ; comm3 :: cmds_caller_restore
           ; [ comm4 ]
           ]
    | _ -> fail (CanNotMakeCallTo f_label)
  ;;

  let args_to_reg : string list -> (State.t, error) t =
    fun l1 state ->
    let rec helper state = function
      | [], _ -> return state state
      | h1 :: t1, h2 :: t2 -> helper (State.add_local h1 h2 state) (t1, t2)
      | _ -> fail TryToStoreInRegsMoreThanSixArguments state
    in
    helper state (l1, R.regs_for_args)
  ;;

  let args_to_stack : string list -> (State.t, error) t =
    fun args state ->
    let state, _ =
      List.fold
        ~init:(state, 2)
        ~f:(fun (state, i) arg ->
          let bind_name = sprintf "qword [%s +%d]" R.rbp (8 * i) in
          State.add_local arg bind_name state, i + 1)
        args
    in
    return state state
  ;;

  (* PUBLIC *)
  let make_func_preambula : string -> string list -> (string list, error) t =
    fun f_name args ->
    let* _ = save_frame in
    let* label = make_label f_name in
    let* cmd_label = make_label_cmd label in
    let in_regs, in_stack = List.split_n args R.number_of_args_saved_to_regs in
    let* _ = args_to_reg in_regs in
    let* _ = args_to_stack in_stack in
    let* cmd_save_rbp = save_rbp in
    let* cmd_mov = mov_cmd_unsave R.rbp (Reg R.rsp) in
    return [ cmd_label; cmd_save_rbp; cmd_mov ]
  ;;

  (* PUBLIC *)
  let make_func_ret =
    let* cmd_pop = pop_rbp in
    let* cmd_ret = ret_cmd ?comment:None in
    let* _ = restore_frame in
    return [ cmd_pop; cmd_ret ]
  ;;

  let start_branch : (unit, 'a) t =
    fun state ->
    let state = State.push_if_frame state in
    return () state
  ;;

  let end_branch state =
    let state = State.pop_if_frame state in
    return () state
  ;;

  (* PUBLIC *)
  let save_return_value var =
    let* save_var_cmd = save_from_reg_to_var ~comment:"save regturn value" R.rax var in
    return save_var_cmd
  ;;

  let bool_to_int b = if b then 1 else 0
  let make_bool boolean = return (Int (sprintf "%d" @@ bool_to_int boolean))
  let make_integer num = return (Int (sprintf "%d" num))
  let reg_to_asmvalue reg = Reg reg
  let label_to_asmvalue label = Label label

  (* Compile program from commands *)

  let commands_to_str l = String.concat l

  let rec pp_commands ppf = function
    | [] -> fprintf ppf ""
    | hd :: tl -> fprintf ppf "%s%a" hd pp_commands tl
  ;;

  let commands_to_file filename l =
    let out = open_out filename in
    List.iter ~f:(Stdlib.Printf.fprintf out "%s") l;
    close_out out
  ;;
end
