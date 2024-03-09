(** Copyright 2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

module VariableNameGeneratorMonad : sig
  type 'a t

  val return : 'a -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val fresh : string -> string t
  val run : 'a t -> 'a
  val monad_fold : init:'a -> f:('a -> 'b -> 'a t) -> 'b list -> 'a t
end

module CompilerMonad : sig
  open Format

  type register
  type asm_value
  type label
  type command
  type error
  type ('a, 'error) t

  val pp_error : formatter -> error -> unit

  (* Standard monad funcitons *)

  val return : 'a -> ('a, 'error) t
  val fail : 'error -> ('a, 'error) t
  val bind : ('a, 'error) t -> ('a -> ('b, 'error) t) -> ('b, 'error) t
  val ( let* ) : ('a, 'error) t -> ('a -> ('b, 'error) t) -> ('b, 'error) t
  val fresh : string -> (string, 'error) t
  val monad_map : 'a list -> f:('a -> ('b, 'c) t) -> ('b list, 'c) t
  val run : ('a, 'error) t -> ('a, 'error) result

  (* Registers *)

  module R : sig
    val rax : register
    val rbx : register
    val rcx : register
    val rdx : register
    val rsi : register
    val rdi : register
    val r8 : register
    val r9 : register
    val r10 : register
    val r11 : register
    val r12 : register
    val r13 : register
    val r14 : register
    val r15 : register
  end

  (* Comands with two arguments *)
  val mov_cmd
    :  ?comment:string option
    -> register
    -> asm_value
    -> (command list, 'error) t

  val add_cmd : ?comment:string option -> register -> asm_value -> (command list, error) t
  val sub_cmd : ?comment:string option -> register -> asm_value -> (command list, error) t
  val idiv_cmd : ?comment:string option -> asm_value -> (command list, error) t

  val imul_cmd
    :  ?comment:string option
    -> register
    -> asm_value
    -> (command list, error) t

  val xor_cmd : ?comment:string option -> register -> asm_value -> (command list, error) t
  val and_cmd : ?comment:string option -> register -> asm_value -> (command list, error) t
  val or_cmd : ?comment:string option -> register -> asm_value -> (command list, error) t
  val cmp_cmd : ?comment:string option -> register -> asm_value -> (command list, error) t

  (* Comands with one arguments *)
  val sete_cmd : ?comment:string option -> register -> (command list, error) t
  val setne_cmd : ?comment:string option -> register -> (command list, error) t
  val setg_cmd : ?comment:string option -> register -> (command list, error) t
  val setl_cmd : ?comment:string option -> register -> (command list, error) t
  val setge_cmd : ?comment:string option -> register -> (command list, error) t
  val setle_cmd : ?comment:string option -> register -> (command list, error) t
  val jmp_cmd : ?comment:string option -> label -> (command, error) t
  val jne_cmd : ?comment:string option -> label -> (command, error) t
  val je_cmd : ?comment:string option -> label -> (command, error) t

  (* Command without arguments *)
  val cqo_cmd : ?comment:string option -> (command, error) t

  (* Some commands *)
  val make_comment : string -> (command, 'error) t
  val add_global : label -> (command, error) t
  val add_extern : label -> (command, error) t

  (* Helper commands *)
  val make_label : string -> (label, 'error) t
  val get_data_from_environemnt : string -> (asm_value, error) t

  (* val find_label : string -> (label option, error) t *)
  val make_label_cmd : label -> (command, error) t
  val make_func_call : asm_value -> asm_value list -> (command list, error) t
  val make_func_preambula : string -> string list -> (command list, error) t
  val make_func_ret : (command list, error) t
  val save_return_value : string -> (command, error) t
  val make_bool : bool -> (asm_value, error) t
  val make_integer : int -> (asm_value, error) t
  val reg_to_asmvalue : register -> asm_value
  val label_to_asmvalue : label -> asm_value

  (* Compile commands *)
  val commands_to_str : command list -> string
  val pp_commands : formatter -> command list -> unit
  val commands_to_file : string -> command list -> unit

  (* If commands *)
  val start_branch : (unit, 'a) t
  val end_branch : (unit, 'a) t
end
