(** Copyright 2023-2024, Vyacheslav Buchin and Artur Gagin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(* Source: https://github.com/dmbaturin/bnfgen/blob/master/src/lib/parse_bnf.ml and
   https://github.com/dmbaturin/bnfgen/blob/master/src/lib/bnfgen.ml such as util.ml *)

open Util
open Lexing
open Parsed_tree
module I = Parser.MenhirInterpreter

let get_parse_error env =
  match I.stack env with
  | (lazy Nil) -> "Invalid syntax"
  | (lazy (Cons (I.Element (state, _, _, _), _))) ->
    (try Parser_messages.message (I.number state) with
     | Not_found -> "invalid syntax (no specific message for this error)")
;;

let rec parse lexbuf checkpoint =
  match checkpoint with
  | I.InputNeeded _env ->
    let token = Lexer.token lexbuf in
    let startp = lexbuf.lex_start_p
    and endp = lexbuf.lex_curr_p in
    let checkpoint = I.offer checkpoint (token, startp, endp) in
    parse lexbuf checkpoint
  | I.Shifting _ | I.AboutToReduce _ ->
    let checkpoint = I.resume checkpoint in
    parse lexbuf checkpoint
  | I.HandlingError _env ->
    let line, pos = Util.get_lexing_position lexbuf in
    let err = get_parse_error _env in
    raise (Syntax_error (Some (line, pos), err))
  | I.Accepted v -> v
  | I.Rejected ->
    raise (Syntax_error (None, "Invalid syntax (parser rejected the input)"))
;;

let parse lexbuf =
  try
    let grammar = parse lexbuf (Parser.Incremental.exp lexbuf.lex_curr_p) in
    Ok grammar
  with
  | Util.Syntax_error (pos, err) ->
    (match pos with
     | Some (line, pos) ->
       Error (Printf.sprintf "Syntax error on line %d, character %d: %s" line pos err)
     | None -> Error (Printf.sprintf "Syntax error: %s" err))
  | Parsed_tree_error msg -> Error (Printf.sprintf "Grammar error: %s" msg)
;;

let grammar_from_channel ic =
  let lexbuf = Lexing.from_channel ic in
  parse lexbuf
;;

let grammar_from_file filename =
  let ic = open_in filename in
  let g = grammar_from_channel ic in
  let () = close_in ic in
  g
;;

let grammar_from_string s =
  let lexbuf = Lexing.from_string s in
  parse lexbuf
;;
