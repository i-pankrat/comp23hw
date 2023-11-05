(** Copyright 2023-2024, Vyacheslav Buchin and Artur Gagin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

{
  open Lexing
  open Parser
}

let digit = ['0' - '9']
let space = [' ' '\t' '\n' '\r']
let const_int = digit+

let ident = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
 | space+  { token lexbuf }
 | '('     { LPAREN }
 | ')'     { RPAREN }
 | const_int   { INT (int_of_string (lexeme lexbuf)) }
 (* | "true"  { BOOL(true) } *)
 (* | "false" { BOOL(false) } *)
 (* | "in"    { IN } *)
 (* | ";"     { SEMICOLON } *)
 (* | "not"   { NOT } *)
 | '-'     { MINUS }
 | '+'     { PLUS }
 | '*'     { MULT }
 | '='     { EQUAL }
 | "=="    { EQUAL_EQUAL }
 | "if"    { IF }
 | "then"  { THEN }
 | "else"  { ELSE }
 | "let"   { LET }
 | "rec"   { REC }
 (* | '/'     { DIV } *)
 (* | "!="    { NOT_EQUAL } *)
 (* | "<="    { LESS_EQUAL } *)
 (* | ">="    { GREATER_EQUAL } *)
 (* | '<'     { LESS } *)
 (* | '>'     { GREATER } *)
 | ident   { IDENT (lexeme lexbuf) }
 | eof     { EOF }
 | _ { raise (Failure ("Unexpected token " ^ Lexing.lexeme lexbuf)) }
