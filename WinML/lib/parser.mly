(** Copyright 2023-2024, Vyacheslav Buchin and Artur Gagin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(* Generate list-errors: menhir --list-errors bin/parser.mly > bin/parser.messages *)

%{
open Parsed_tree
%}

%token <int> INT
(* %token <bool> BOOL *)
%token LET
%token IF
%token THEN
%token ELSE
%token REC
(* %token IN *)
(* %token SEMICOLON *)
(* %right SEMICOLON *)
%token LPAREN
%token RPAREN
(* %token NOT *)
%token EQUAL
%token EQUAL_EQUAL
%left  EQUAL_EQUAL 
(* %left NOT_EQUAL LESS GREATER LESS_EQUAL GREATER_EQUAL *)
%token MINUS
%left MINUS
%token PLUS
%left  PLUS
%token MULT
%left  MULT

(* %token DIV *)
(* %left DIV *)
(* %token NOT_EQUAL *)
(* %token LESS_EQUAL *)
(* %token GREATER_EQUAL *)
(* %token LESS *)
(* %token GREATER *)
%token <string> IDENT

(* %token PREC_LET *)
(* %right PREC_LET *)
(* %token PREC_IF *)
(* %right PREC_IF *)
(* %token PREC_UNARY_MINUS *)
(* %right PREC_UNARY_MINUS *)
(* %token PREC_UNARY_NOT *)
(* %right PREC_UNARY_NOT *)

%token EOF

%type <Parsed_tree.func> exp
%start exp

%%
exp:
    (* | LET; IDENT; EQUAL; expr *)
    (* | LET; IDENT; EQUAL; expr; IN; expr *)
    (* | LET; func *)
    (* | LET; func; IN; expr *)
    (* | LET; REC; func; IN; expr*)
    | LET; REC; func; EOF                    { RecFunction($3) }

_expr:
    | IDENT                                  { Var $1 }
    | INT                                    { Const(CInt $1) }
    | LPAREN expr RPAREN                     { $2 }
    (* | LPAREN RPAREN                       { Unit } *)
    (* | BOOL                                { Bool $1 } *)

expr:
    | _expr                                  { $1 }    
    | IF; expr; THEN; expr; ELSE; expr       { IfThenElse($2, $4, $6) }
    (* | expr; SEMICOLON; expr *)
    (* | NOT; expr                           { Not $2 } %prec PREC_UNARY_NOT *)
    | expr; EQUAL_EQUAL; expr                { BinOp (Eq, $1, $3) }
    | expr; PLUS; expr                       { BinOp (Add, $1, $3) }
    | expr; MINUS; expr                      { BinOp (Sub, $1, $3) }
    (* | MINUS; expr                         { Neg $2 } %prec PREC_UNARY_MINUS *)
    (* | expr; NOT_EQUAL; expr               { Not (Eq ($1, $3)) } *)
    (* | expr; LESS_EQUAL; expr              { Le ($1, $3) } *)
    (* | expr; GREATER_EQUAL; expr           { Le($3, $1) } *)
    (* | expr; LESS; expr                    { Not (Le ($3, $1)) } *)
    (* | expr; GREATER; expr                 { Not (Le ($1, $3)) } *) 
    | expr; MULT;   expr                     { BinOp (Mul, $1, $3) }
    (* | expr; DIV;   expr                   { Div ($1, $3) } *)
    | expr; _expr; app_args                  { Application($1, $2, $3) }
    | expr; _expr                            { Application($1, $2, []) }

func:
    | IDENT; func_args; EQUAL; expr          { ($1, $2, $4) }

func_args:
    | IDENT; func_args                       { ($1) :: $2 }
    | IDENT                                  { [$1] }

app_args:
    | app_args; _expr                        { $1 @ [$2] }
    | _expr                                  { [$1] }
