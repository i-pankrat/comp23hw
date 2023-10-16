open Angstrom
open Ast
open List

type error_message = string
type input = string

type dispatch =
  { parse_list_constructing : dispatch -> expression Angstrom.t
  ; parse_tuple : dispatch -> expression Angstrom.t
  ; parse_binary_operation : dispatch -> expression Angstrom.t
  ; parse_unary_operation : dispatch -> expression Angstrom.t
  ; parse_list : dispatch -> expression Angstrom.t
  ; parse_application : dispatch -> expression Angstrom.t
  ; parse_fun : dispatch -> expression Angstrom.t
  ; parse_conditional : dispatch -> expression Angstrom.t
  ; parse_matching : dispatch -> expression Angstrom.t
  ; parse_let_in : dispatch -> expression Angstrom.t
  ; parse_expression : dispatch -> expression Angstrom.t
  ; parse_wildcard : pattern Angstrom.t
  ; parse_ptuple : dispatch -> pattern Angstrom.t
  ; parse_plist : dispatch -> pattern Angstrom.t
  ; parse_pconstruct_list : dispatch -> pattern Angstrom.t
  ; parse_pliteral : pattern Angstrom.t
  ; parse_pidentifier : pattern Angstrom.t
  }

(* ------------------------------- *)

(* Helpers *)
let space_predicate x = x == ' ' || x == '\n' || x == '\t' || x == '\r'
let remove_spaces = take_while space_predicate
let parens parser = remove_spaces *> char '(' *> parser <* remove_spaces <* char ')'

let parse_entity =
  fix
  @@ fun self ->
  remove_spaces
  *> (parens self
     <|> take_while1 (function
           | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
           | _ -> false))
;;

let parse_uncapitalized_entity =
  parse_entity
  >>= fun entity ->
  match entity.[0] with
  | 'a' .. 'z' | '_' -> return entity
  | _ -> fail "Parsing error: not an uncapitalized entity."
;;

let parse_capitalized_entity =
  parse_entity
  >>= fun entity ->
  match entity.[0] with
  | 'A' .. 'Z' -> return entity
  | _ -> fail "Parsing error: not a capitalized entity."
;;

let keywords =
  [ "let"; "rec"; "match"; "with"; "if"; "then"; "else"; "in"; "fun"; "and"; "type" ]
;;

(* ------- *)

(* Patterns parsers *)

let parse_pattern d =
  choice
    [ d.parse_ptuple d
    ; d.parse_pconstruct_list d
    ; d.parse_plist d
    ; d.parse_pliteral
    ; d.parse_pidentifier
    ; d.parse_wildcard
    ]
;;

let parse_literal constructor =
  fix
  @@ fun self ->
  remove_spaces
  *> (parens self
     <|>
     let is_digit = function
       | '0' .. '9' -> true
       | _ -> false
     in
     let parse_int_literal = take_while1 is_digit >>| int_of_string >>| lint in
     let parse_string_literal =
       char '"' *> take_while (( != ) '"') <* char '"' >>| lstring
     in
     let parse_char_literal = char '\'' *> any_char <* char '\'' >>| lchar in
     let parse_bool_literal =
       string "true" <|> string "false" >>| bool_of_string >>| lbool
     in
     let parse_unit_literal = string "()" >>| lunit in
     let parse_literal =
       choice
         [ parse_int_literal
         ; parse_string_literal
         ; parse_char_literal
         ; parse_bool_literal
         ; parse_unit_literal
         ]
     in
     constructor <$> parse_literal)
;;

let parse_pliteral = parse_literal pliteral
let parse_literal = parse_literal eliteral

let parse_pidentifier =
  fix
  @@ fun _ ->
  remove_spaces
  *>
  let parse_identifier =
    parse_uncapitalized_entity
    >>= fun entity ->
    if List.exists (( = ) entity) keywords
    then fail "Parsing error: keyword used."
    else if String.for_all (fun c -> c = '_') entity
    then fail "Parsing error: this is a wildcard."
    else return @@ pidentifier entity
  in
  parse_identifier
;;

let parse_wildcard =
  fix
  @@ fun self ->
  parens self <|> remove_spaces *> (pwildcard <$> take_while1 (fun c -> c = '_'))
;;

let parse_ptuple d =
  fix
  @@ fun self ->
  (remove_spaces
  *>
  let separator = remove_spaces *> char ',' *> remove_spaces in
  let parse_pattern =
    choice
      [ parens self
      ; d.parse_pconstruct_list d
      ; d.parse_wildcard
      ; d.parse_plist d
      ; d.parse_pliteral
      ; d.parse_pidentifier
      ]
  in
  lift2
    ptuple
    (parse_pattern <* separator)
    (sep_by1 separator parse_pattern <* remove_spaces))
  <|> parens self
;;

let parse_plist d =
  fix
  @@ fun self ->
  remove_spaces
  *>
  let brackets parser = char '[' *> parser <* char ']'
  and separator = remove_spaces *> char ';' *> remove_spaces <|> remove_spaces in
  parens self
  <|> (plist <$> brackets @@ (remove_spaces *> many (parse_pattern d <* separator)))
;;

let parse_pconstruct_list d =
  fix
  @@ fun self ->
  remove_spaces
  *> (parens self
     <|>
     let parse_pattern =
       choice
         [ parens @@ d.parse_ptuple d
         ; parens self
         ; d.parse_wildcard
         ; d.parse_plist d
         ; d.parse_pliteral
         ; d.parse_pidentifier
         ]
     in
     let separator = remove_spaces *> string "::" *> remove_spaces in
     lift2 pconstruct_list (parse_pattern <* separator) (self <|> parse_pattern))
;;

(* Parsers *)

let parse_identifier =
  fix
  @@ fun _ ->
  remove_spaces
  *>
  let parse_identifier =
    parse_uncapitalized_entity
    >>= fun entity ->
    if List.exists (( = ) entity) keywords
    then fail "Parsing error: keyword used."
    else return @@ eidentifier entity
  in
  parse_identifier
;;

let parse_tuple d =
  fix
  @@ fun self ->
  remove_spaces
  *> ((let parse_content =
         choice
           [ parens self
           ; d.parse_list_constructing d
           ; d.parse_binary_operation d
           ; d.parse_unary_operation d
           ; d.parse_list d
           ; d.parse_application d
           ; d.parse_fun d
           ; d.parse_conditional d
           ; d.parse_matching d
           ; d.parse_let_in d
           ; parse_literal
           ; parse_identifier
           ]
       and separator = remove_spaces *> char ',' *> remove_spaces in
       lift2
         etuple
         (parse_content <* separator)
         (sep_by1 separator parse_content <* remove_spaces))
     <|> parens self)
;;

let parse_list d =
  fix
  @@ fun self ->
  remove_spaces
  *>
  let brackets parser = char '[' *> parser <* char ']'
  and separator = remove_spaces *> char ';' *> remove_spaces <|> remove_spaces
  and parse_content =
    choice
      [ d.parse_tuple d
      ; d.parse_list_constructing d
      ; d.parse_binary_operation d
      ; d.parse_unary_operation d
      ; self
      ; d.parse_application d
      ; d.parse_fun d
      ; d.parse_conditional d
      ; d.parse_matching d
      ; d.parse_let_in d
      ; parse_literal
      ; parse_identifier
      ]
  in
  parens self
  <|> lift elist @@ brackets @@ (remove_spaces *> many (parse_content <* separator))
;;

let parse_fun d =
  fix
  @@ fun self ->
  remove_spaces
  *>
  let parse_content =
    choice
      [ d.parse_tuple d
      ; d.parse_list_constructing d
      ; d.parse_binary_operation d
      ; d.parse_unary_operation d
      ; d.parse_list d
      ; d.parse_application d
      ; self
      ; d.parse_conditional d
      ; d.parse_matching d
      ; d.parse_let_in d
      ; parse_literal
      ; parse_identifier
      ]
  in
  parens self
  <|> string "fun"
      *> lift2
           efun
           (many1 (parse_pattern d) <* remove_spaces <* string "->" <* remove_spaces)
           (parse_content <* remove_spaces)
;;

(* Used in parse_declaration and parse_let_in *)
let declaration_helper constructing_function d =
  let parse_content =
    choice
      [ d.parse_tuple d
      ; d.parse_list_constructing d
      ; d.parse_binary_operation d
      ; d.parse_unary_operation d
      ; d.parse_list d
      ; d.parse_application d
      ; d.parse_fun d
      ; d.parse_conditional d
      ; d.parse_matching d
      ; d.parse_let_in d
      ; parse_literal
      ; parse_identifier
      ]
  in
  lift3
    constructing_function
    (parse_uncapitalized_entity
    >>= fun name ->
    if name = "_" then fail "Parsing error: wildcard not expected." else return name)
    (many (parse_pattern d))
    (remove_spaces *> string "=" *> parse_content)
;;

let parse_declaration d =
  fix
  @@ fun _ ->
  remove_spaces
  *> string "let"
  *> take_while1 space_predicate
  *> option "" (string "rec" <* take_while1 space_predicate)
  >>= function
  | "rec" -> declaration_helper drecursivedeclaration d
  | _ -> declaration_helper ddeclaration d
;;

let parse_let_in d =
  fix
  @@ fun self ->
  remove_spaces
  *>
  let parse_content =
    choice
      [ d.parse_tuple d
      ; d.parse_list_constructing d
      ; d.parse_binary_operation d
      ; d.parse_unary_operation d
      ; d.parse_list d
      ; d.parse_application d
      ; d.parse_fun d
      ; d.parse_conditional d
      ; d.parse_matching d
      ; self
      ; parse_literal
      ; parse_identifier
      ]
  in
  parens self
  <|> (string "let"
       *> take_while1 space_predicate
       *> option "" (string "rec" <* take_while1 space_predicate)
      >>= fun parsed_rec ->
      lift2
        eletin
        (let separator = remove_spaces *> string "and" *> take_while1 space_predicate in
         match parsed_rec with
         | "rec" -> sep_by1 separator @@ declaration_helper drecursivedeclaration d
         | _ -> sep_by1 separator @@ declaration_helper ddeclaration d)
        (remove_spaces *> string "in" *> parse_content))
;;

let parse_conditional d =
  fix
  @@ fun self ->
  remove_spaces
  *>
  let parse_content =
    choice
      [ d.parse_tuple d
      ; d.parse_list_constructing d
      ; d.parse_binary_operation d
      ; d.parse_unary_operation d
      ; d.parse_list d
      ; d.parse_application d
      ; d.parse_fun d
      ; self
      ; d.parse_matching d
      ; d.parse_let_in d
      ; parse_literal
      ; parse_identifier
      ]
  in
  parens self
  <|> string "if"
      *> lift3
           eif
           parse_content
           (remove_spaces *> string "then" *> parse_content)
           (remove_spaces *> string "else" *> parse_content)
;;

let parse_matching d =
  fix
  @@ fun self ->
  remove_spaces
  *>
  let parse_content_right =
    choice
      [ d.parse_tuple d
      ; d.parse_list_constructing d
      ; d.parse_binary_operation d
      ; d.parse_unary_operation d
      ; d.parse_list d
      ; d.parse_application d
      ; d.parse_fun d
      ; d.parse_conditional d
      ; self
      ; d.parse_let_in d
      ; parse_literal
      ; parse_identifier
      ]
  in
  parens self
  <|> string "match"
      *> lift2
           ematchwith
           parse_content_right
           (let parse_case =
              lift2
                (fun case action -> case, action)
                (parse_pattern d)
                (remove_spaces *> string "->" *> parse_content_right)
            and separator = remove_spaces *> string "|" in
            remove_spaces
            *> string "with"
            *> remove_spaces
            *> (string "|" <|> remove_spaces)
            *> sep_by1 separator parse_case)
;;

let parse_binary_operation d =
  fix
  @@ fun self ->
  remove_spaces
  *>
  let multiplicative = remove_spaces *> choice [ char '*' >>| bmul; char '/' >>| bdiv ]
  and additive = remove_spaces *> choice [ char '+' >>| badd; char '-' >>| bsub ]
  and relational =
    remove_spaces
    *> choice
         [ string ">=" >>| bgte
         ; string "<=" >>| blte
         ; char '>' >>| bgt
         ; char '<' >>| blt
         ]
  and equality =
    remove_spaces *> choice [ string "=" >>| beq; string "!=" <|> string "<>" >>| bneq ]
  and logical_and = remove_spaces *> (string "&&" >>| band)
  and logical_or = remove_spaces *> (string "||" >>| bor) in
  let chainl1 expression_parser operation_parser =
    let rec go acc =
      lift2
        (fun binary_operator right_operand ->
          ebinary_operation binary_operator acc right_operand)
        operation_parser
        expression_parser
      >>= go
      <|> return acc
    in
    expression_parser >>= fun init -> go init
  in
  let ( <||> ) = chainl1 in
  let parse_content =
    choice
      [ parens self
      ; d.parse_list_constructing d
      ; d.parse_unary_operation d
      ; d.parse_list d
      ; d.parse_application d
      ; d.parse_conditional d
      ; d.parse_matching d
      ; d.parse_let_in d
      ; parse_literal
      ; parse_identifier
      ]
  in
  parse_content
  <||> multiplicative
  <||> additive
  <||> relational
  <||> equality
  <||> logical_and
  <||> logical_or
  >>= fun result ->
  match result with
  | EBinaryOperation (_, _, _) -> return result
  | _ -> fail "Parsing error: not binary operation."
;;

let parse_application d =
  fix
  @@ fun self ->
  remove_spaces
  *> (parens self
     <|>
     let function_parser =
       choice
         [ parens @@ d.parse_fun d
         ; parens @@ d.parse_conditional d
         ; parens @@ d.parse_matching d
         ; parens @@ d.parse_let_in d
         ; parse_identifier
         ]
     and operand_parser =
       choice
         [ parens @@ d.parse_tuple d
         ; parens @@ d.parse_list_constructing d
         ; parens @@ d.parse_binary_operation d
         ; parens @@ d.parse_unary_operation d
         ; d.parse_list d
         ; parens self
         ; parens @@ d.parse_fun d
         ; parens @@ d.parse_conditional d
         ; parens @@ d.parse_matching d
         ; parens @@ d.parse_let_in d
         ; parse_literal
         ; parse_identifier
         ]
     in
     let apply_lift acc = lift (eapplication acc) operand_parser in
     let rec go acc = apply_lift acc >>= go <|> return acc in
     function_parser >>= fun init -> apply_lift init >>= fun init -> go init)
;;

let parse_unary_operation d =
  fix
  @@ fun self ->
  remove_spaces
  *>
  let parse_content_minus =
    let indent = many1 (satisfy space_predicate) in
    choice
      [ parens self <|> indent *> self
      ; parens @@ d.parse_application d <|> indent *> d.parse_application d
      ; parens @@ d.parse_conditional d <|> indent *> d.parse_conditional d
      ; parens @@ d.parse_matching d <|> indent *> d.parse_matching d
      ; parens @@ d.parse_let_in d <|> indent *> d.parse_let_in d
      ; parse_literal
      ; parse_identifier
      ; parens @@ d.parse_binary_operation d
      ]
  and parse_content_not =
    choice
      [ parens @@ d.parse_binary_operation d
      ; parens self
      ; parens @@ d.parse_application d
      ; parens @@ d.parse_conditional d
      ; parens @@ d.parse_matching d
      ; parens @@ d.parse_let_in d
      ; parse_literal
      ; parse_identifier
      ]
  in
  parens self
  <|> lift2 eunary_operation (char '-' >>| uminus) parse_content_minus
  <|> lift2 eunary_operation (string "not" >>| unot) parse_content_not
;;

let parse_list_constructing d =
  fix
  @@ fun self ->
  remove_spaces
  *> (parens self
     <|>
     let separator = remove_spaces *> string "::" *> remove_spaces
     and parse_content =
       choice
         [ parens @@ d.parse_tuple d
         ; parens self
         ; parens @@ d.parse_binary_operation d
         ; d.parse_unary_operation d
         ; d.parse_list d
         ; d.parse_application d
         ; parens @@ d.parse_fun d
         ; parens @@ d.parse_conditional d
         ; parens @@ d.parse_matching d
         ; d.parse_let_in d
         ; parse_literal
         ; parse_identifier
         ]
     in
     lift2 econstruct_list (parse_content <* separator) (self <|> parse_content))
;;

(* --------------------------- *)

let parse_expression d =
  choice
    [ d.parse_tuple d
    ; d.parse_list_constructing d
    ; d.parse_binary_operation d
    ; d.parse_unary_operation d
    ; d.parse_list d
    ; d.parse_application d
    ; d.parse_fun d
    ; d.parse_conditional d
    ; d.parse_matching d
    ; d.parse_let_in d
    ; parse_literal
    ; parse_identifier
    ]
;;

let default =
  { parse_list_constructing
  ; parse_tuple
  ; parse_binary_operation
  ; parse_unary_operation
  ; parse_list
  ; parse_application
  ; parse_fun
  ; parse_conditional
  ; parse_matching
  ; parse_let_in
  ; parse_expression
  ; parse_wildcard
  ; parse_ptuple
  ; parse_plist
  ; parse_pconstruct_list
  ; parse_pliteral
  ; parse_pidentifier
  }
;;

let parse_tuple = parse_tuple default
let parse_list = parse_list default
let parse_fun = parse_fun default
let parse_declaration = parse_declaration default
let parse_conditional = parse_conditional default
let parse_matching = parse_matching default
let parse_binary_operation = parse_binary_operation default
let parse_let_in = parse_let_in default
let parse_application = parse_application default
let parse_unary_operation = parse_unary_operation default
let parse_list_constructing = parse_list_constructing default
let parse_expression = parse_expression default
let parse_ptuple = parse_ptuple default
let parse_plist = parse_plist default
let parse_pconstruct_list = parse_pconstruct_list default

(* Main parsing function *)
let parse : input -> (declaration list, error_message) result =
 fun program ->
  parse_string ~consume:All (many parse_declaration <* remove_spaces) program
;;

(* -------------------- TESTS -------------------- *)

(* 1 *)
let%test _ =
  parse
    "let rec factorial n acc = if n <= 1 then acc else factorial (n - 1) (acc * n)\n\
     let main = factorial 5 1 "
  = Result.ok
    @@ [ DRecursiveDeclaration
           ( "factorial"
           , [ PIdentifier "n"; PIdentifier "acc" ]
           , EIf
               ( EBinaryOperation (LTE, EIdentifier "n", ELiteral (LInt 1))
               , EIdentifier "acc"
               , EApplication
                   ( EApplication
                       ( EIdentifier "factorial"
                       , EBinaryOperation (Sub, EIdentifier "n", ELiteral (LInt 1)) )
                   , EBinaryOperation (Mul, EIdentifier "acc", EIdentifier "n") ) ) )
       ; DDeclaration
           ( "main"
           , []
           , EApplication
               ( EApplication (EIdentifier "factorial", ELiteral (LInt 5))
               , ELiteral (LInt 1) ) )
       ]
;;

(* 2 *)
let%test _ =
  parse " let main = 1 :: 2 :: [] "
  = Result.ok
    @@ [ DDeclaration
           ( "main"
           , []
           , EConstructList
               (ELiteral (LInt 1), EConstructList (ELiteral (LInt 2), EList [])) )
       ]
;;

(* 3 *)
let%test _ =
  parse " let main = true :: (false) :: [false] "
  = Result.ok
    @@ [ DDeclaration
           ( "main"
           , []
           , EConstructList
               ( ELiteral (LBool true)
               , EConstructList (ELiteral (LBool false), EList [ ELiteral (LBool false) ])
               ) )
       ]
;;

(* 4 *)
let%test _ =
  parse " let main = (10 + 20) :: [30; 4 * 10] "
  = Result.ok
    @@ [ DDeclaration
           ( "main"
           , []
           , EConstructList
               ( EBinaryOperation (Add, ELiteral (LInt 10), ELiteral (LInt 20))
               , EList
                   [ ELiteral (LInt 30)
                   ; EBinaryOperation (Mul, ELiteral (LInt 4), ELiteral (LInt 10))
                   ] ) )
       ]
;;

(* 5 *)
let%test _ =
  parse " let main = (fun x -> 'a') :: [fun _ -> 'b'] "
  = Result.ok
    @@ [ DDeclaration
           ( "main"
           , []
           , EConstructList
               ( EFun ([ PIdentifier "x" ], ELiteral (LChar 'a'))
               , EList [ EFun ([ PWildcard ], ELiteral (LChar 'b')) ] ) )
       ]
;;

(* 6 *)
let%test _ =
  parse " let main = () :: (()) :: ((())) :: (((()))) :: [] "
  = Result.ok
    @@ [ DDeclaration
           ( "main"
           , []
           , EConstructList
               ( ELiteral LUnit
               , EConstructList
                   ( ELiteral LUnit
                   , EConstructList
                       (ELiteral LUnit, EConstructList (ELiteral LUnit, EList [])) ) ) )
       ]
;;

(* 7 *)
let%test _ =
  parse " let main = [\"apple\";\n\"orange\";\n\"banana\";\n\"pear\"] "
  = Result.ok
    @@ [ DDeclaration
           ( "main"
           , []
           , EList
               [ ELiteral (LString "apple")
               ; ELiteral (LString "orange")
               ; ELiteral (LString "banana")
               ; ELiteral (LString "pear")
               ] )
       ]
;;

(* 8 *)
let%test _ =
  parse " let main = [ 'h' ; 'e' ; 'l' ; 'l' ; 'o' ] "
  = Result.ok
    @@ [ DDeclaration
           ( "main"
           , []
           , EList
               [ ELiteral (LChar 'h')
               ; ELiteral (LChar 'e')
               ; ELiteral (LChar 'l')
               ; ELiteral (LChar 'l')
               ; ELiteral (LChar 'o')
               ] )
       ]
;;

(* 9 *)
let%test _ =
  parse " let main = [1] "
  = Result.ok @@ [ DDeclaration ("main", [], EList [ ELiteral (LInt 1) ]) ]
;;

(* 10 *)
let%test _ =
  parse " let main = [] " = Result.ok @@ [ DDeclaration ("main", [], EList []) ]
;;

(* 11 *)
let%test _ =
  parse
    " let main = [let x = 5 and y = 7 in x + y; (fun t -> t - 1) 10; if (5 >= 1) then 1 \
     else 0] "
  = Result.ok
    @@ [ DDeclaration
           ( "main"
           , []
           , EList
               [ ELetIn
                   ( [ DDeclaration ("x", [], ELiteral (LInt 5))
                     ; DDeclaration ("y", [], ELiteral (LInt 7))
                     ]
                   , EBinaryOperation (Add, EIdentifier "x", EIdentifier "y") )
               ; EApplication
                   ( EFun
                       ( [ PIdentifier "t" ]
                       , EBinaryOperation (Sub, EIdentifier "t", ELiteral (LInt 1)) )
                   , ELiteral (LInt 10) )
               ; EIf
                   ( EBinaryOperation (GTE, ELiteral (LInt 5), ELiteral (LInt 1))
                   , ELiteral (LInt 1)
                   , ELiteral (LInt 0) )
               ] )
       ]
;;

(* 12 *)
let%test _ =
  parse
    " let main = (if x > 0 then 1 else (if x = 0 then 0 else -1)) :: [0 ; (if y > 0 then \
     1 else (if y = 0 then 0 else -1)) ; 0] "
  = Result.ok
    @@ [ DDeclaration
           ( "main"
           , []
           , EConstructList
               ( EIf
                   ( EBinaryOperation (GT, EIdentifier "x", ELiteral (LInt 0))
                   , ELiteral (LInt 1)
                   , EIf
                       ( EBinaryOperation (Eq, EIdentifier "x", ELiteral (LInt 0))
                       , ELiteral (LInt 0)
                       , EUnaryOperation (Minus, ELiteral (LInt 1)) ) )
               , EList
                   [ ELiteral (LInt 0)
                   ; EIf
                       ( EBinaryOperation (GT, EIdentifier "y", ELiteral (LInt 0))
                       , ELiteral (LInt 1)
                       , EIf
                           ( EBinaryOperation (Eq, EIdentifier "y", ELiteral (LInt 0))
                           , ELiteral (LInt 0)
                           , EUnaryOperation (Minus, ELiteral (LInt 1)) ) )
                   ; ELiteral (LInt 0)
                   ] ) )
       ]
;;

(* 13 *)
let%test _ =
  parse " let main = fun x y z -> x + y * z "
  = Result.ok
    @@ [ DDeclaration
           ( "main"
           , []
           , EFun
               ( [ PIdentifier "x"; PIdentifier "y"; PIdentifier "z" ]
               , EBinaryOperation
                   ( Add
                   , EIdentifier "x"
                   , EBinaryOperation (Mul, EIdentifier "y", EIdentifier "z") ) ) )
       ]
;;

(* 14 *)
let%test _ =
  parse " let main = fun _ -> 42 "
  = Result.ok @@ [ DDeclaration ("main", [], EFun ([ PWildcard ], ELiteral (LInt 42))) ]
;;

(* 15 *)
let%test _ =
  parse " let main = fun _ -> fun _ -> \"Hello\" "
  = Result.ok
    @@ [ DDeclaration
           ( "main"
           , []
           , EFun ([ PWildcard ], EFun ([ PWildcard ], ELiteral (LString "Hello"))) )
       ]
;;

(* 16 *)
let%test _ =
  parse " let main = fun x y -> if x < 0 then [x;y] else [0;y] "
  = Result.ok
    @@ [ DDeclaration
           ( "main"
           , []
           , EFun
               ( [ PIdentifier "x"; PIdentifier "y" ]
               , EIf
                   ( EBinaryOperation (LT, EIdentifier "x", ELiteral (LInt 0))
                   , EList [ EIdentifier "x"; EIdentifier "y" ]
                   , EList [ ELiteral (LInt 0); EIdentifier "y" ] ) ) )
       ]
;;

(* 17 *)
let%test _ =
  parse
    " let rec matrix_mult_number matrix number =\n\
    \     let rec line_mult_number line =\n\
    \       match line with\n\
    \         | head :: tail -> (head * number) :: line_mult_number tail\n\
    \         | _ -> []\n\
    \     in\n\
    \     match matrix with\n\
    \       | head :: tail -> line_mult_number head :: matrix_mult_number tail number\n\
    \       | _ -> []"
  = Result.ok
    @@ [ DRecursiveDeclaration
           ( "matrix_mult_number"
           , [ PIdentifier "matrix"; PIdentifier "number" ]
           , ELetIn
               ( [ DRecursiveDeclaration
                     ( "line_mult_number"
                     , [ PIdentifier "line" ]
                     , EMatchWith
                         ( EIdentifier "line"
                         , [ ( PConstructList (PIdentifier "head", PIdentifier "tail")
                             , EConstructList
                                 ( EBinaryOperation
                                     (Mul, EIdentifier "head", EIdentifier "number")
                                 , EApplication
                                     (EIdentifier "line_mult_number", EIdentifier "tail")
                                 ) )
                           ; PWildcard, EList []
                           ] ) )
                 ]
               , EMatchWith
                   ( EIdentifier "matrix"
                   , [ ( PConstructList (PIdentifier "head", PIdentifier "tail")
                       , EConstructList
                           ( EApplication
                               (EIdentifier "line_mult_number", EIdentifier "head")
                           , EApplication
                               ( EApplication
                                   (EIdentifier "matrix_mult_number", EIdentifier "tail")
                               , EIdentifier "number" ) ) )
                     ; PWildcard, EList []
                     ] ) ) )
       ]
;;

(* 18 *)
let%test _ =
  parse " let main = \"Danya\", \"Ilya\" "
  = Result.ok
    @@ [ DDeclaration
           ("main", [], ETuple [ ELiteral (LString "Danya"); ELiteral (LString "Ilya") ])
       ]
;;

(* 19 *)
let%test _ =
  parse " let main = ( 123\t, \"aaa\"\t, 'b'\n, true\t, ()\t ) "
  = Result.ok
    @@ [ DDeclaration
           ( "main"
           , []
           , ETuple
               [ ELiteral (LInt 123)
               ; ELiteral (LString "aaa")
               ; ELiteral (LChar 'b')
               ; ELiteral (LBool true)
               ; ELiteral LUnit
               ] )
       ]
;;

(* 20 *)
let%test _ =
  parse " let main = (fun _ -> 1, fun _ -> 2) "
  = Result.ok
    @@ [ DDeclaration
           ( "main"
           , []
           , EFun
               ( [ PWildcard ]
               , ETuple [ ELiteral (LInt 1); EFun ([ PWildcard ], ELiteral (LInt 2)) ] )
           )
       ]
;;

(* 21 *)
let%test _ =
  parse " let main = [fun _ -> 1; fun _ -> 2] "
  = Result.ok
    @@ [ DDeclaration
           ( "main"
           , []
           , EList
               [ EFun ([ PWildcard ], ELiteral (LInt 1))
               ; EFun ([ PWildcard ], ELiteral (LInt 2))
               ] )
       ]
;;

(* 22 *)
let%test _ =
  parse " let main = f (g 5, h ()) (let x = 17 and y = 6 and z = 3 in x * y / z) "
  = Result.ok
    @@ [ DDeclaration
           ( "main"
           , []
           , EApplication
               ( EApplication
                   ( EIdentifier "f"
                   , ETuple
                       [ EApplication (EIdentifier "g", ELiteral (LInt 5))
                       ; EApplication (EIdentifier "h", ELiteral LUnit)
                       ] )
               , ELetIn
                   ( [ DDeclaration ("x", [], ELiteral (LInt 17))
                     ; DDeclaration ("y", [], ELiteral (LInt 6))
                     ; DDeclaration ("z", [], ELiteral (LInt 3))
                     ]
                   , EBinaryOperation
                       ( Div
                       , EBinaryOperation (Mul, EIdentifier "x", EIdentifier "y")
                       , EIdentifier "z" ) ) ) )
       ]
;;

(* 23 *)
let%test _ =
  parse
    " let main = func (if x > 0 && y < 0 then x * y else 0) (let f t = t * t * t in (f \
     x) * (f x)) () "
  = Result.ok
    @@ [ DDeclaration
           ( "main"
           , []
           , EApplication
               ( EApplication
                   ( EApplication
                       ( EIdentifier "func"
                       , EIf
                           ( EBinaryOperation
                               ( AND
                               , EBinaryOperation (GT, EIdentifier "x", ELiteral (LInt 0))
                               , EBinaryOperation (LT, EIdentifier "y", ELiteral (LInt 0))
                               )
                           , EBinaryOperation (Mul, EIdentifier "x", EIdentifier "y")
                           , ELiteral (LInt 0) ) )
                   , ELetIn
                       ( [ DDeclaration
                             ( "f"
                             , [ PIdentifier "t" ]
                             , EBinaryOperation
                                 ( Mul
                                 , EBinaryOperation (Mul, EIdentifier "t", EIdentifier "t")
                                 , EIdentifier "t" ) )
                         ]
                       , EBinaryOperation
                           ( Mul
                           , EApplication (EIdentifier "f", EIdentifier "x")
                           , EApplication (EIdentifier "f", EIdentifier "x") ) ) )
               , ELiteral LUnit ) )
       ]
;;

(* 24 *)
let%test _ =
  parse
    " let main = if x * y / (z * z) > 15 || (f t) <= 0 || (fun x -> x * x) r >= 100 then \
     x - y - z * r else -1 "
  = Result.ok
    @@ [ DDeclaration
           ( "main"
           , []
           , EIf
               ( EBinaryOperation
                   ( OR
                   , EBinaryOperation
                       ( OR
                       , EBinaryOperation
                           ( GT
                           , EBinaryOperation
                               ( Div
                               , EBinaryOperation (Mul, EIdentifier "x", EIdentifier "y")
                               , EBinaryOperation (Mul, EIdentifier "z", EIdentifier "z")
                               )
                           , ELiteral (LInt 15) )
                       , EBinaryOperation
                           ( LTE
                           , EApplication (EIdentifier "f", EIdentifier "t")
                           , ELiteral (LInt 0) ) )
                   , EBinaryOperation
                       ( GTE
                       , EApplication
                           ( EFun
                               ( [ PIdentifier "x" ]
                               , EBinaryOperation (Mul, EIdentifier "x", EIdentifier "x")
                               )
                           , EIdentifier "r" )
                       , ELiteral (LInt 100) ) )
               , EBinaryOperation
                   ( Sub
                   , EBinaryOperation (Sub, EIdentifier "x", EIdentifier "y")
                   , EBinaryOperation (Mul, EIdentifier "z", EIdentifier "r") )
               , EUnaryOperation (Minus, ELiteral (LInt 1)) ) )
       ]
;;

(* 25 *)
let%test _ =
  parse " let main = if not(x = 5) && y = 5 then f x else f y "
  = Result.ok
    @@ [ DDeclaration
           ( "main"
           , []
           , EIf
               ( EBinaryOperation
                   ( AND
                   , EUnaryOperation
                       (Not, EBinaryOperation (Eq, EIdentifier "x", ELiteral (LInt 5)))
                   , EBinaryOperation (Eq, EIdentifier "y", ELiteral (LInt 5)) )
               , EApplication (EIdentifier "f", EIdentifier "x")
               , EApplication (EIdentifier "f", EIdentifier "y") ) )
       ]
;;

(* 31 *)
let%test _ =
  parse
    " let phi n = let rec helper last1 last2 n = if n > 0 then helper last2 (last1 + \
     last2) (n - 1) else last2 in\n\
    \  helper 1 1 (n - 2) "
  = Result.ok
    @@ [ DDeclaration
           ( "phi"
           , [ PIdentifier "n" ]
           , ELetIn
               ( [ DRecursiveDeclaration
                     ( "helper"
                     , [ PIdentifier "last1"; PIdentifier "last2"; PIdentifier "n" ]
                     , EIf
                         ( EBinaryOperation (GT, EIdentifier "n", ELiteral (LInt 0))
                         , EApplication
                             ( EApplication
                                 ( EApplication (EIdentifier "helper", EIdentifier "last2")
                                 , EBinaryOperation
                                     (Add, EIdentifier "last1", EIdentifier "last2") )
                             , EBinaryOperation (Sub, EIdentifier "n", ELiteral (LInt 1))
                             )
                         , EIdentifier "last2" ) )
                 ]
               , EApplication
                   ( EApplication
                       ( EApplication (EIdentifier "helper", ELiteral (LInt 1))
                       , ELiteral (LInt 1) )
                   , EBinaryOperation (Sub, EIdentifier "n", ELiteral (LInt 2)) ) ) )
       ]
;;

(* 32 *)
let%test _ =
  parse " let main = let sq x = x * x in sq x + sq y + sq z "
  = Result.ok
    @@ [ DDeclaration
           ( "main"
           , []
           , ELetIn
               ( [ DDeclaration
                     ( "sq"
                     , [ PIdentifier "x" ]
                     , EBinaryOperation (Mul, EIdentifier "x", EIdentifier "x") )
                 ]
               , EBinaryOperation
                   ( Add
                   , EBinaryOperation
                       ( Add
                       , EApplication (EIdentifier "sq", EIdentifier "x")
                       , EApplication (EIdentifier "sq", EIdentifier "y") )
                   , EApplication (EIdentifier "sq", EIdentifier "z") ) ) )
       ]
;;

(* 33 *)
let%test _ =
  parse " let mult x y z = x * y * z \n  let main = mult 5 6 7 "
  = Result.ok
    @@ [ DDeclaration
           ( "mult"
           , [ PIdentifier "x"; PIdentifier "y"; PIdentifier "z" ]
           , EBinaryOperation
               ( Mul
               , EBinaryOperation (Mul, EIdentifier "x", EIdentifier "y")
               , EIdentifier "z" ) )
       ; DDeclaration
           ( "main"
           , []
           , EApplication
               ( EApplication
                   ( EApplication (EIdentifier "mult", ELiteral (LInt 5))
                   , ELiteral (LInt 6) )
               , ELiteral (LInt 7) ) )
       ]
;;

(* 35 *)
let%test _ =
  parse
    " let f x y z = match x, y, z with\n\
    \  | true, true, false -> true\n\
    \  | true, false, true -> true\n\
    \  | false, true, true -> true\n\
    \  | _ -> false"
  = Result.ok
    @@ [ DDeclaration
           ( "f"
           , [ PIdentifier "x"; PIdentifier "y"; PIdentifier "z" ]
           , EMatchWith
               ( ETuple [ EIdentifier "x"; EIdentifier "y"; EIdentifier "z" ]
               , [ ( PTuple
                       [ PLiteral (LBool true)
                       ; PLiteral (LBool true)
                       ; PLiteral (LBool false)
                       ]
                   , ELiteral (LBool true) )
                 ; ( PTuple
                       [ PLiteral (LBool true)
                       ; PLiteral (LBool false)
                       ; PLiteral (LBool true)
                       ]
                   , ELiteral (LBool true) )
                 ; ( PTuple
                       [ PLiteral (LBool false)
                       ; PLiteral (LBool true)
                       ; PLiteral (LBool true)
                       ]
                   , ELiteral (LBool true) )
                 ; PWildcard, ELiteral (LBool false)
                 ] ) )
       ]
;;

let%test _ =
  parse "let f 2 = 1"
  = Result.ok @@ [ DDeclaration ("f", [ PLiteral (LInt 2) ], ELiteral (LInt 1)) ]
;;

let%test _ =
  parse "let f _ = 1"
  = Result.ok @@ [ DDeclaration ("f", [ PWildcard ], ELiteral (LInt 1)) ]
;;

let%test _ =
  parse "let f (x, \"abacaba\") = 1"
  = Result.ok
    @@ [ DDeclaration
           ( "f"
           , [ PTuple [ PIdentifier "x"; PLiteral (LString "abacaba") ] ]
           , ELiteral (LInt 1) )
       ]
;;

let%test _ =
  parse "let f [(x, y); ((), _)] = 1"
  = Result.ok
    @@ [ DDeclaration
           ( "f"
           , [ PList
                 [ PTuple [ PIdentifier "x"; PIdentifier "y" ]
                 ; PTuple [ PLiteral LUnit; PWildcard ]
                 ]
             ]
           , ELiteral (LInt 1) )
       ]
;;

let%test _ =
  parse "let f ((true, '\n') :: [x, y]) = 1"
  = Result.ok
    @@ [ DDeclaration
           ( "f"
           , [ PConstructList
                 ( PTuple [ PLiteral (LBool true); PLiteral (LChar '\n') ]
                 , PList [ PTuple [ PIdentifier "x"; PIdentifier "y" ] ] )
             ]
           , ELiteral (LInt 1) )
       ]
;;

let%test _ =
  parse "let f = fun x -> x"
  = Result.ok @@ [ DDeclaration ("f", [], EFun ([ PIdentifier "x" ], EIdentifier "x")) ]
;;

let%test _ =
  parse "let f = fun _ -> 2"
  = Result.ok @@ [ DDeclaration ("f", [], EFun ([ PWildcard ], ELiteral (LInt 2))) ]
;;

let%test _ =
  parse "let f = fun x, (true, \"42\") -> true"
  = Result.ok
    @@ [ DDeclaration
           ( "f"
           , []
           , EFun
               ( [ PTuple
                     [ PIdentifier "x"
                     ; PTuple [ PLiteral (LBool true); PLiteral (LString "42") ]
                     ]
                 ]
               , ELiteral (LBool true) ) )
       ]
;;

let%test _ =
  parse "let f = fun [1, x, true; _, _, y] -> true"
  = Result.ok
    @@ [ DDeclaration
           ( "f"
           , []
           , EFun
               ( [ PList
                     [ PTuple
                         [ PLiteral (LInt 1); PIdentifier "x"; PLiteral (LBool true) ]
                     ; PTuple [ PWildcard; PWildcard; PIdentifier "y" ]
                     ]
                 ]
               , ELiteral (LBool true) ) )
       ]
;;

let%test _ =
  parse "let f = fun ([x] :: [y; [(), ([], [])]]) -> true"
  = Result.ok
    @@ [ DDeclaration
           ( "f"
           , []
           , EFun
               ( [ PConstructList
                     ( PList [ PIdentifier "x" ]
                     , PList
                         [ PIdentifier "y"
                         ; PList
                             [ PTuple [ PLiteral LUnit; PTuple [ PList []; PList [] ] ] ]
                         ] )
                 ]
               , ELiteral (LBool true) ) )
       ]
;;

let%test _ =
  parse " let f x y z = match x, y, z with\n  | true, _, x -> true\n  | _ -> false"
  = Result.ok
    @@ [ DDeclaration
           ( "f"
           , [ PIdentifier "x"; PIdentifier "y"; PIdentifier "z" ]
           , EMatchWith
               ( ETuple [ EIdentifier "x"; EIdentifier "y"; EIdentifier "z" ]
               , [ ( PTuple [ PLiteral (LBool true); PWildcard; PIdentifier "x" ]
                   , ELiteral (LBool true) )
                 ; PWildcard, ELiteral (LBool false)
                 ] ) )
       ]
;;

let%test _ =
  parse
    " let f x y z = match x, y, z with\n\
    \  | [a; b], c :: [d; e], _ -> true\n\
    \  | _ -> false"
  = Result.ok
    @@ [ DDeclaration
           ( "f"
           , [ PIdentifier "x"; PIdentifier "y"; PIdentifier "z" ]
           , EMatchWith
               ( ETuple [ EIdentifier "x"; EIdentifier "y"; EIdentifier "z" ]
               , [ ( PTuple
                       [ PList [ PIdentifier "a"; PIdentifier "b" ]
                       ; PConstructList
                           (PIdentifier "c", PList [ PIdentifier "d"; PIdentifier "e" ])
                       ; PWildcard
                       ]
                   , ELiteral (LBool true) )
                 ; PWildcard, ELiteral (LBool false)
                 ] ) )
       ]
;;

let%test _ =
  parse
    " let f x y z = match x, y, z with\n\
    \  | (), [a, 1; (), _], (c, [d; _] :: _, []) -> true\n\
    \  | _ -> false"
  = Result.ok
    @@ [ DDeclaration
           ( "f"
           , [ PIdentifier "x"; PIdentifier "y"; PIdentifier "z" ]
           , EMatchWith
               ( ETuple [ EIdentifier "x"; EIdentifier "y"; EIdentifier "z" ]
               , [ ( PTuple
                       [ PLiteral LUnit
                       ; PList
                           [ PTuple [ PIdentifier "a"; PLiteral (LInt 1) ]
                           ; PTuple [ PLiteral LUnit; PWildcard ]
                           ]
                       ; PTuple
                           [ PIdentifier "c"
                           ; PConstructList
                               (PList [ PIdentifier "d"; PWildcard ], PWildcard)
                           ; PList []
                           ]
                       ]
                   , ELiteral (LBool true) )
                 ; PWildcard, ELiteral (LBool false)
                 ] ) )
       ]
;;

let%test _ =
  parse "let f ___ = fun [1, x, true; _, _, y] -> true"
  = Result.ok
    @@ [ DDeclaration
           ( "f"
           , [ PWildcard ]
           , EFun
               ( [ PList
                     [ PTuple
                         [ PLiteral (LInt 1); PIdentifier "x"; PLiteral (LBool true) ]
                     ; PTuple [ PWildcard; PWildcard; PIdentifier "y" ]
                     ]
                 ]
               , ELiteral (LBool true) ) )
       ]
;;

let%test _ =
  parse "let f (__) = fun [1, x, true; _, _, y] -> true"
  = Result.ok
    @@ [ DDeclaration
           ( "f"
           , [ PWildcard ]
           , EFun
               ( [ PList
                     [ PTuple
                         [ PLiteral (LInt 1); PIdentifier "x"; PLiteral (LBool true) ]
                     ; PTuple [ PWildcard; PWildcard; PIdentifier "y" ]
                     ]
                 ]
               , ELiteral (LBool true) ) )
       ]
;;

let%test _ =
  parse "let f _x = fun [1, x, true; _, _, y] -> true"
  = Result.ok
    @@ [ DDeclaration
           ( "f"
           , [ PIdentifier "_x" ]
           , EFun
               ( [ PList
                     [ PTuple
                         [ PLiteral (LInt 1); PIdentifier "x"; PLiteral (LBool true) ]
                     ; PTuple [ PWildcard; PWildcard; PIdentifier "y" ]
                     ]
                 ]
               , ELiteral (LBool true) ) )
       ]
;;
