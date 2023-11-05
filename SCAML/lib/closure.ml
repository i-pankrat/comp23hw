open Ast
open Base
open Parser

let closure_conversion decl =
  let rec expr_closure = function
    | EConst x -> Set.empty (module String), constr_econst x
    | EVar x -> Set.singleton (module String) x, constr_evar x
    | EBinOp (op, e1, e2) ->
      let s1, e1' = expr_closure e1 in
      let s2, e2' = expr_closure e2 in
      Set.union s1 s2, constr_ebinop op e1' e2'
    | EIf (e1, e2, e3) ->
      let s1, e1' = expr_closure e1 in
      let s2, e2' = expr_closure e2 in
      let s3, e3' = expr_closure e3 in
      Set.union (Set.union s1 s2) s3, constr_eif e1' e2' e3'
    | EApp (e1, e2) ->
      let s1, e1' = expr_closure e1 in
      let s2, e2' = expr_closure e2 in
      Set.union s1 s2, constr_eapp e1' [ e2' ]
    | EFun (x, _) as orig ->
      let s, e' = efun_helper orig in
      (match x with
       | PVar st ->
         let folded =
           Set.fold
             ~init:e'
             ~f:(fun acc x ->
               constr_eapp (constr_efun [ constr_pvar x ] acc) [ constr_evar st ])
             s
         in
         Set.empty (module String), folded
       | _ -> Set.empty (module String), e')
    | ELetIn (b, x, e1, e2) ->
      let s1, e1' = expr_closure e1 in
      let s2, e2' = expr_closure e2 in
      Set.union s1 (Set.remove s2 x), constr_eletin b x e1' e2'
  and efun_helper = function
    | EFun (x, e) ->
      (match x with
       | PVar id ->
         let s, e' = efun_helper e in
         Set.remove s id, constr_efun [ constr_pvar id ] e'
       | p ->
         let s, e' = efun_helper e in
         s, constr_efun [ p ] e')
    | expr -> expr_closure expr
  in
  let decl_closure = function
    | ELet (is_rec, id, e) -> constr_elet is_rec id (snd (expr_closure e))
  in
  decl_closure decl
;;

let prog_conversion = List.map ~f:closure_conversion

let print_prog_result decl =
  let buf = closure_conversion decl in
  Stdlib.Format.printf "%s" (Ast.show_program [ buf ])
;;

let%expect_test _ =
  print_prog_result
      (ELet (false, "fac",
          (EFun ((PVar "n"),
             (ELetIn (true, "fack",
                (EFun ((PVar "n"),
                   (EFun ((PVar "k"),
                      (EIf ((EBinOp (Leq, (EVar "n"), (EConst (CInt 1)))),
                         (EApp ((EVar "k"), (EConst (CInt 1)))),
                         (EApp (
                            (EApp ((EVar "fack"),
                               (EBinOp (Sub, (EVar "n"), (EConst (CInt 1)))))),
                            (EFun ((PVar "m"),
                               (EApp ((EVar "k"),
                                  (EBinOp (Mul, (EVar "m"), (EVar "n")))))
                               ))
                            ))
                         ))
                     ))
                   )),
                (EApp ((EApp ((EVar "fack"), (EVar "n"))),
                   (EFun ((PVar "x"), (EVar "x")))))
                ))
             ))
          ))
  ;
[%expect {|
    [(ELet (false, "fac",
        (EFun ((PVar "n"),
           (ELetIn (true, "fack",
              (EApp (
                 (EFun ((PVar "fack"),
                    (EFun ((PVar "n"),
                       (EFun ((PVar "k"),
                          (EIf ((EBinOp (Leq, (EVar "n"), (EConst (CInt 1)))),
                             (EApp ((EVar "k"), (EConst (CInt 1)))),
                             (EApp (
                                (EApp ((EVar "fack"),
                                   (EBinOp (Sub, (EVar "n"), (EConst (CInt 1))))
                                   )),
                                (EApp (
                                   (EFun ((PVar "n"),
                                      (EApp (
                                         (EFun ((PVar "k"),
                                            (EFun ((PVar "m"),
                                               (EApp ((EVar "k"),
                                                  (EBinOp (Mul, (EVar "m"),
                                                     (EVar "n")))
                                                  ))
                                               ))
                                            )),
                                         (EVar "m")))
                                      )),
                                   (EVar "m")))
                                ))
                             ))
                          ))
                       ))
                    )),
                 (EVar "n"))),
              (EApp ((EApp ((EVar "fack"), (EVar "n"))),
                 (EFun ((PVar "x"), (EVar "x")))))
              ))
           ))
        ))
      ] |}]
;;

(* let fac n =
   let rec fack n k =
   if n <= 1 then k 1 else fack (n - 1) ((fun k n m -> k (m * n)) k n)
   in
   fack n (fun x -> x)
   ;;

   let fac n =
   let rec fack n k = if n <= 1 then k 1 else fack (n - 1) (fun m -> k (m * n)) in
   fack n (fun x -> x)
   ;; *)

(* let fac n =
   let fack1 k m = k (m * n) in
   let rec fack n k = if n <= 1 then k 1 else fack (n - 1) (fack1 k ) in
   fack n (fun x -> x);; *)
