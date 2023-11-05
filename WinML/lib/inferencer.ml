(** Copyright 2023-2024, Vyacheslav Buchin and Artur Gagin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Monads.Std
open Types
open Parsed_tree

module Scheme : sig
  include Parameterized

  val type_vars : t -> TypeVars.t
  val typ : t -> Typ.t
  val empty : Typ.t -> t
end = struct
  type t = TypeVars.t * Typ.t

  let ftv (tvs, t) = Typ.ftv t |> TypeVars.union tvs

  let apply s (tvs, t) =
    let s = TypeVars.fold Subst.remove tvs s in
    tvs, Typ.apply s t
  ;;

  let type_vars = fst
  let typ = snd
  let empty t = TypeVars.empty, t
end

module Context : sig
  include Parameterized

  val find : string -> t -> Scheme.t option
  val add : string -> Scheme.t -> t -> t
  val empty : t
end = struct
  module M = Map.Make (String)
  include M

  type t = Scheme.t M.t

  let ftv ctx =
    let folder _ schm acc = Scheme.ftv schm |> TypeVars.union acc in
    fold folder ctx TypeVars.empty
  ;;

  let apply s = map (Scheme.apply s)

  let find i ctx =
    let folder id schm acc =
      match acc with
      | Some _ as x -> x
      | None when id = i -> Some schm
      | None -> None
    in
    M.fold folder ctx None
  ;;

  let add = M.add
  let empty = M.empty
end

module TIState = struct
  module TIRuntime = struct
    type t = int
  end

  include Monad.State.T1 (TIRuntime) (Monad.Ident)
  include Monad.State.Make (TIRuntime) (Monad.Ident)
end

module TI = struct
  include Monad.Result.Make (Monoid.String) (TIState)

  let run init c = fst @@ TIState.run (run c) init
  let fail_occurs_check () = fail "occurs check failed"
  let fail_unification () = fail "types do not unify"
  let fail_unbound () = fail "unbound identifier"
end

let tvar () =
  TIState.(
    let* i = get () in
    let* () = put (i + 1) in
    return @@ TVar i)
  |> TI.lift
;;

let return = TI.return
let ( let* ) = TI.( let* )
let ( >>= ) = TI.( >>= )

module SeqM = struct
  let rec mapM f s =
    match Seq.uncons s with
    | Some (h, t) ->
      let* h = f h in
      let* t = mapM f t in
      return @@ Seq.cons h t
    | None -> return @@ Seq.empty
  ;;
end

module ListM = struct
  let rec mapM f = function
    | [] -> return []
    | x :: xs ->
      let* x = f x in
      let* xs = mapM f xs in
      return @@ (x :: xs)
  ;;

  let rec foldM f init = function
    | [] -> return init
    | x :: xs ->
      let* init = f init x in
      foldM f init xs
  ;;
end

let instantiate schm =
  let tvs = Scheme.type_vars schm |> TypeVars.to_seq in
  let typ = Scheme.typ schm in
  let* instances = SeqM.mapM (fun _ -> tvar ()) tvs in
  let subst = Seq.zip tvs instances |> Subst.from_seq in
  Typ.apply subst typ |> Typ.to_typ |> return
;;

let bindVar i (t : typ) =
  match () with
  | _ when t = TVar i -> return Subst.empty
  | _ when Typ.from_typ t |> Typ.ftv |> TypeVars.mem i -> TI.fail_occurs_check ()
  | _ -> return @@ Subst.singleton i t
;;

let apply s t = Typ.from_typ t |> Typ.apply s |> Typ.to_typ

let rec unify a b =
  match a, b with
  | TArrow (a', b'), TArrow (c', d') ->
    let* s_a = unify a' c' in
    let* s_b = unify (apply s_a b') (apply s_a d') in
    return @@ Subst.compose apply s_a s_b
  | TVar i, t | t, TVar i -> bindVar i t
  | t1, t2 when t1 = t2 -> return Subst.empty
  | _ -> TI.fail_unification ()
;;

let infer_const = function
  | CInt _ -> TInt
;;

let infer_binop = function
  | Add | Mul | Sub -> return @@ TArrow (TInt, TArrow (TInt, TInt))
  | Eq ->
    let* a = tvar () in
    return @@ TArrow (a, TArrow (a, TBool))
;;

let rec infer_expression ctx subst = function
  | Const c -> return @@ (infer_const c, subst)
  | Var n ->
    (match Context.find n ctx with
     | None -> TI.fail_unbound ()
     | Some schm ->
       let* t = instantiate schm in
       return (apply subst t, subst))
  | BinOp (op, e1, e2) ->
    let* op_typ = infer_binop op in
    let* t1, s1 = infer_expression ctx subst e1 in
    let subst = Subst.compose apply subst s1 in
    let* t2, s2 = infer_expression ctx subst e2 in
    let subst = Subst.compose apply subst s2 in
    let* return_t = tvar () in
    let t = TArrow (apply subst t1, TArrow (apply subst t2, return_t)) in
    let* s = unify op_typ t in
    let subst = Subst.compose apply subst s in
    return (apply subst return_t, subst)
  | IfThenElse (cond, thn, els) ->
    let* cond_t, s = infer_expression ctx subst cond in
    let subst = Subst.compose apply subst s in
    let* s = unify TBool cond_t in
    let subst = Subst.compose apply subst s in
    let* thn_t, s = infer_expression ctx subst thn in
    let subst = Subst.compose apply subst s in
    let* els_t, s = infer_expression ctx subst els in
    let subst = Subst.compose apply subst s in
    let* s = unify thn_t els_t in
    let subst = Subst.compose apply subst s in
    return (apply subst thn_t, subst)
  | Application (e1, e2, es) ->
    let folder (t, subst) e =
      let* e_t, s = infer_expression ctx subst e in
      let subst = Subst.compose apply subst s in
      let* return_t = tvar () in
      let applied_t = TArrow (e_t, return_t) in
      let* s = unify (apply subst t) (apply subst applied_t) in
      let subst = Subst.compose apply subst s in
      return (apply subst return_t, subst)
    in
    let* e1_t, s = infer_expression ctx subst e1 in
    let subst = Subst.compose apply subst s in
    ListM.foldM folder (apply subst e1_t, subst) @@ (e2 :: es)
;;

let infer_declaration ctx subst (_, args, body) =
  let* tvars = ListM.mapM (fun _ -> tvar () |> TI.map ~f:Typ.from_typ) args in
  let ctx =
    List.map Scheme.empty tvars
    |> List.fold_left2 (fun ctx id var -> Context.add id var ctx) ctx args
  in
  let* body_t, s = infer_expression ctx subst body in
  let subst = Subst.compose apply subst s in
  match List.map Typ.to_typ tvars with
  | [] -> return (apply subst body_t, subst)
  | tvar :: tvars ->
    let folder tvar t arg = TArrow (apply subst arg, t @@ apply subst tvar) in
    let init arg = TArrow (arg, body_t) in
    let tvar = apply subst tvar in
    let decl_t = List.fold_right folder tvars init tvar in
    let decl_t = apply subst decl_t in
    return (decl_t, subst)
;;

let infer_func ctx subst = function
  | Function decl -> infer_declaration ctx subst decl
  | RecFunction ((name, _, _) as decl) ->
    let* v = tvar () in
    let schm = Scheme.empty @@ Typ.from_typ v in
    let ctx = Context.add name schm ctx in
    infer_declaration ctx subst decl
;;

let infer f = infer_func Context.empty Subst.empty f |> TI.run 0

let%test "f x = 1 + 1" =
  let f = Function ("f", [ "x" ], BinOp (Add, Const (CInt 1), Const (CInt 1))) in
  let t, _ =
    match infer f with
    | Monad.Result.Ok x -> x
    | _ -> failwith "kek"
  in
  match t with
  | TArrow (TVar _, TInt) -> true
  | _ -> false
;;

let%test "f x = if x then 1 else x" =
  let f = Function ("f", [ "x" ], IfThenElse (Var "x", Const (CInt 1), Var "x")) in
  match infer f with
  | Monad.Result.Ok _ -> false
  | _ -> true
;;

let%test "factorial" =
  let fac =
    RecFunction
      ( "f"
      , [ "n" ]
      , IfThenElse
          ( BinOp (Eq, Var "n", Const (CInt 1))
          , Const (CInt 1)
          , BinOp
              ( Mul
              , Var "n"
              , Application (Var "f", BinOp (Sub, Var "n", Const (CInt 1)), []) ) ) )
  in
  let expected = TArrow (TInt, TInt) in
  let t, _ =
    match infer fac with
    | Monad.Result.Ok x -> x
    | _ -> failwith "kek"
  in
  t = expected
;;
