(** Copyright 2023-2024, Ilya Pankratov, Maxim Drumov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Typetree
open Typedtree

type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of ty * ty
  ]

let pp_error ppf : error -> _ = function
  | `Occurs_check -> Stdlib.Format.fprintf ppf "Typechecker error: occurs check failed"
  | `No_variable s ->
    Stdlib.Format.fprintf ppf "Typechecker error: undefined variable '%s'" s
  | `Unification_failed (l, r) ->
    Stdlib.Format.fprintf
      ppf
      "Typechecker error: unification failed on %a and %a"
      Pprinttypetree.pp_ty
      l
      Pprinttypetree.pp_ty
      r
;;

module R : sig
  include Monad.Infix

  (* val bind : 'a t -> f:('a -> 'b t) -> 'b t *)
  val return : 'a -> 'a t
  val fail : error -> 'a t

  module Syntax : sig
    val ( let* ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t
end = struct
  (* A compositon: State monad after Result monad *)
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Error x
    | Ok a -> f a last
  ;;

  let fail e st = st, Result.fail e
  let return x last = last, Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module VarSet = struct
  include Stdlib.Set.Make (Int)

  (* let pp ppf s =
     Stdlib.Format.fprintf ppf "[ ";
    iter (Stdlib.Format.fprintf ppf "%d; ") s;
    Stdlib.Format.fprintf ppf "]"
     ;; *)

  let fold_left_m f acc set =
    fold
      (fun x acc ->
        let open R.Syntax in
        let* acc = acc in
        f acc x)
      acc
      set
  ;;
end

module Type = struct
  type t = ty

  let rec occurs_in v = function
    | Tyvar b -> b = v
    | Arrow (l, r) -> occurs_in v l || occurs_in v r
    | Prim _ -> false
  ;;

  let free_vars =
    let rec helper acc = function
      | Tyvar b -> VarSet.add b acc
      | Arrow (l, r) -> helper (helper acc l) r
      | Prim _ -> acc
    in
    helper VarSet.empty
  ;;
end

let fold_left map ~init ~f =
  Map.Poly.fold map ~init ~f:(fun ~key ~data acc ->
    let open R.Syntax in
    let* acc = acc in
    f key data acc)
;;

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> ty -> t R.t
  val apply : t -> ty -> ty
  val unify : ty -> ty -> t R.t

  (** Compositon of substitutions *)
  val compose : t -> t -> t R.t

  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
  (* val pp : Format.formatter -> t -> unit *)
end = struct
  open R
  open R.Syntax

  type t = (fresh, ty) Map.Poly.t

  (* let pp ppf subst =
     let open Format in
     Map.Poly.iteri subst ~f:(fun ~key ~data ->
     fprintf ppf "'_%d -> %a@\n" key (Pprinttypetree.pp_ty_with_subs None) data)
     ;; *)

  let empty = Map.Poly.empty
  let mapping k v = if Type.occurs_in k v then fail `Occurs_check else return (k, v)

  let singleton k v =
    let* f, t = mapping k v in
    return @@ Map.Poly.singleton f t
  ;;

  let find_exn f m = Map.Poly.find_exn m f
  let remove m f = Map.Poly.remove m f

  let apply subst =
    let rec helper = function
      | Tyvar b as ty ->
        (match find_exn b subst with
         | exception Not_found_s _ -> ty
         | x -> x)
      | Arrow (l, r) -> Arrow (helper l, helper r)
      | other -> other
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | Prim l, Prim r when equal_prim l r -> return empty
    | Prim _, Prim _ -> fail (`Unification_failed (l, r))
    | Tyvar a, Tyvar b when Int.equal a b -> return empty
    | Tyvar b, t | t, Tyvar b -> singleton b t
    | Arrow (l1, r1), Arrow (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | _ -> fail (`Unification_failed (l, r))

  and extend key value extensible_subst =
    match Map.Poly.find extensible_subst key with
    | None ->
      let v = apply extensible_subst value in
      let* s2 = singleton key v in
      fold_left extensible_subst ~init:(return s2) ~f:(fun key value acc ->
        let v = apply s2 value in
        let* mapk, mapv = mapping key v in
        match Map.Poly.add acc ~key:mapk ~data:mapv with
        | `Ok map -> return map
        | `Duplicate -> return acc)
    | Some v2 ->
      let* s2 = unify value v2 in
      compose extensible_subst s2

  and compose s1 s2 = fold_left s2 ~init:(return s1) ~f:extend

  let compose_all s1 =
    let fold_left xs ~init ~f =
      List.fold_left xs ~init ~f:(fun acc x ->
        let open Syntax in
        let* acc = acc in
        f acc x)
    in
    fold_left s1 ~init:(return empty) ~f:compose
  ;;
end

type binder_set = VarSet.t
type scheme = S of binder_set * ty

module Scheme = struct
  type t = scheme

  let free_vars = function
    | S (names, ty) -> VarSet.diff (Type.free_vars ty) names
  ;;

  let apply sub (S (names, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) names sub in
    S (names, Subst.apply s2 ty)
  ;;

  (* let pp =
     let helper ppf =
     let open Stdlib.Format in
     let open Pprinttypetree in
     function
     | S (set, ty) -> fprintf ppf "S(%s, %a)" (show_binder_set set) pp_ty ty
     in
     helper
     ;; *)
end

type environment = (string * scheme) list

module TypeEnv = struct
  type t = environment

  let extend e h = h :: e
  let empty = []

  let free_vars : t -> VarSet.t =
    List.fold_left ~init:VarSet.empty ~f:(fun acc (_, scheme) ->
      VarSet.union acc (Scheme.free_vars scheme))
  ;;

  let apply subst env = List.Assoc.map env ~f:(Scheme.apply subst)

  (* let pp ppf xs =
    Stdlib.Format.fprintf ppf "{| ";
    List.iter xs ~f:(fun (n, s) -> Stdlib.Format.fprintf ppf "%s -> %a; " n pp_scheme s);
    Stdlib.Format.fprintf ppf "|}%!"
  ;; *)

  (* let available_bindings env = List.Assoc.map env ~f:(fun (S (_, ty)) -> ty) *)
end

open R
open R.Syntax

let unify = Subst.unify
let fresh_var = fresh >>| fun n -> var_typ n

let instantiate : scheme -> ty R.t =
  fun (S (names, ty)) ->
  VarSet.fold_left_m
    (fun typ name ->
      let* f1 = fresh_var in
      let* s = Subst.singleton name f1 in
      return (Subst.apply s typ))
    names
    (return ty)
;;

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
  fun env ty ->
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  S (free, ty)
;;

let lookup_env var env =
  match List.Assoc.find_exn env ~equal:String.equal var with
  | (exception Stdlib.Not_found) | (exception Not_found_s _) -> fail (`No_variable var)
  | scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;

(* let pp_env subst ppf env =
   let env : TypeEnv.t =
   List.map ~f:(fun (k, S (args, v)) -> k, S (args, Subst.apply subst v)) env
   in
   TypeEnv.pp ppf env
   ;; *)

let infer_expr =
  let open Ast in
  let rec (helper : TypeEnv.t -> Ast.expr -> (Subst.t * ty * texpr) R.t) =
    fun env -> function
    | EVar x ->
      let* s, t = lookup_env x env in
      return (s, t, tvar x t)
    | EConst const ->
      (match const with
       | CBool _ -> return (Subst.empty, tybool, tconst const tybool)
       | CInt _ -> return (Subst.empty, tyint, tconst const tyint))
    | EBinop (op, e1, e2) ->
      let* s1, t1, te1 = helper env e1 in
      let* s2, t2, te2 = helper env e2 in
      (match op with
       | Add | Sub | Div | Mul ->
         let* s3 = unify t1 tyint in
         let* s4 = unify t2 tyint in
         let* final_subst = Subst.compose_all [ s1; s2; s3; s4 ] in
         return (final_subst, tyint, tbinop op te1 te2 (arrow tyint (arrow tyint tyint)))
       | Xor | And | Or ->
         let* s3 = unify t1 tybool in
         let* s4 = unify t2 tybool in
         let* final_subst = Subst.compose_all [ s1; s2; s3; s4 ] in
         return
           (final_subst, tybool, tbinop op te1 te2 (arrow tybool (arrow tybool tybool)))
       | Eq | Neq | Gt | Lt | Gte | Lte ->
         (match t1, t2 with
          | _ ->
            let* s3 = unify t1 t2 in
            let final_typ = Subst.apply s3 t1 in
            let* final_subst = Subst.compose_all [ s1; s2; s3 ] in
            return
              ( final_subst
              , tybool
              , tbinop op te1 te2 (arrow final_typ (arrow final_typ tybool)) )))
    | EApp (e1, e2) ->
      let* s1, t1, te1 = helper env e1 in
      let* s2, t2, te2 = helper (TypeEnv.apply s1 env) e2 in
      let* tv = fresh_var in
      let* s3 = unify (Subst.apply s2 t1) (Arrow (t2, tv)) in
      let* final_subst = Subst.compose_all [ s3; s2; s1 ] in
      let typedres = Subst.apply final_subst tv in
      return (final_subst, typedres, tapp te1 te2 typedres)
    | EIfThenElse (c, th, el) ->
      let* s1, t1, te1 = helper env c in
      let* s2, t2, te2 = helper env th in
      let* s3, t3, te3 = helper env el in
      let* s4 = unify t1 tybool in
      let* s5 = unify t2 t3 in
      let* final_subst = Subst.compose_all [ s5; s4; s3; s2; s1 ] in
      return
        ( final_subst
        , Subst.apply final_subst t2
        , tifthenelse te1 te2 te3 (Subst.apply final_subst t2) )
    | ELetIn (name, e1, e2) ->
      let* s1, t1_typ, te1 = helper env e1 in
      let env2 = TypeEnv.apply s1 env in
      let t1 = generalize env2 t1_typ in
      let* s2, t3, te2 = helper (TypeEnv.extend env2 (name, t1)) e2 in
      let* final_subst = Subst.compose s1 s2 in
      return (final_subst, t3, tletin name te1 te2 t1_typ)
    | ELetRecIn (name, e1, e2) ->
      let* tv = fresh_var in
      let env = TypeEnv.extend env (name, S (VarSet.empty, tv)) in
      let* s1, t1, te1 = helper env e1 in
      let* s2 = unify (Subst.apply s1 tv) t1 in
      let* s = Subst.compose s1 s2 in
      let env = TypeEnv.apply s env in
      let t2 = generalize env (Subst.apply s tv) in
      let* s2, t2, te2 = helper TypeEnv.(extend (apply s env) (name, t2)) e2 in
      let* final_subst = Subst.compose s s2 in
      return (final_subst, t2, tletrecin name te1 te2 t1)
    | EFun (arg, e) ->
      let* tv = fresh_var in
      let env = TypeEnv.extend env (arg, S (VarSet.empty, tv)) in
      let* s1, t1, te1 = helper env e in
      let typedres = Arrow (Subst.apply s1 tv, t1) in
      return (s1, typedres, tfun arg tv te1 typedres)
  in
  helper
;;

let infer_binding env =
  let open Ast in
  let open Typedtree in
  function
  | ELet (name, e) ->
    let* s, t, te = infer_expr env e in
    return (s, t, tlet name te t)
  | ELetRec (name, e) ->
    let* tv = fresh_var in
    let env = TypeEnv.extend env (name, S (VarSet.empty, tv)) in
    let* s1, t, te = infer_expr env e in
    let* s2 = unify (Subst.apply s1 tv) t in
    let* funal_subst = Subst.compose s1 s2 in
    return (funal_subst, t, tletrec name te t)
;;

let fix_typedtree subst =
  let apply_subst = Subst.apply subst in
  let rec helper = function
    | TVar (name, typ) -> tvar name (apply_subst typ)
    | TBinop (op, e1, e2, typ) -> tbinop op (helper e1) (helper e2) (apply_subst typ)
    | TApp (e1, e2, typ) -> tapp (helper e1) (helper e2) (apply_subst typ)
    | TLetIn (name, e1, e2, typ) -> tletin name (helper e1) (helper e2) (apply_subst typ)
    | TLetRecIn (name, e1, e2, typ) ->
      tletrecin name (helper e1) (helper e2) (apply_subst typ)
    | TFun (Arg (name, arg_typ), e, typ) ->
      tfun name (apply_subst arg_typ) (helper e) (apply_subst typ)
    | TIfThenElse (i, t, e, typ) ->
      tifthenelse (helper i) (helper t) (helper e) (apply_subst typ)
    | other -> other
  in
  helper
;;

let empty : environment = TypeEnv.empty

let infer_expr e =
  let* subst, ty, te = infer_expr empty e in
  let typed_tree = fix_typedtree subst te in
  return (ty, typed_tree)
;;

let fix_typedtree subst =
  let apply_subst = Subst.apply subst in
  function
  | TLet (name, e, typ) -> tlet name (fix_typedtree subst e) (apply_subst typ)
  | TLetRec (name, e, typ) -> tletrec name (fix_typedtree subst e) (apply_subst typ)
;;

let infer_statements (bindings : Ast.statements) : tbinding list t =
  let open Ast in
  let* _, tbindings =
    List.fold
      ~init:(return (empty, []))
      ~f:(fun env_binding ->
        function
        | (ELet (name, _) | ELetRec (name, _)) as new_binding ->
          let* env, tbindings = env_binding in
          let* subst, ty, tbinding = infer_binding env new_binding in
          return
            ( TypeEnv.extend env (name, S (VarSet.empty, ty))
            , tbindings @ [ fix_typedtree subst tbinding ] ))
      bindings
  in
  return tbindings
;;

let infer_type infer_fun s = Result.map ~f:Stdlib.Fun.id (run (infer_fun s))
let infer_expr = infer_type infer_expr
let infer_statements = infer_type infer_statements
let infer = infer_statements

let run_infer_expr =
  let open Pprinttypedtree in
  function
  | Result.Error e -> Stdlib.Format.printf "%a%!" pp_error e
  | Result.Ok (_, te) -> Stdlib.Format.printf "%a%!" pp_texpr te
;;

let run_infer_statements =
  let open Pprinttypedtree in
  function
  | Result.Error e -> Stdlib.Format.printf "%a%!" pp_error e
  | Result.Ok te ->
    let pp_statements = pp_statements ";\n" Complete in
    Stdlib.Format.printf "%a%!" pp_statements te
;;

(** Infer tests *)

(** Constants  tests *)

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EConst (CInt 4) in
    infer_expr e |> run_infer_expr
  in
  [%expect {| (TConst((CInt 4): int)) |}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EConst (CBool false) in
    infer_expr e |> run_infer_expr
  in
  [%expect {| (TConst((CBool false): bool)) |}]
;;

(** Variable tests *)

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EVar "x" in
    infer_expr e |> run_infer_expr
  in
  [%expect {| Typechecker error: undefined variable 'x'  |}]
;;

(** Tests for integer binary operations *)

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Add, EConst (CInt 1), EConst (CInt 1)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Add: (int -> (int -> int)) (
        (TConst((CInt 1): int)),
        (TConst((CInt 1): int))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Sub, EConst (CInt 1), EConst (CInt 1)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Sub: (int -> (int -> int)) (
        (TConst((CInt 1): int)),
        (TConst((CInt 1): int))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Div, EConst (CInt 1), EConst (CInt 1)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Div: (int -> (int -> int)) (
        (TConst((CInt 1): int)),
        (TConst((CInt 1): int))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Mul, EConst (CInt 1), EConst (CInt 1)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Mul: (int -> (int -> int)) (
        (TConst((CInt 1): int)),
        (TConst((CInt 1): int))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Eq, EConst (CInt (-1)), EConst (CInt 100)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Eq: (int -> (int -> bool)) (
        (TConst((CInt -1): int)),
        (TConst((CInt 100): int))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Neq, EConst (CInt (-1)), EConst (CInt 100)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Neq: (int -> (int -> bool)) (
        (TConst((CInt -1): int)),
        (TConst((CInt 100): int))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Gt, EConst (CInt (-1)), EConst (CInt 100)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Gt: (int -> (int -> bool)) (
        (TConst((CInt -1): int)),
        (TConst((CInt 100): int))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Lt, EConst (CInt (-1)), EConst (CInt 100)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Lt: (int -> (int -> bool)) (
        (TConst((CInt -1): int)),
        (TConst((CInt 100): int))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Gte, EConst (CInt (-1)), EConst (CInt 100)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Gte: (int -> (int -> bool)) (
        (TConst((CInt -1): int)),
        (TConst((CInt 100): int))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Lte, EConst (CInt (-1)), EConst (CInt 100)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Lte: (int -> (int -> bool)) (
        (TConst((CInt -1): int)),
        (TConst((CInt 100): int))
    ))
|}]
;;

(** Tests for boolean binary operations *)

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Xor, EConst (CBool true), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Xor: (bool -> (bool -> bool)) (
        (TConst((CBool true): bool)),
        (TConst((CBool false): bool))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (And, EConst (CBool true), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (And: (bool -> (bool -> bool)) (
        (TConst((CBool true): bool)),
        (TConst((CBool false): bool))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Or, EConst (CBool true), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Or: (bool -> (bool -> bool)) (
        (TConst((CBool true): bool)),
        (TConst((CBool false): bool))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Eq, EConst (CBool true), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Eq: (bool -> (bool -> bool)) (
        (TConst((CBool true): bool)),
        (TConst((CBool false): bool))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Neq, EConst (CBool true), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Neq: (bool -> (bool -> bool)) (
        (TConst((CBool true): bool)),
        (TConst((CBool false): bool))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Gt, EConst (CBool true), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Gt: (bool -> (bool -> bool)) (
        (TConst((CBool true): bool)),
        (TConst((CBool false): bool))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Gt, EConst (CBool true), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Gt: (bool -> (bool -> bool)) (
        (TConst((CBool true): bool)),
        (TConst((CBool false): bool))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Lt, EConst (CBool true), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Lt: (bool -> (bool -> bool)) (
        (TConst((CBool true): bool)),
        (TConst((CBool false): bool))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Gte, EConst (CBool true), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Gte: (bool -> (bool -> bool)) (
        (TConst((CBool true): bool)),
        (TConst((CBool false): bool))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Lte, EConst (CBool true), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (Lte: (bool -> (bool -> bool)) (
        (TConst((CBool true): bool)),
        (TConst((CBool false): bool))
    ))
|}]
;;

(** Tests for wrong typed binary operations *)

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Add, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on bool and int
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Sub, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on bool and int
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Div, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on bool and int
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Mul, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on bool and int
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Xor, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on int and bool
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (And, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on int and bool
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Or, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on int and bool
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Eq, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on int and bool
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Neq, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on int and bool
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Gt, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on int and bool
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Lt, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on int and bool
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Gte, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on int and bool
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EBinop (Lte, EConst (CInt 42), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on int and bool
|}]
;;

(** Test for typing anonymous function *)

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EFun ("x", EVar "x") in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    (TFun: ('a -> 'a) (
        (x: 'a),
        (x: 'a)
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EFun ("x", EConst (CInt 1)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (TFun: ('a -> int) (
        (x: 'a),
        (TConst((CInt 1): int))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EFun ("x", EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (TFun: ('a -> bool) (
        (x: 'a),
        (TConst((CBool false): bool))
    ))
|}]
;;

(** Tests for typing condition statement *)

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EIfThenElse (EConst (CBool true), EConst (CInt 4), EConst (CInt 5)) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (TIfThenElse: int
        ((TConst((CBool true): bool)),
        (TConst((CInt 4): int)),
        (TConst((CInt 5): int))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e =
      EIfThenElse (EConst (CBool true), EConst (CBool true), EConst (CBool false))
    in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (TIfThenElse: bool
        ((TConst((CBool true): bool)),
        (TConst((CBool true): bool)),
        (TConst((CBool false): bool))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EIfThenElse (EConst (CInt 3), EConst (CBool true), EConst (CBool false)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on int and bool
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = EIfThenElse (EConst (CBool true), EConst (CBool true), EConst (CInt 3)) in
    infer_expr e |> run_infer_expr
  in
  [%expect {|
    Typechecker error: unification failed on bool and int
|}]
;;

(** Let in tests *)

let%expect_test _ =
  let open Ast in
  let _ =
    let e = ELetIn ("id", EFun ("x", EVar "x"), EApp (EVar "id", EConst (CInt 1))) in
    infer_expr e |> run_infer_expr
  in
  [%expect
    {|
    (TLetIn(
        id: ('a -> 'a),
        (TFun: ('a -> 'a) (
            (x: 'a),
            (x: 'a)
        )),
        (TApp: int (
            (id: (int -> int)),
            (TConst((CInt 1): int))
        ))
    ))
|}]
;;

(** Statements tests *)

let%expect_test _ =
  let open Ast in
  let _ =
    let e = [ ELet ("result", EConst (CBool true)) ] in
    infer_statements e |> run_infer_statements
  in
  [%expect
    {|
    (TLet(
        result: bool,
        (TConst((CBool true): bool))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = [ ELet ("result", EConst (CInt 4)) ] in
    infer_statements e |> run_infer_statements
  in
  [%expect {|
    (TLet(
        result: int,
        (TConst((CInt 4): int))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = [ ELet ("result", EFun ("x", EVar "x")) ] in
    infer_statements e |> run_infer_statements
  in
  [%expect
    {|
    (TLet(
        result: ('a -> 'a),
        (TFun: ('a -> 'a) (
            (x: 'a),
            (x: 'a)
        ))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = [ ELet ("result", EFun ("x", EConst (CInt 5))) ] in
    infer_statements e |> run_infer_statements
  in
  [%expect
    {|
    (TLet(
        result: ('a -> int),
        (TFun: ('a -> int) (
            (x: 'a),
            (TConst((CInt 5): int))
        ))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e =
      [ ELet
          ( "sum4"
          , EFun
              ( "x"
              , EFun
                  ( "y"
                  , EFun
                      ( "z"
                      , EFun
                          ( "w"
                          , EBinop
                              ( Add
                              , EBinop (Add, EVar "x", EVar "y")
                              , EBinop (Add, EVar "z", EVar "w") ) ) ) ) ) )
      ]
    in
    infer_statements e |> run_infer_statements
  in
  [%expect
    {|
    (TLet(
        sum4: (int -> (int -> (int -> (int -> int)))),
        (TFun: (int -> (int -> (int -> (int -> int)))) (
            (x: int),
            (TFun: (int -> (int -> (int -> int))) (
                (y: int),
                (TFun: (int -> (int -> int)) (
                    (z: int),
                    (TFun: (int -> int) (
                        (w: int),
                        (Add: (int -> (int -> int)) (
                            (Add: (int -> (int -> int)) (
                                (x: int),
                                (y: int)
                            )),
                            (Add: (int -> (int -> int)) (
                                (z: int),
                                (w: int)
                            ))
                        ))
                    ))
                ))
            ))
        ))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = [ ELet ("apply", EFun ("f", EFun ("a", EApp (EVar "f", EVar "a")))) ] in
    infer_statements e |> run_infer_statements
  in
  [%expect
    {|
    (TLet(
        apply: (('a -> 'b) -> ('a -> 'b)),
        (TFun: (('a -> 'b) -> ('a -> 'b)) (
            (f: ('a -> 'b)),
            (TFun: ('a -> 'b) (
                (a: 'a),
                (TApp: 'b (
                    (f: ('a -> 'b)),
                    (a: 'a)
                ))
            ))
        ))
    ))

|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e =
      [ ELet
          ( "apply5"
          , EFun
              ( "function"
              , EFun
                  ( "a"
                  , EFun
                      ( "b"
                      , EFun
                          ( "c"
                          , EFun
                              ( "d"
                              , EFun
                                  ( "e"
                                  , EApp
                                      ( EApp
                                          ( EApp
                                              ( EApp
                                                  ( EApp (EVar "function", EVar "a")
                                                  , EVar "b" )
                                              , EVar "c" )
                                          , EVar "d" )
                                      , EVar "e" ) ) ) ) ) ) ) )
      ]
    in
    infer_statements e |> run_infer_statements
  in
  [%expect
    {|
    (TLet(
        apply5: (('a -> ('b -> ('c -> ('d -> ('e -> 'f))))) -> ('a -> ('b -> ('c -> ('d -> ('e -> 'f)))))),
        (TFun: (('a -> ('b -> ('c -> ('d -> ('e -> 'f))))) -> ('a -> ('b -> ('c -> ('d -> ('e -> 'f)))))) (
            (function: ('a -> ('b -> ('c -> ('d -> ('e -> 'f)))))),
            (TFun: ('a -> ('b -> ('c -> ('d -> ('e -> 'f))))) (
                (a: 'a),
                (TFun: ('b -> ('c -> ('d -> ('e -> 'f)))) (
                    (b: 'b),
                    (TFun: ('c -> ('d -> ('e -> 'f))) (
                        (c: 'c),
                        (TFun: ('d -> ('e -> 'f)) (
                            (d: 'd),
                            (TFun: ('e -> 'f) (
                                (e: 'e),
                                (TApp: 'f (
                                    (TApp: ('e -> 'f) (
                                        (TApp: ('d -> ('e -> 'f)) (
                                            (TApp: ('c -> ('d -> ('e -> 'f))) (
                                                (TApp: ('b -> ('c -> ('d -> ('e -> 'f)))) (
                                                    (function: ('a -> ('b -> ('c -> ('d -> ('e -> 'f)))))),
                                                    (a: 'a)
                                                )),
                                                (b: 'b)
                                            )),
                                            (c: 'c)
                                        )),
                                        (d: 'd)
                                    )),
                                    (e: 'e)
                                ))
                            ))
                        ))
                    ))
                ))
            ))
        ))
    ))
|}]
;;

(** Let rec in tests *)

let%expect_test _ =
  let open Ast in
  let _ =
    let e =
      [ ELet
          ( "sumn"
          , EFun
              ( "x"
              , ELetRecIn
                  ( "helper"
                  , EFun
                      ( "x"
                      , EIfThenElse
                          ( EBinop (Eq, EVar "x", EConst (CInt 1))
                          , EConst (CInt 1)
                          , EBinop (Add, EVar "x", EBinop (Sub, EVar "x", EConst (CInt 1)))
                          ) )
                  , EApp (EVar "helper", EVar "x") ) ) )
      ]
    in
    infer_statements e |> run_infer_statements
  in
  [%expect
    {|
    (TLet(
        sumn: (int -> int),
        (TFun: (int -> int) (
            (x: int),
            (TLetRecIn(
                helper: (int -> int),
                (TFun: (int -> int) (
                    (x: int),
                    (TIfThenElse: int
                        ((Eq: (int -> (int -> bool)) (
                            (x: int),
                            (TConst((CInt 1): int))
                        )),
                        (TConst((CInt 1): int)),
                        (Add: (int -> (int -> int)) (
                            (x: int),
                            (Sub: (int -> (int -> int)) (
                                (x: int),
                                (TConst((CInt 1): int))
                            ))
                        ))
                    ))
                )),
                (TApp: int (
                    (helper: (int -> int)),
                    (x: int)
                ))
            ))
        ))
    ))
|}]
;;

(** Factorial and fibonacci tests *)

let%expect_test _ =
  let open Ast in
  let _ =
    let e =
      [ ELetRec
          ( "fact"
          , EFun
              ( "n"
              , EIfThenElse
                  ( EBinop (Eq, EVar "n", EConst (CInt 1))
                  , EConst (CInt 1)
                  , EBinop
                      ( Mul
                      , EApp (EVar "fact", EVar "n")
                      , EApp (EVar "fact", EBinop (Sub, EVar "n", EConst (CInt 1))) ) ) )
          )
      ]
    in
    infer_statements e |> run_infer_statements
  in
  [%expect
    {|
    (TLetRec(
        fact: (int -> int),
        (TFun: (int -> int) (
            (n: int),
            (TIfThenElse: int
                ((Eq: (int -> (int -> bool)) (
                    (n: int),
                    (TConst((CInt 1): int))
                )),
                (TConst((CInt 1): int)),
                (Mul: (int -> (int -> int)) (
                    (TApp: int (
                        (fact: (int -> int)),
                        (n: int)
                    )),
                    (TApp: int (
                        (fact: (int -> int)),
                        (Sub: (int -> (int -> int)) (
                            (n: int),
                            (TConst((CInt 1): int))
                        ))
                    ))
                ))
            ))
        ))
    ))
|}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e =
      [ ELet
          ( "fac"
          , EFun
              ( "n"
              , ELetRecIn
                  ( "fact"
                  , EFun
                      ( "n"
                      , EFun
                          ( "acc"
                          , EIfThenElse
                              ( EBinop (Lt, EVar "n", EConst (CInt 1))
                              , EVar "acc"
                              , EApp
                                  ( EApp
                                      ( EVar "fact"
                                      , EBinop (Sub, EVar "n", EConst (CInt 1)) )
                                  , EBinop (Mul, EVar "acc", EVar "n") ) ) ) )
                  , EApp (EApp (EVar "fact", EVar "n"), EConst (CInt 1)) ) ) )
      ]
    in
    infer_statements e |> run_infer_statements
  in
  [%expect
    {|
    (TLet(
        fac: (int -> int),
        (TFun: (int -> int) (
            (n: int),
            (TLetRecIn(
                fact: (int -> (int -> int)),
                (TFun: (int -> (int -> int)) (
                    (n: int),
                    (TFun: (int -> int) (
                        (acc: int),
                        (TIfThenElse: int
                            ((Lt: (int -> (int -> bool)) (
                                (n: int),
                                (TConst((CInt 1): int))
                            )),
                            (acc: int),
                            (TApp: int (
                                (TApp: (int -> int) (
                                    (fact: (int -> (int -> int))),
                                    (Sub: (int -> (int -> int)) (
                                        (n: int),
                                        (TConst((CInt 1): int))
                                    ))
                                )),
                                (Mul: (int -> (int -> int)) (
                                    (acc: int),
                                    (n: int)
                                ))
                            ))
                        ))
                    ))
                )),
                (TApp: int (
                    (TApp: (int -> int) (
                        (fact: (int -> (int -> int))),
                        (n: int)
                    )),
                    (TConst((CInt 1): int))
                ))
            ))
        ))
    ))
|}]
;;
