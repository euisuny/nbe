(* OCaml implementation of Kovacs' scheme of NbE for the untyped
   lambda-calculus *)
open Printf

type ix = int
type lvl = int

type tm =
  | Var of ix
  | Lam of tm
  | App of tm * tm
  | Let of tm * tm (* let t (x.u) *)

let rec to_string (t : tm) =
  match t with
  | Var i -> string_of_int i
  | Lam tm ->
    sprintf "lam (%s)" (to_string tm)
  | App (tm1, tm2) ->
    sprintf "app (%s, %s)" (to_string tm1) (to_string tm2)
  | Let (tm1, tm2) ->
    sprintf "let (%s, %s)" (to_string tm1) (to_string tm2)

(* "Semantic" values of the lambda calculus:
  Each term gets translated into an *open* singleton term model with De Bruijn
  levels and closures in place of abstraction *)
type value =
  | VVar of lvl
  | VApp of value * value
  | VLam of closure
and closure = Close of (value list) * tm

(* Analogous to [reflect] *)
let rec eval (env : value list) (t : tm) =
  match t with
  | Var x -> List.nth env x
  | App (t, u) ->
    (match (eval env t, eval env u) with
     | VLam (Close (env', t)), u -> eval (u::env') t
     | t, u -> VApp (t, u))
  | Lam t -> VLam (Close (env, t))
  | Let (t, u) ->
    eval (eval env t::env) u

(* Analogous to [reify] *)
let rec quote (l : lvl) (t : value) =
  match t with
  | VVar x ->
    (* Sort of doing "shifting" here *)
    Var (l - x - 1)
  | VApp (t, u) ->
    App (quote l t, quote l u)
  | VLam (Close (env, t)) ->
    Lam (quote (l + 1) (eval (VVar l :: env) t))

let nf (env : value list) (t : tm) =
  quote (List.length env) (eval env t)
