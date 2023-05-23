type ix = int
type lvl = int

type tm =
  | Var of ix
  | Lam of tm
  | App of tm * tm
  | Let of tm * tm (* let t (x.u) *)

type value and closure

val to_string : tm -> string

val nf : value list -> tm -> tm
