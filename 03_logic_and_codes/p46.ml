(** Problem 46/47: Truth tables for logical expressions.*)

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let rec eval (a : 'a) (val_a : bool) (b : 'a) (val_b : bool) (expr : bool_expr) : bool =
  match expr with
  | Var x -> if x = a then val_a
             else if x = b then val_b
             else failwith "Invalid variable"
  | Not e -> not (eval a val_a b val_b e)
  | And (e1, e2) -> eval a val_a b val_b e1 && eval a val_a b val_b e2
  | Or (e1, e2) -> eval a val_a b val_b e1 || eval a val_a b val_b e2

let table2 a b expr =
  [(true, true, eval a true b true expr);
   (true, false, eval a true b false expr);
   (false, true, eval a false b true expr);
   (false, false, eval a false b false expr)]

