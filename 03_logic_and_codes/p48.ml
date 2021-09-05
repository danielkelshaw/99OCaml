(** Problem 48: Truth tables for logical expressions.*)

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let rec eval val_vars expr =
  match expr with
  | Var x -> List.assoc x val_vars
  | Not e -> not (eval val_vars e)
  | And (e1, e2) -> eval val_vars e1 && eval val_vars e2
  | Or (e1, e2) -> eval val_vars e1 || eval val_vars e2

let table (vars : 'a list) (expr : bool_expr) : (('a * bool) list * bool) list =
  let rec aux acc vars expr =
    match vars with
    | [] -> [(List.rev acc, eval acc expr)]
    | hd :: tl ->
        aux ((hd, true) :: acc) tl expr @ aux ((hd, false) :: acc) tl expr
  in
    aux [] vars expr

