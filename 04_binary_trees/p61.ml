(** Problem 61: Count the leaves of a binary tree.*)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let rec count_leaves (tree : 'a binary_tree) : int =
  match tree with
  | Empty -> 0
  | Node (_, Empty, Empty) -> 1
  | Node (_, left, right) -> count_leaves left + count_leaves right

let leaves (tree : 'a binary_tree) : 'a list =
  let rec aux acc tree =
    match tree with
    | Empty -> acc
    | Node (x, Empty, Empty) -> x :: acc
    | Node (_, left, right) -> aux acc left @ aux acc right
  in
    aux [] tree

