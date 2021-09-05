(** Problem 62: Collect the internal notes of a binary tree in a list.*)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let internals (tree : 'a binary_tree) : 'a list =
  let rec aux acc tree =
    match tree with
    | Empty -> acc
    | Node (x, Empty, Empty) -> acc
    | Node (x, left, right) -> aux (x :: aux acc right) left
  in
    aux [] tree

let at_level (tree : 'a binary_tree) (level : int) : 'a list =
  let rec aux acc tree counter =
    match tree with
    | Empty -> acc
    | Node (x, left, right) ->
        if counter = level then x :: acc
        else aux (aux acc right (counter + 1)) left (counter + 1)
  in
    aux [] tree 1

