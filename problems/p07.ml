(** Problem 07: Flatten a nested list structure.*)

type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten (list : 'a node list) : 'a list =
  let rec _flatten acc list =
    match list with
    | [] -> acc
    | One x :: tl -> _flatten (x :: acc) tl
    | Many l :: tl -> _flatten (_flatten acc l) tl
  in
  P05.rev (_flatten [] list)

