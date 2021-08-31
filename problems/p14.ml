(** Problem 14: Duplicate the elements of a list.*)

let rec duplicate (list : 'a list) : 'a list =
  match list with
  | [] -> []
  | hd :: tl -> hd :: hd :: duplicate tl

