(** Problem 20: Remove the K'th element from a list.*)

let rec remove_at (n : int) (list : 'a list) : 'a list =
  match list with
  | [] -> []
  | hd :: tl -> if n = 0 then tl
                else hd :: remove_at (n - 1) tl

