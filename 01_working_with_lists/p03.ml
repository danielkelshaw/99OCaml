(** Problem 03: Find the K'th element of a list.*)

let rec at (k : int) (list : 'a list) : 'a option =
  match list with
  | [] -> None
  | hd :: tl -> if k = 1 then Some hd
                else at (k - 1) tl

