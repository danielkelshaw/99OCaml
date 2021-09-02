(** Problem 21: Insert an element at a given position into a list.*)

let rec insert_at (x : 'a) (n : int) (list : 'a list) : 'a list =
  match list with
  | [] -> [x]
  | hd :: tl as l -> if n = 0 then x :: l
                     else hd :: insert_at x (n - 1) tl

