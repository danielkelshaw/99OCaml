(** Problem 18: Extract a slice from a list.*)

let slice (list : 'a list) (i : int) (k : int) : 'a list =
  let rec take list n =
    match list with
    | [] -> []
    | hd :: tl -> if n = 0 then [] else hd :: take tl (n - 1)
  in
  let rec drop list n =
    match list with
    | [] -> []
    | hd :: tl as l -> if n = 0 then l else drop tl (n - 1)
  in
  take (drop list i)  (k - i + 1) 

