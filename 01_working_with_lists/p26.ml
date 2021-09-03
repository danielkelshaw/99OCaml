(** Problem 26: Generate the combinations of K distinct objects chosen from the
                N elements of a list.*)

let rec extract (k : int) (list : 'a list) : 'a list list =
  if k <= 0 then [[]]
  else
    match list with
    | [] -> []
    | hd :: tl ->
        let with_hd = List.map (fun l -> hd :: l) (extract (k - 1) tl) in
        let without_hd = extract k tl in
        with_hd @ without_hd

