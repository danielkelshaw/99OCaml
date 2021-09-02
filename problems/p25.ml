(** Problem 25: Generate a random permutation of the elements of a list.*)

let permutation (list : 'a list) : 'a list =
  let rec extract acc n list =
    match list with
    | [] -> raise Not_found
    | hd :: tl -> if n = 0 then (hd, acc @ tl)
                  else extract (hd :: acc) (n - 1) tl
  in
  let extract_random list len =
    extract [] (Random.int len) list
  in
  let rec aux acc list len =
    match list with
    | [] -> acc
    | _ -> let chosen, the_rest = extract_random list len in
           aux (chosen :: acc) the_rest (len - 1)
  in
  let len = List.length list in
    aux [] list len

