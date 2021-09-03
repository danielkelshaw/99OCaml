(** Problem 23: Extract a given number of randomly selected elements
                from a list.*)

let rand_select (list : 'a list) (n : int) : 'a list =
  let rec extract acc n list =
    match list with
    | [] -> raise Not_found
    | hd :: tl -> if n = 0 then (hd, acc @ tl)
                  else extract (hd :: acc) (n - 1) tl
  in
  let rec extract_random list len =
    extract [] (Random.int len) list
  in
  let rec aux acc n list len =
    if n = 0 then acc
    else
      let picked, the_rest = extract_random list len in
      aux (picked :: acc) (n - 1) the_rest (len - 1)
  in
  let len = List.length list in
    aux [] (min len n) list len

