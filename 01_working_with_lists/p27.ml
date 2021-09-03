(** Problem 27: Group the elements of a set into disjoint subsets.*)

let rec mapcan f list =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux ((f hd) @ acc) tl
  in
  aux [] list

let rec group (list : 'a list) (sizes : int list) : 'a list list list =
  let remove to_remove list =
    List.filter (fun elt -> not (List.mem elt to_remove)) list
  in
  let rec aux acc list size_list =
    match size_list with
    | [] -> [List.rev acc]
    | size :: rest_sizes -> mapcan (fun comb -> aux (comb :: acc) (remove comb list) rest_sizes)
                                   (P26.extract size list)
  in
  aux [] list sizes

