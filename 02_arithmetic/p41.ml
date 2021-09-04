(** Problem 41: A list of Goldbach compositions.*)

let rec goldbach_list (a : int) (b : int) : (int * (int * int)) list =
  if a > b then []
  else
    match a mod 2 = 0 with
    | true -> (a, P40.goldbach a) :: goldbach_list (a + 2) b
    | false -> goldbach_list (a + 1) b

let goldbach_limit a b lim =
  List.filter (fun (_, (n1, n2)) -> n1 > lim && n2 > lim) (goldbach_list a b)

