(** Problem 34: Calculate Euler's totient function \phi(m).*)

let phi (n : int) : int =
  let rec aux acc curr_n =
    if curr_n = 1 then acc + 1
    else
      match P33.coprime curr_n n with
      | true -> aux (acc + 1) (curr_n - 1)
      | false -> aux acc (curr_n - 1)
  in
  aux 0 n

