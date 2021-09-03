(** Problem 37: Calculate Euler's totient function \phi(m) (improved).*)

let rec pow n p =
  if p < 1 then 1
  else n * pow n (p - 1)

let phi_improved (m : int) : int =
  List.fold_left ( * ) 1 (List.map (fun (p, m) -> (p - 1) * pow p (m - 1)) (P36.factors m))

