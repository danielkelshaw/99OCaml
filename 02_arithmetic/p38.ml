(** Problem 38: Compare the two methods of calculating Euler's
                totient function.*)

let timeit f a =
  let t0 = Unix.gettimeofday() in
    ignore (f a);
  let t1 = Unix.gettimeofday() in
    t1 -. t0

let snd_phi_faster () : bool =
  let test_val = 10090 in
  let t_basic = timeit P34.phi test_val in
  let t_improved = timeit P37.phi_improved test_val in
  t_improved < t_basic

