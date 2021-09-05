open OUnit2
open Batteries


open P70
let example_tree = (T ('a', [T ('f', []) ]))
let test_p70 =
  "P70" >::: [
    "T1" >:: (fun _ -> assert_equal 2 (P70.count_nodes example_tree));
  ]
 
let test_suite =
  "Problems" >:::
    [
      test_p70;
    ]

let _ = 
  run_test_tt_main test_suite

