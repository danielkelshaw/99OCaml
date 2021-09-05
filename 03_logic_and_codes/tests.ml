open OUnit2
open Batteries

open P46
let test_p46 =
  "P46" >::: [
    "T1" >:: (fun _ -> assert_equal [(true, true, true); (true, false, true); (false, true, false); (false, false, false)]
                                    (P46.table2 "a" "b" (And (Var "a", Or (Var "a", Var "b")))))
  ]

open P48
let test_p48 =
  "P48" >::: [
    "T1" >:: (fun _ -> assert_equal [([("a", true); ("b", true); ("c", true)], true);
                                     ([("a", true); ("b", true); ("c", false)], true);
                                     ([("a", true); ("b", false); ("c", true)], true);
                                     ([("a", true); ("b", false); ("c", false)], false);
                                     ([("a", false); ("b", true); ("c", true)], false);
                                     ([("a", false); ("b", true); ("c", false)], false);
                                     ([("a", false); ("b", false); ("c", true)], false);
                                     ([("a", false); ("b", false); ("c", false)], false)]
                                    (P48.table ["a"; "b"; "c"] (Or (And (Var "a", Or (Var "b", Var "c")), Or (And (Var "a", Var "b"), And (Var "a", Var "c"))))))
  ]

let test_suite =
  "Problems" >:::
    [
      test_p46;
      test_p48;
    ]

let _ = 
  run_test_tt_main test_suite

