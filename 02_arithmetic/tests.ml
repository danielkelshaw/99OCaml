open OUnit2
open Batteries

let test_p31 =
  "P31" >::: [
    "T1" >:: (fun _ -> assert_equal true (P31.is_prime 7));
    "T2" >:: (fun _ -> assert_equal false (P31.is_prime 12))
  ]

let test_p32 =
  "P32" >::: [
    "T1" >:: (fun _ -> assert_equal 1 (P32.gcd 13 27));
    "T2" >:: (fun _ -> assert_equal 2 (P32.gcd 20536 7826))
  ]

let test_p33 =
  "P33" >::: [
    "T1" >:: (fun _ -> assert_equal true (P33.coprime 13 27));
    "T2" >:: (fun _ -> assert_equal false (P33.coprime 20536 7826))
  ]

let test_p34 =
  "P34" >::: [
    "T1" >:: (fun _ -> assert_equal 4 (P34.phi 10));
    "T2" >:: (fun _ -> assert_equal 12 (P34.phi 13))
  ]

let test_p35 =
  "P35" >::: [
    "T1" >:: (fun _ -> assert_equal [3; 3; 5; 7] (P35.factors 315))
  ]

let test_p36 =
  "P36" >::: [
    "T1" >:: (fun _ -> assert_equal [(3, 2); (5, 1); (7, 1)] (P36.factors 315))
  ]

let test_p37 =
  "P37" >::: [
    "T1" >:: (fun _ -> assert_equal 4 (P37.phi_improved 10));
    "T2" >:: (fun _ -> assert_equal 12 (P37.phi_improved 13))
  ]

let test_p38 =
  "P38" >::: [
    "T1" >:: (fun _ -> assert_equal true (P38.snd_phi_faster ()))
  ]

let test_p39 = 
  "P39" >::: [
    "T1" >:: (fun _ -> assert_equal 1000 (List.length (P39.all_primes 2 7920)))
  ]

let test_suite =
  "Problems" >:::
    [
      test_p31;
      test_p32;
      test_p33;
      test_p34;
      test_p35;
      test_p36;
      test_p37;
      test_p38;
    ]

let _ = 
  run_test_tt_main test_suite

