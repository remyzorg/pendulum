
open OUnit

let test_bitset ctx =
  let open Runtime_misc in
  let open Bitset in
  let bs = Bitset.make 100 in
  let b1 = mem bs 1 || mem bs 2 || mem bs 31 || mem bs 100 in
  add bs 0; add bs 1; add bs 31; add bs 100;
  let b2 = mem bs 0 && mem bs 1 && mem bs 31 && mem bs 100 in
  assert_bool "Error in implementation of Bitset" (not b1 && b2)



let suite =
  "Test_misc">::: [
    "bitset">:: test_bitset;
  ] |> run_test_tt_main
