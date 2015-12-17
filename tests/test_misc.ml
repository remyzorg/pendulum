
open OUnit

let test_bitset ctx =
  let open Runtime_misc in
  let open Bitset in
  let bs = Bitset.make 100 in
  let b1 = mem bs 1 || mem bs 2 || mem bs 31 || mem bs 100 in
  add bs 0; add bs 1; add bs 31; add bs 100;
  let b2 = mem bs 0 && mem bs 1 && mem bs 31 && mem bs 100 in
  assert_bool "Error in implementation of Bitset" (not b1 && b2)

let sq = Seqlist [
    MLenter 1;
    MLenter 2;
    MLexit 7;
    MLexit 3;
    MLexit 6;
    MLenter 3;
    MLenter 4;
    MLexit 8;
    MLenter 5;
  ]

let test_bitset2 ctx =
  let Some (bs1, bs2), Seqlist [MLenters_exits (bs1', bs2')] = gather_enter_exits None sq 10 in
  assert_bool "Error Bitset" (Bitset.mem bs1 1);
  assert_bool "Error Bitset" (Bitset.mem bs1 2);
  assert_bool "Error Bitset" (Bitset.mem bs1 3);
  assert_bool "Error Bitset" (Bitset.mem bs1 4);
  assert_bool "Error Bitset" (Bitset.mem bs1 5);

  assert_bool "Error Bitset" (bs1 == bs1' && bs2 == bs2');
  assert_bool "Error Bitset" (not (Bitset.mem bs2 7));
  assert_bool "Error Bitset" (not @@ Bitset.mem bs2 6);
  assert_bool "Error Bitset" (not @@ Bitset.mem bs2 8);
  assert_bool "Error Bitset" (not @@ Bitset.mem bs2 3)



let suite =
  "Test_misc">::: [
    "bitset">:: test_bitset;
  ] |> run_test_tt_main
