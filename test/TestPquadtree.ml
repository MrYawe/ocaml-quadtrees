open OUnit2;;
open Rectangle;;
open Pquadtree;;

let simple_pquadtree = PNode (
  {x=10; y=10},
  {top=16; right=16; bottom=0; left=0},
  PEmpty, PEmpty, PEmpty, PEmpty
);;

let test_pbelong test_ctxt =
  assert_equal (pbelong {x=10; y=10} simple_pquadtree) true;;



let pqdt2 = (insert {x=300; y=10} (insert {x=30; y=30} PEmpty));;
let pqdt2_result =
PNode (
  {x=30; y=30},
  {top=base_length; right=base_length; bottom=0; left=0},
  PEmpty, PEmpty, PEmpty, PNode (
    {x=300; y=10},
    {top=(base_length/2); right=base_length; bottom=0; left=(base_length/2)},
    PEmpty, PEmpty, PEmpty, PEmpty
  )
);;

let test_insert test_ctxt =
  assert_equal (pquadtree_equal pqdt2 pqdt2_result) true;;

(* Name the test cases and group them together *)
let tests =
"tests">::: [
  "test_pbelong">:: test_pbelong;
  "test_insert">:: test_insert
];;
