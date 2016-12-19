open OUnit2;;
open Rectangle;;
open Pquadtree;;

let simple_pquadtree = PNode (
  {x=10; y=10},
  {top=16; right=16; bottom=0; left=0},
  PEmpty, PEmpty, PEmpty, PEmpty
);;

let test_pbelong test_ctxt =
  assert_equal true (pbelong {x=10; y=10} simple_pquadtree);;

(* Name the test cases and group them together *)
let tests =
"tests">::: [
  "test_pbelong">:: test_pbelong
];;
