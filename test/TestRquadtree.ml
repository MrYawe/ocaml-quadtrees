open OUnit2;;
open Rectangle;;
open Rquadtree;;


(******************************************************************************)
(*                            Test intersection                               *)
(******************************************************************************)

let rqt_inter_1 = RQ (
  Plain Black,
  Plain White,
  RQ (Plain Black, Plain White, Plain White, Plain Black),
  Plain White
);;
let rqt_inter_2 = RQ (
  Plain Black,
  Plain Black,
  RQ (Plain Black, Plain White, Plain White, Plain White),
  Plain White
);;

let rqt_inter_result = RQ (
  Plain Black,
  Plain White,
  RQ (Plain Black, Plain White, Plain White, Plain White),
  Plain White
);;

let test_intersection test_ctxt =
  assert_equal (rquadtree_equal rqt_inter_result (intersection rqt_inter_1 rqt_inter_2)) true;;

(* Name the test cases and group them together *)
let tests =
"tests">::: [
  "test_intersection">:: test_intersection;
];;
