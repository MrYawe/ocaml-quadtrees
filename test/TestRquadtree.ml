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



(******************************************************************************)
(*                              Test union                                    *)
(******************************************************************************)

let rqt_union_1 = RQ (
  Plain Black,
  Plain White,
  RQ (Plain Black, Plain White, Plain White, Plain Black),
  Plain White
);;
let rqt_union_2 = RQ (
  Plain Black,
  Plain Black,
  RQ (Plain Black, Plain White, Plain White, Plain White),
  Plain White
);;

let rqt_union_result = RQ (
  Plain Black,
  Plain Black,
  RQ (Plain Black, Plain White, Plain White, Plain Black),
  Plain White
);;

let test_union test_ctxt =
  assert_equal (rquadtree_equal rqt_union_result (union rqt_union_1 rqt_union_2)) true;;

(******************************************************************************)
(*                                 Code                                       *)
(******************************************************************************)

let rqt_code_1 = RQ (
  Plain Black,
  Plain White,
  RQ (Plain Black, Plain White, Plain White, Plain Black),
  Plain White
);;

let rqt_code_1_res = [0;1;1;1;0; 0;1;1;1;0;1;0;1;1; 1;0];;

let test_code test_ctxt =
  assert_equal (code rqt_code_1) rqt_code_1_res;;

(* Name the test cases and group them together *)
let tests =
"tests">::: [
  "test_intersection">:: test_intersection;
  "test_union">:: test_union;
  "test_code">:: test_code;
];;
