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
  assert_equal rqt_inter_result (intersection rqt_inter_1 rqt_inter_2);;



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
  assert_equal rqt_union_result (union rqt_union_1 rqt_union_2);;

(******************************************************************************)
(*                         Test vertical_symmetry                             *)
(******************************************************************************)

let rqt_vsymme_1 = RQ (
  Plain Black,
  Plain White,
  RQ (Plain Black, Plain White, Plain White, Plain Black),
  Plain White
);;

let rqt_vsymme_result_1 = RQ (
  Plain White,
  Plain Black,
  Plain White,
  RQ (Plain White, Plain Black, Plain Black, Plain White)
);;

let test_vertical_symmetry test_ctxt =
  assert_equal rqt_vsymme_result_1 (vertical_symmetry rqt_vsymme_1);;


(******************************************************************************)
(*                         Test horizontal_symmetry                           *)
(******************************************************************************)

let rqt_hsymme_1 = RQ (
  Plain Black,
  Plain White,
  RQ (Plain Black, Plain White, Plain White, Plain Black),
  Plain White
);;

let rqt_hsymme_result_1 = RQ (
  RQ (Plain White, Plain Black, Plain Black, Plain White),
  Plain White,
  Plain Black,
  Plain White
);;

let test_horizontal_symmetry test_ctxt =
  assert_equal rqt_hsymme_result_1 (horizontal_symmetry rqt_hsymme_1);;

(******************************************************************************)
(*                               Test code                                    *)
(******************************************************************************)

let rqt_code_1 = RQ (
  Plain Black,
  Plain White,
  RQ (Plain Black, Plain White, Plain White, Plain Black),
  Plain White
);;

let rqt_code_1_res = [0; 1;1; 1;0; 0;1;1;1;0;1;0;1;1; 1;0];;

let test_code test_ctxt =
  assert_equal (code rqt_code_1) rqt_code_1_res;;

(******************************************************************************)
(*                              Test decode                                   *)
(******************************************************************************)

let list_decode_1 = [0; 1;1; 1;0; 0;1;1;1;0;1;0;1;1; 1;0];;

let rqt_decode_1_res = RQ (
  Plain Black,
  Plain White,
  RQ (Plain Black, Plain White, Plain White, Plain Black),
  Plain White
);;

let list_decode_2 = [0; 1;0; 1;1;0; 0;0;1;0;1;1;0;1;0; 1;1];;

let test_decode test_ctxt =
  assert_equal rqt_decode_1_res (decode list_decode_1);;
  assert_raises InconsistentEncoding (fun _ -> decode list_decode_2);;

(* Name the test cases and group them together *)
let tests =
"tests">::: [
  "test_intersection">:: test_intersection;
  "test_union">:: test_union;
  "test_vertical_symmetry">:: test_vertical_symmetry;
  "test_horizontal_symmetry">:: test_horizontal_symmetry;
  "test_code">:: test_code;
  "test_decode">:: test_decode;
];;
