open Point;;
open Rectangle;;

type colors = White | Black ;;
type rquadtree =
    Plain of colors
  | RQ of rquadtree * rquadtree * rquadtree * rquadtree;;

exception InconsistentEncoding;;


let rec rquadtree_equal rquadtree1 rquadtree2 =
  match (rquadtree1, rquadtree2) with
    | (RQ (q11, q12, q13, q14), RQ (q21, q22, q23, q24)) ->
        (rquadtree_equal q11 q21) && (rquadtree_equal q12 q22) &&
        (rquadtree_equal q13 q23) && (rquadtree_equal q14 q24)
    | (Plain c1, Plain c2) when c1=c2 -> true
    | _ -> false;;

let draw_rquadtree ?(scale=1) base_size rquadtree =
  let rec aux scale rect = function
    | Plain White ->
      draw_plain_rectangle ~scale:scale rect Graphics.white;
      draw_rectangle ~scale:scale rect
    | Plain Black ->
      draw_plain_rectangle ~scale:scale rect Graphics.black;
      draw_rectangle ~scale:scale rect
    | RQ (q1, q2, q3, q4) ->
      let c = center rect in
        aux scale {top=rect.top; right=c.x; bottom=c.y; left=rect.left} q1;
        aux scale {top=rect.top; right=rect.right; bottom=c.y; left=c.x} q2;
        aux scale {top=c.y; right=c.x; bottom=rect.bottom; left=rect.left} q3;
        aux scale {top=c.y; right=rect.right; bottom=rect.bottom; left=c.x} q4;
  in aux scale {top=base_size; right=base_size; bottom=0; left=0} rquadtree;;

let rec invert = function
  | Plain White -> Plain Black
  | Plain Black -> Plain White
  | RQ (q1, q2, q3, q4) -> RQ (invert q1, invert q2, invert q3, invert q4);;

let rec intersection rquadtree1 rquadtree2 =
  match (rquadtree1, rquadtree2) with
    | (RQ (q11, q12, q13, q14), RQ (q21, q22, q23, q24)) ->
        RQ (intersection q11 q21, intersection q12 q22, intersection q13 q23,
          intersection q14 q24)
    | (Plain Black, RQ (q1, q2, q3, q4)) | (RQ (q1, q2, q3, q4), Plain Black) ->
      RQ ((intersection (Plain Black) q1), (intersection (Plain Black) q3),
        (intersection (Plain Black) q3), (intersection (Plain Black) q4))
    | (Plain Black, Plain Black) -> Plain Black
    | _ -> Plain White;;

let rec union rquadtree1 rquadtree2 =
  match (rquadtree1, rquadtree2) with
    | (RQ (q11, q12, q13, q14), RQ (q21, q22, q23, q24)) ->
      let res = RQ (union q11 q21, union q12 q22, union q13 q23, union q14 q24)
      in (match res with
        | RQ (Plain Black, Plain Black, Plain Black, Plain Black) -> Plain Black
        | _ -> res)
    | (Plain White, RQ (q1, q2, q3, q4)) | (RQ (q1, q2, q3, q4), Plain White) ->
      RQ ((union (Plain White) q1), (union (Plain White) q3),
        (union (Plain White) q3), (union (Plain White) q4))
    | (Plain White, Plain White) -> Plain White
    | _ -> Plain Black;;

let rec vertical_symmetry = function
  | Plain c -> Plain c
  | RQ (q1, q2, q3, q4) ->
    RQ (vertical_symmetry q2, vertical_symmetry q1,
      vertical_symmetry q4, vertical_symmetry q3);;

let rec horizontal_symmetry = function
  | Plain c -> Plain c
  | RQ (q1, q2, q3, q4) ->
    RQ (vertical_symmetry q3, vertical_symmetry q4,
      vertical_symmetry q1, vertical_symmetry q2);;

let code rquadtree =
  let rec code_step acc = function
    | Plain White -> 1::0::acc
    | Plain Black -> 1::1::acc
    | RQ (q1, q2, q3, q4) ->
      0::(code_step (code_step (code_step (code_step acc q4) q3) q2) q1)
  in code_step [] rquadtree;;

let decode l =
  let rec decode_step = function
    | 1::0::l -> Plain White, l
    | 1::1::l -> Plain Black, l
    | 0::l ->
      let q1, l = decode_step l in
      let q2, l = decode_step l in
      let q3, l = decode_step l in
      let q4, l = decode_step l in
        RQ (q1, q2, q3, q4), l
    | _ -> raise InconsistentEncoding
  in let rqt, _ = decode_step l in rqt;;
