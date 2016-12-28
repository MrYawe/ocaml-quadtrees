open Rectangle;;

type colors = White | Black ;;
type rquadtree =
    Plain of colors
  | RQ of rquadtree * rquadtree * rquadtree * rquadtree;;


let rec rquadtree_equal rquadtree1 rquadtree2 =
  match (rquadtree1, rquadtree2) with
    | (RQ (q11, q12, q13, q14), RQ (q21, q22, q23, q24)) ->
        (rquadtree_equal q11 q21) && (rquadtree_equal q12 q22) &&
        (rquadtree_equal q13 q23) && (rquadtree_equal q14 q24)
    | (Plain c1, Plain c2) when c1=c2 -> true
    | _ -> false;;

let draw_rquadtree scale base_size rquadtree =
  let rec draw_rquadtree_step scale rect = function
    | Plain White ->
      draw_plain_rectangle scale rect Graphics.white;
      draw_rectangle scale rect
    | Plain Black ->
      draw_plain_rectangle scale rect Graphics.black;
      draw_rectangle scale rect
    | RQ (q1, q2, q3, q4) ->
      let c = center rect in
        draw_rquadtree_step scale {top=rect.top; right=c.x; bottom=c.y; left=rect.left} q1;
        draw_rquadtree_step scale {top=rect.top; right=rect.right; bottom=c.y; left=c.x} q2;
        draw_rquadtree_step scale {top=c.y; right=c.x; bottom=rect.bottom; left=rect.left} q3;
        draw_rquadtree_step scale {top=c.y; right=rect.right; bottom=rect.bottom; left=c.x} q4;
  in draw_rquadtree_step scale {top=base_size; right=base_size; bottom=0; left=0} rquadtree;;

let rec invert = function
  | Plain White -> Plain Black
  | Plain Black -> Plain White
  | RQ (q1, q2, q3, q4) -> RQ (invert q1, invert q2, invert q3, invert q4);;

let rec intersection rquadtree1 rquadtree2 =
  match (rquadtree1, rquadtree2) with
    | (Plain White, _) | (_, Plain White) -> Plain White
    | (Plain Black, Plain Black) -> Plain Black
    | (Plain Black, RQ (q1, q2, q3, q4)) | (RQ (q1, q2, q3, q4), Plain Black) ->
      RQ ((intersection (Plain Black) q1), (intersection (Plain Black) q3),
        (intersection (Plain Black) q3), (intersection (Plain Black) q4))
    | (RQ (q11, q12, q13, q14), RQ (q21, q22, q23, q24)) ->
        RQ (intersection q11 q21, intersection q12 q22, intersection q13 q23,
          intersection q14 q24);;
