open Rectangle;;

type rcquadtree =
    RCEmpty
  | RCNode of rect * (rect list) * (rect list) *
      rcquadtree * rcquadtree * rcquadtree * rcquadtree;;

exception InconsistentRCquadtree;;
exception InconsistentRCNode;;

let base_length = 512;;
let base_surface = {top=base_length; right=base_length; bottom=0; left=0};;

let rec rcquadtree_equal rcquadtree1 rcquadtree2 =
  match (rcquadtree1, rcquadtree2) with
  | RCNode (s1, lv1, lh1, q11, q12, q13, q14),
    RCNode (s2, lv2, lh2, q21, q22, q23, q24)
    when (rectangle_equal s1 s2) && (rectangle_list_equal lv1 lv2) &&
    (rectangle_list_equal lh1 lh2) ->
      (rcquadtree_equal q11 q21) && (rcquadtree_equal q12 q22) &&
      (rcquadtree_equal q13 q23) && (rcquadtree_equal q14 q24)
  | RCEmpty, RCEmpty -> true
  | _ -> false;;

let rec draw_rcquadtree scale = function
  | RCEmpty -> ()
  | RCNode (s, lv, lh, q1, q2, q3, q4) ->
    draw_rectangle scale s;
    draw_median scale s;
    Graphics.set_color Graphics.blue;
    List.iter (draw_rectangle scale) lv;
    List.iter (draw_rectangle scale) lh;
    Graphics.set_color Graphics.black;
    draw_rcquadtree scale q1;
    draw_rcquadtree scale q2;
    draw_rcquadtree scale q3;
    draw_rcquadtree scale q4;;

let rec rcinsert ?(surface = base_surface) rect rcquadtree =
  let aux = function
    | RCNode (s, lv, lh, q1, q2, q3, q4) ->
      let c = center s in (
        match (get_pole_rect rect s) with
        | NO -> RCNode (s, lv, lh, (rcinsert ~surface:{top=s.top; right=c.x; bottom=c.y; left=s.left} rect q1), q2, q3, q4)
        | NE -> RCNode (s, lv, lh, q1, (rcinsert ~surface:{top=s.top; right=s.right; bottom=c.y; left=c.x} rect q2), q3, q4)
        | SO -> RCNode (s, lv, lh, q1, q2, (rcinsert ~surface:{top=c.y; right=c.x; bottom=s.bottom; left=s.left} rect q3), q4)
        | SE -> RCNode (s, lv, lh, q1, q2, q3, (rcinsert ~surface:{top=c.y; right=s.right; bottom=s.bottom; left=c.x} rect q4))
      )
    | _ -> raise InconsistentRCNode
  in
  match rcquadtree with
  | RCEmpty when vertical_median_cross surface rect ->
    RCNode (surface, [rect], [], RCEmpty, RCEmpty, RCEmpty, RCEmpty)
  | RCEmpty when horizontal_median_cross surface rect ->
    RCNode (surface, [], [rect], RCEmpty, RCEmpty, RCEmpty, RCEmpty)
  | RCEmpty ->
    aux (RCNode (surface, [], [], RCEmpty, RCEmpty, RCEmpty, RCEmpty))
  | RCNode (s, lv, lh, q1, q2, q3, q4) when vertical_median_cross s rect ->
    RCNode (s, rect::lv, lh, q1, q2, q3, q4)
  | RCNode (s, lv, lh, q1, q2, q3, q4) when horizontal_median_cross s rect ->
    RCNode (s, lv, rect::lh, q1, q2, q3, q4)
  | RCNode (s, lv, lh, q1, q2, q3, q4) ->
    aux (RCNode (s, lv, lh, q1, q2, q3, q4));;

let rects_contain p rcquadtree =
  let rec aux acc = function
    | RCEmpty -> acc
    | RCNode (s, lv, lh, q1, q2, q3, q4) ->
      let res = contain_in_list p (lv@lh) in (
        match (get_pole p s) with
        | NO -> aux res q1
        | NE -> aux res q2
        | SO -> aux res q3
        | SE -> aux res q4
      )
  in aux [] rcquadtree;;
