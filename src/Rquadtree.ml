open Rectangle;;

type colors = White | Black ;;
type rquadtree =
    Plain of colors
  | RQ of rquadtree * rquadtree * rquadtree * rquadtree;;

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
