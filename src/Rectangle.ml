(******************************************************************************)
(*                                Point                                       *)
(******************************************************************************)

type point = {x: int; y: int};;
exception InconsistentPoint;;

let point_equal p1 p2 =
  match (p1, p2) with
  | (p1, p2) when p1.x=p2.x && p1.y=p2.y -> true
  | _ -> false;;

let draw_point scale point =
  let size = 3 in
    Graphics.moveto ((point.x-size)*scale) (point.y*scale);
    Graphics.lineto ((point.x+size)*scale) (point.y*scale);

    Graphics.moveto (point.x*scale) ((point.y-size)*scale);
    Graphics.lineto (point.x*scale) ((point.y+size)*scale);;

(******************************************************************************)
(*                              Rectangle                                     *)
(******************************************************************************)

type pole = NO | NE | SO | SE;;
type rect = {top: int; bottom: int; left: int; right: int};;

let rectangle_equal rect1 rect2 =
  match (rect1, rect2) with
  | (r1, r2) when r1.top=r2.top && r1.bottom=r2.bottom &&
                  r1.left=r2.left && r1.right=r2.right -> true
  | _ -> false;;

let center rect =
  {x=(rect.right-rect.left)/2+rect.left; y=(rect.top-rect.bottom)/2+rect.bottom};;

let get_pole point rect =
  let c = center rect in
    if point.x < c.x && point.y >= c.y then NO
    else if point.x >= c.x && point.y >= c.y then NE
    else if point.x < c.x && point.y < c.y then SO
    else if point.x >= c.x && point.y < c.y then SE
    else raise InconsistentPoint;;

(* let draw_rectangle rect =
  Graphics.draw_rect rect.left rect.bottom
    (rect.right-rect.left) (rect.top-rect.bottom);; *)

let draw_rectangle scale rect =
  Graphics.draw_rect (rect.left*scale) (rect.bottom*scale)
    ((rect.right-rect.left)*scale) ((rect.top-rect.bottom)*scale);;

let draw_median scale rect =
  let c = center rect in
    Graphics.moveto (c.x*scale) (rect.bottom*scale);
    Graphics.lineto (c.x*scale) (rect.top*scale);
    Graphics.moveto (rect.left*scale) (c.y*scale);
    Graphics.lineto (rect.right*scale) (c.y*scale);;

let draw_plain_rectangle scale rect color =
  Graphics.set_color color;
  Graphics.fill_rect (rect.left*scale) (rect.bottom*scale)
    ((rect.right-rect.left)*scale) ((rect.top-rect.bottom)*scale);
  Graphics.set_color Graphics.black;;
