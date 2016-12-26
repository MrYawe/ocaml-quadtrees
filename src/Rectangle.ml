type pole = NO | NE | SO | SE;;

type point = {x: int; y: int};;
exception InconsistentPoint;;

type rect = {top: int; bottom: int; left: int; right: int};;

let center rect = {x=(rect.right-rect.left)/2; y=(rect.top-rect.bottom)/2};;

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

let draw_point scale point =
  let size = 3 in
    Graphics.moveto ((point.x-size)*scale) (point.y*scale);
    Graphics.lineto ((point.x+size)*scale) (point.y*scale);

    Graphics.moveto (point.x*scale) ((point.y-size)*scale);
    Graphics.lineto (point.x*scale) ((point.y+size)*scale);;
