(******************************************************************************)
(*                                Point                                       *)
(******************************************************************************)

type point = {x: int; y: int};;
exception InconsistentPoint;;
exception MedianCrossed;;

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

let rec rectangle_list_equal list1 list2 =
  match (list1, list2) with
  | [], [] -> true
  | r1::l1, r2::l2 when rectangle_equal r1 r2 -> rectangle_list_equal l1 l2
  | _ -> false;;


let center rect =
  {
    x=(rect.right-rect.left)/2+rect.left;
    y=(rect.top-rect.bottom)/2+rect.bottom
  };;

(**
  Return the pole depending on the position of the given point against
  the given rectangle.
 *)
let get_pole point rect =
  let c = center rect in
    if point.x < c.x && point.y >= c.y then NO
    else if point.x >= c.x && point.y >= c.y then NE
    else if point.x < c.x && point.y < c.y then SO
    else if point.x >= c.x && point.y < c.y then SE
    else raise InconsistentPoint;;

(**
  Return the pole depending on the position of the first rectangle against
  the second rectangle. Raise an error if the first rectangle cross a median
  of the second rectangle.
 *)
let get_pole_rect rect1 rect2 =
  let c = center rect2 in
    if rect1.right < c.x && rect1.bottom > c.y then NO
    else if rect1.left > c.x && rect1.bottom > c.y then NE
    else if rect1.right < c.x && rect1.top < c.y then SO
    else if rect1.left > c.x && rect1.top < c.y then SE
    else raise MedianCrossed;;

(**
  True if the vertical median of the first rectangle cross the second
  rectangle.
 *)
let vertical_median_cross rect1 rect2 =
  let c = center rect1 in
    rect2.left <= c.x && c.x <= rect2.right;;

(**
  True if the horizontal median of the first rectangle cross the second
  rectangle.
 *)
let horizontal_median_cross rect1 rect2 =
  let c = center rect1 in
    rect2.bottom <= c.y && c.y <= rect2.top;;

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
