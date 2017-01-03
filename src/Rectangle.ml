open Point;;

type pole = NO | NE | SO | SE;;
type rect = {top: int; bottom: int; left: int; right: int};;

let base_g_origin = {x=0; y=0};;

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

let get_rect_at_pole pole rect =
  let c = center rect in match pole with
  | NO -> {top=rect.top; right=c.x; bottom=c.y; left=rect.left}
  | NE -> {top=rect.top; right=rect.right; bottom=c.y; left=c.x}
  | SO -> {top=c.y; right=c.x; bottom=rect.bottom; left=rect.left}
  | SE -> {top=c.y; right=rect.right; bottom=rect.bottom; left=c.x};;

(**
  True if the given rectangle contains the given point.
 *)
let contain point rect =
  point.x >= rect.left && point.x <= rect.right &&
  point.y >= rect.bottom && point.y <= rect.top;;

(**
  Return all rectangles given in the rectangle list that contain the given point
 *)
let contain_in_list point li =
  List.fold_left
    (fun acc r -> if contain point r then (r::acc) else acc) [] li;;

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

let string_of_rectangle rect =
  Printf.sprintf
    "{top=%d; bottom=%d; left=%d; right=%d}"
    rect.top rect.bottom rect.left rect.right;;

let string_of_rectangle_list ?(indent=0) li =
  let rect_strings = List.map string_of_rectangle li and
  i = String.make indent ' ' and
  i2 = String.make (indent+3) ' ' in
    let rect_strings = String.concat (Printf.sprintf ";\n%s" i2) rect_strings in
      Printf.sprintf "\n%s[\n%s%s\n%s]" i i2 rect_strings i;;

let draw_rectangle ?(scale=1) ?(g_origin = base_g_origin) rect =
  Graphics.draw_rect
    ((rect.left*scale)+g_origin.x)
    ((rect.bottom*scale)+g_origin.y)
    ((rect.right-rect.left)*scale)
    ((rect.top-rect.bottom)*scale);;

let draw_median ?(scale=1) rect =
  let c = center rect in
    Graphics.moveto (c.x*scale) (rect.bottom*scale);
    Graphics.lineto (c.x*scale) (rect.top*scale);
    Graphics.moveto (rect.left*scale) (c.y*scale);
    Graphics.lineto (rect.right*scale) (c.y*scale);;

let draw_plain_rectangle ?(scale=1) rect color =
  Graphics.set_color color;
  Graphics.fill_rect (rect.left*scale) (rect.bottom*scale)
    ((rect.right-rect.left)*scale) ((rect.top-rect.bottom)*scale);
  Graphics.set_color Graphics.black;;
