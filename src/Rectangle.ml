(**
  The type rectangle and its operations are designed to represent a rectangle
  within the context of quadtrees.

  @author Yannis Weishaupt
 *)

open Point;;

(**
  The record type representing the rectangle with:
  {ul {- y coordinate of the superior edge}
      {- y coordinate of the inferior edge}
      {- x coordinate of the left edge}
      {- x coordinate of the right edge}}
 *)
type rect = {top: int; bottom: int; left: int; right: int};;

(**
  The sum type representing the four intercardinal directions
  northwest, northeast, southwest and southeast.

  A given rectangle can be divided in 4 equal rectangles corresponding to
  an intercardinal direction.
 *)
type intercardinal_direction = NW | NE | SW | SE;;

(**
  Exception raised when a given rectangle is inconsistent.

  {!val:Rectangle.is_consistent}
 *)
exception Inconsistent_Rectangle;;

(**
  Return [true] if the given rectangle is consistent and [false] otherwise.

  A consistent rectangle is a rectangle with a width and a height strictly
  positive.
 *)
let is_consistent rect =
  let w = (rect.right-rect.left)/2
  and h = (rect.top-rect.bottom)/2 in
    (w > 0) && (h > 0);;

(**
  Return the width of the given rectangle.

  Raise [Inconsistent_Rectangle] if the given rectangle is inconsistent.
 *)
let width rect =
  if not (is_consistent rect) then raise Inconsistent_Rectangle;
  rect.right-rect.left;;

(**
  Return the height of the given rectangle.

  Raise [Inconsistent_Rectangle] if the given rectangle is inconsistent.
 *)
let height rect =
  if not (is_consistent rect) then raise Inconsistent_Rectangle;
  rect.top-rect.bottom;;

(**
  Return [true] if the width and the height of the rectangle is
  a power of two and [false] otherwise.

  Raise [Inconsistent_Rectangle] if the given rectangle is inconsistent.
 *)
let rect_is_a_power_of_two rect =
  if not (is_consistent rect) then raise Inconsistent_Rectangle;
  let w = width rect and h = height rect in
    ((w land (w - 1)) == 0) && ((h land (h - 1)) == 0);;

(**
  Return the center of the given rectangle.

  Raise [Inconsistent_Rectangle] if the given rectangle is inconsistent.
 *)
let center rect =
  if not (is_consistent rect) then raise Inconsistent_Rectangle;
  {
    x=(width rect)/2+rect.left;
    y=(height rect)/2+rect.bottom
  };;

(**
  Return the pole depending on the position of the given point against
  the given rectangle.

  Raise [Inconsistent_Rectangle] if the given rectangle is inconsistent.

  For example with the point [p] and the rectangle [rect] of center [c]
  it will return:
  {ul {- [NW] if [p.x < c.x && p.y >= c.y]}
      {- [NE] if [point.x >= c.x && point.y >= c.y]}
      {- [SW] if [p.x < c.x && p.y < c.y]}
      {- [SE] if [p.x >= c.x && p.y < c.y]}}
 *)
let get_pole point rect =
  if not (is_consistent rect) then raise Inconsistent_Rectangle;
  let c = center rect in
    if point.x < c.x && point.y >= c.y then NW
    else if point.x >= c.x && point.y >= c.y then NE
    else if point.x < c.x && point.y < c.y then SW
    else if point.x >= c.x && point.y < c.y then SE
    else failwith "get_pole: inconsistent point";;

(**
  Return the pole depending on the position of the first rectangle against
  the second rectangle. Raise an error if the first rectangle crosses a median
  of the second rectangle.

  Raise [Inconsistent_Rectangle] if one of the given rectangles is inconsistent.

  For example with the rectangle [r1] and the rectangle [r2] of center [c]
  it will return:
  {ul {- [NW] if [r1.right < c.x && r1.bottom > c.y]}
      {- [NE] if [r1.left > c.x && r1.bottom > c.y]}
      {- [SW] if [r1.right < c.x && r1.top < c.y]}
      {- [SE] if [r1.left > c.x && r1.top < c.y]}
      {- a [Failure] if [r1] crosses a median of [r2]}}
 *)
let get_pole_rect rect1 rect2 =
  if not (is_consistent rect1) then raise Inconsistent_Rectangle;
  if not (is_consistent rect2) then raise Inconsistent_Rectangle;
  let c = center rect2 in
    if rect1.right < c.x && rect1.bottom > c.y then NW
    else if rect1.left > c.x && rect1.bottom > c.y then NE
    else if rect1.right < c.x && rect1.top < c.y then SW
    else if rect1.left > c.x && rect1.top < c.y then SE
    else failwith "get_pole_rect: median crossed";;

(**
  Return the rectangle at the given pole of the given rectangle.

  Raise [Inconsistent_Rectangle] if the given rectangle is inconsistent.
 *)
let get_rect_at_pole pole rect =
  if not (is_consistent rect) then raise Inconsistent_Rectangle;
  let c = center rect in match pole with
  | NW -> {top=rect.top; right=c.x; bottom=c.y; left=rect.left}
  | NE -> {top=rect.top; right=rect.right; bottom=c.y; left=c.x}
  | SW -> {top=c.y; right=c.x; bottom=rect.bottom; left=rect.left}
  | SE -> {top=c.y; right=rect.right; bottom=rect.bottom; left=c.x};;

(**
  Return [true] if the given rectangle contains the given point.

  Raise [Inconsistent_Rectangle] if the given rectangle is inconsistent.
 *)
let contain_point point rect =
  if not (is_consistent rect) then raise Inconsistent_Rectangle;
  point.x >= rect.left && point.x <= rect.right &&
  point.y >= rect.bottom && point.y <= rect.top;;

(**
  Return all rectangles given in the rectangles list that contain the
  given point.

  Raise [Inconsistent_Rectangle] if one of the given rectangles is inconsistent.
 *)
let contain_in_list point li =
  List.fold_left
    (fun acc r -> if contain_point point r then (r::acc) else acc) [] li;;

(**
  Return [true] if the vertical median of the first rectangle crosses the second
  rectangle.

  Raise [Inconsistent_Rectangle] if one of the given rectangles is inconsistent.
 *)
let vertical_median_cross rect1 rect2 =
  if not (is_consistent rect1) then raise Inconsistent_Rectangle;
  let c = center rect1 in
    rect2.left <= c.x && c.x <= rect2.right;;

(**
  Return [true] if the horizontal median of the first given rectangle crosses
  the second given rectangle.

  Raise [Inconsistent_Rectangle] if one of the given rectangles is inconsistent.
 *)
let horizontal_median_cross rect1 rect2 =
  if not (is_consistent rect2) then raise Inconsistent_Rectangle;
  let c = center rect1 in
    rect2.bottom <= c.y && c.y <= rect2.top;;

(**
  Return the string representation of the given intercardinal direction.
 *)
let string_of_intercard = function
  | NW -> "NW"
  | NE -> "NE"
  | SW -> "SW"
  | SE -> "SE";;

(**
  Return the string representation of the given intercardinal direction list.
 *)
let string_of_intercard_list li =
  let m = List.map string_of_intercard li in
    String.concat " " m;;

(**
  Return the string representation of the given rectangle.
 *)
let string_of_rectangle rect =
  Printf.sprintf
    "{top=%d; bottom=%d; left=%d; right=%d}"
    rect.top rect.bottom rect.left rect.right;;

(**
  Return the string representation of the given rectangles list.
 *)
let string_of_rectangle_list ?(indent=0) li =
  let rect_strings = List.map string_of_rectangle li and
  i = String.make indent ' ' and
  i2 = String.make (indent+3) ' ' in match li with
  | [] -> Printf.sprintf "\n%s[]" i 
  | _ ->
    let rect_strings = String.concat (Printf.sprintf ";\n%s" i2) rect_strings in
      Printf.sprintf "\n%s[\n%s%s\n%s]" i i2 rect_strings i;;

(**
  The default graphical origin used by draw functions of this module.

  Its value is [{x=0; y=0}].
 *)
let base_g_origin = {x=0; y=0};;

(**
  Draw the given rectangle with the graphic module of OCaml.

  Raise [Inconsistent_Rectangle] if the given rectangle is inconsistent.

  @param scale Optional scaling parameter. For example if [scale = 2] a
  rectangle with a height of [10] will be drawn with a height of [20] pixels.
  Default is [1].
  @param g_origin Optional parameter representing the graphical origin of the
  coordinate system where the rectangle is drawn. Default is
  {!val:Rectangle.base_g_origin}.
 *)
let draw_rectangle ?(scale=1) ?(g_origin = base_g_origin) rect =
  if not (is_consistent rect) then raise Inconsistent_Rectangle;
  Graphics.draw_rect
    ((rect.left+g_origin.x)*scale)
    ((rect.bottom+g_origin.y)*scale)
    ((width rect)*scale)
    ((height rect)*scale);;

(**
  Draw the given rectangle filled by the given color
  with the graphic module of OCaml.

  Raise [Inconsistent_Rectangle] if the given rectangle is inconsistent.

  @param scale Optional scaling parameter. For example if [scale = 2] a
  rectangle with a height of [10] will be drawn with a height of [20] pixels.
  Default is [1].
  @param g_origin Optional parameter representing the graphical origin of the
  coordinate system where the rectangle is drawn. Default is
  {!val:Rectangle.base_g_origin}.
 *)
let draw_plain_rectangle ?(scale=1) ?(g_origin = base_g_origin) rect color =
  if not (is_consistent rect) then raise Inconsistent_Rectangle;
  Graphics.set_color color;
  Graphics.fill_rect
    ((rect.left+g_origin.x)*scale)
    ((rect.bottom+g_origin.y)*scale)
    ((width rect)*scale)
    ((height rect)*scale);
  Graphics.set_color Graphics.black;;

(**
  Draw the medians of the given rectangle with the graphic module of OCaml.

  Raise [Inconsistent_Rectangle] if the given rectangle is inconsistent.

  @param scale Optional scaling parameter. For example if [scale = 2] a
  rectangle with a height of [10] will be drawn with a vertical median of
  [20] pixels. Default is [1].
  @param g_origin Optional parameter representing the graphical origin of the
  coordinate system where the medians are drawn. Default is
  {!val:Rectangle.base_g_origin}.
 *)
let draw_medians ?(scale=1) ?(g_origin = base_g_origin) rect =
  if not (is_consistent rect) then raise Inconsistent_Rectangle;
  let c = center rect in
    Graphics.moveto ((c.x+g_origin.x)*scale) ((rect.bottom+g_origin.y)*scale);
    Graphics.lineto ((c.x+g_origin.x)*scale) ((rect.top+g_origin.y)*scale);
    Graphics.moveto ((rect.left+g_origin.x)*scale) ((c.y+g_origin.y)*scale);
    Graphics.lineto ((rect.right+g_origin.x)*scale) ((c.y+g_origin.y)*scale);;
