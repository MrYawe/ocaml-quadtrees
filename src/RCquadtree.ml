(**
  The type rcquadtree and its operations are designed to represent a
  rectangles collection quadtree.

  A rectangles collection quadtree is an adaptation of a binary tree used
  to represent a collection of rectangles.

  @author Yannis Weishaupt
 *)

open Point;;
open Rectangle;;

(**
  The sum type representing the rectangles collection quadtree with:
  {ul {- [RCEmpty] the empty rcquadtree}
      {- [RCNode] a node of the rcquadtree}}

  Each [RCNode] contains:
  {ul {- its surface represented by a {!type: Rectangle.rect}}
      {- its collection of rectangles that crosses the vertical median
          of its surface represented by a list of {!type: Rectangle.rect}}
      {- its collection of rectangles that crosses the horizontal median
          of its surface represented by a list of {!type: Rectangle.rect}}
      {- 4 children {!type:RCquadtree.rcquadtree}}}
 *)
type rcquadtree =
    RCEmpty
  | RCNode of rect * (rect list) * (rect list) *
      rcquadtree * rcquadtree * rcquadtree * rcquadtree;;

(**
  Exception raised when a given rcquadtree is inconsistent.

  See {!val:RCquadtree.is_consistent}.
 *)
exception InconsistentRCquadtree;;

(**
  Exception raised when a given [RCNode] is inconsistent.
 *)
exception InconsistentRCNode;;

(**
  The default width and height of the surface of a rcquadtree.

  Its value is [512].
 *)
let base_length = 512;;

(**
  The default default surface of a rcquadtree.

  Its value is [{top=base_length; right=base_length; bottom=0; left=0}].
 *)
let base_surface = {top=base_length; right=base_length; bottom=0; left=0};;

(**
  Insert the given rectangle in the given rcquadtree and return
  the new rcquadtree.

  TODO
  Raise [InvalidSurface] if the surface is invalid. See
  {!exception:RCquadtree.InvalidSurface}.

  TODO
  Raise [RectangleOutOfRange] if the given rectangle if out of range of the
  given rcquadtree.

  TODO
  Raise [InconsistentRCquadtree] if the given rcquadtree is inconsistent. See
  {!val:RCquadtree.is_consistent}.

  @param surface Optional parameter representing the surface of the first
  [RCNode] of a rcquadtree. This parameter will be used if the given rcquadtree
  is [RCEmpty]. Default is {!val:RCquadtree.base_surface}.
 *)
let rec rcinsert ?(surface = base_surface) rcquadtree rect =
  let aux = function
    | RCNode (s, lv, lh, q1, q2, q3, q4) ->
      let c = center s in (
        match (get_pole_rect rect s) with
        | NW -> RCNode (s, lv, lh, (rcinsert ~surface:{top=s.top; right=c.x; bottom=c.y; left=s.left} q1 rect), q2, q3, q4)
        | NE -> RCNode (s, lv, lh, q1, (rcinsert ~surface:{top=s.top; right=s.right; bottom=c.y; left=c.x} q2 rect), q3, q4)
        | SW -> RCNode (s, lv, lh, q1, q2, (rcinsert ~surface:{top=c.y; right=c.x; bottom=s.bottom; left=s.left} q3 rect), q4)
        | SE -> RCNode (s, lv, lh, q1, q2, q3, (rcinsert ~surface:{top=c.y; right=s.right; bottom=s.bottom; left=c.x} q4 rect))
      )
    | _ -> raise InconsistentRCNode
  in match rcquadtree with
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

(**
  Insert all given points in a new rcquadtree and return
  the new rcquadtree.

  TODO
  Raise [InvalidSurface] if the surface is invalid. See
  {!exception:RCquadtree.InvalidSurface}.

  TODO
  Raise [PointOutOfRange] if the given point if out of range of the given
  rcquadtree.

  TODO
  Raise [InconsistentPquadtree] if the given rcquadtree is inconsistent. See
  {!val:RCquadtree.is_consistent}.

  @param surface Optional parameter representing the surface of the first
  [RCNode] of a rcquadtree. This parameter will be used if the given rcquadtree
  is [RCEmpty]. Default is {!val:RCquadtree.base_surface}.
 *)
let rcinsert_list ?(surface = base_surface) rect_list =
  List.fold_left (rcinsert ~surface:surface) RCEmpty rect_list;;
  
(* TODO *)
let rccontain rcquadtree p =
  let rec aux acc = function
    | RCEmpty -> acc
    | RCNode (s, lv, lh, q1, q2, q3, q4) ->
      let res = contain_in_list p (lv@lh) in (
        match (get_pole p s) with
        | NW -> aux (res@acc) q1
        | NE -> aux (res@acc) q2
        | SW -> aux (res@acc) q3
        | SE -> aux (res@acc) q4
      )
  in aux [] rcquadtree;;

(**
  Return the string representation of the given rcquadtree.

  @param indent Optional parameter representing the number of spaces of
  the indentation. Default is [0].
 *)
let rec string_of_rcquadtree ?(indent=0) = function
  | RCEmpty -> "RCEmpty"
  | RCNode (s, lv, lh, q1, q2, q3, q4) ->
    let is = String.make indent ' ' and
    ss = string_of_rectangle s and
    lvs = string_of_rectangle_list ~indent:(indent) lv and
    lhs = string_of_rectangle_list ~indent:(indent) lh and
    q1s = string_of_rcquadtree ~indent:(indent+3) q1 and
    q2s = string_of_rcquadtree ~indent:(indent+3) q2 and
    q3s = string_of_rcquadtree ~indent:(indent+3) q3 and
    q4s = string_of_rcquadtree ~indent:(indent+3) q4 in
      Printf.sprintf
        "\n%slv:%s\n%slh:%s\n%sr:%s\n%sq1:%s\n%sq2:%s\n%sq3:%s\n%sq4:%s"
        is lvs is lhs is ss is q1s is q2s is q3s is q4s;;

(**
  The default graphical origin used by draw functions of this module.

  Its value is [{x=0; y=0}].
 *)
let base_g_origin = {x=0; y=0};;

(**
  Draw the given rcquadtree with the graphic module of OCaml.

  Raise [InconsistentRCquadtree] if the given rcquadtree is inconsistent.

  @param scale Optional scaling parameter. For example if [scale = 2] a
  rcquadtree's surface of height [10] will be drawn with a height
  of [20] pixels. Default is [1].
  TODO
  @param g_origin Optional parameter representing the graphical origin of the
  coordinate system where the rcquadtree is drawn. Default is
  {!val:RCquadtree.base_g_origin}.
 *)
let rec draw_rcquadtree ?(scale=1)  = function
  | RCEmpty -> ()
  | RCNode (s, lv, lh, q1, q2, q3, q4) ->
    draw_rectangle ~scale:scale s;
    draw_medians ~scale:scale s;
    Graphics.set_color Graphics.blue;
    List.iter (draw_rectangle ~scale:scale) lv;
    List.iter (draw_rectangle ~scale:scale) lh;
    Graphics.set_color Graphics.black;
    draw_rcquadtree ~scale:scale q1;
    draw_rcquadtree ~scale:scale q2;
    draw_rcquadtree ~scale:scale q3;
    draw_rcquadtree ~scale:scale q4;;
