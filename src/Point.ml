(**
  The type point and its operations are designed to represent a two-dimensional
  point within the context of quadtrees.

  @author Yannis Weishaupt
 *)

(**
 The record type representing the two-dimensional point.
*)
type point = {x: int; y: int};;

(**
  The default graphical origin used by draw functions of this module.

  Its value is [{x=0; y=0}].
 *)
let base_g_origin = {x=0; y=0};;

(**
  Draw the given point with the graphic module of OCaml.

  @param scale Optional scaling parameter. For example if [scale = 2] a point
  will appear two times bigger.
  @param g_origin Optional parameter representing the graphical origin of the
  coordinate system where the point is drawn. Default is
  {!val:Point.base_g_origin}.
 *)
let draw_point ?(scale=1) ?(g_origin = base_g_origin) point =
  let size = 3 in
    Graphics.moveto
      ((point.x-size+g_origin.x)*scale)
      ((point.y+g_origin.y)*scale);
    Graphics.lineto
      ((point.x+size+g_origin.x)*scale)
      ((point.y+g_origin.y)*scale);

    Graphics.moveto
      ((point.x+g_origin.x)*scale)
      ((point.y-size+g_origin.y)*scale);
    Graphics.lineto
      ((point.x+g_origin.x)*scale)
      ((point.y+size+g_origin.y)*scale);;

(**
  Return the string representation of the given point.
 *)
let string_of_point point =
  Printf.sprintf "{x=%d; y=%d}" point.x point.y;;
