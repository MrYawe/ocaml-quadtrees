(**
  The type point and its operations are designed to represent a point in a
  two-dimensional space.

  Points are represented by a record type.
 *)

type point = {x: int; y: int};;
exception InconsistentPoint;;
exception MedianCrossed;;

let base_g_origin = {x=0; y=0};;

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

let string_of_point point =
  Printf.sprintf "{x=%d; y=%d}" point.x point.y;;
