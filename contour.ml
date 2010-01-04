(*
 *  Copyright (c) 2005, 2006, 2007 Abram Hindle
 *  
 *  This file is part of CaptchaBreaker

 *  CaptchaBreaker is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 3 of the License, or
 *  (at your option) any later version.

 *  Foobar is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.

 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
open Images;;
open OImages;;
open Abez;;
open Captchas;;
open Shrinker;;

(* TODO:
   vectors from points 
*)

let print_contour_to_file f l =
  let fd = open_out f in
    List.iter (fun (x,y) ->
		 output_string fd ((string_of_int x) ^ " " ^ (string_of_int y) ^ "\n")
	      )
      l;
      close_out fd;
;;

let print_floating_contour_to_file f l =
  let fd = open_out f in
    List.iter (fun (x,y) ->
		 output_string fd ((string_of_float x) ^ " " ^ (string_of_float y) ^ "\n")
	      )
      l;
      close_out fd;
;;


let contour bmp =
  let d = [ 
    (-1,-1) ; (-1,0) ; (-1,1) ;
    ( 0,-1) ;          ( 0,1) ;
    ( 1,-1) ; ( 1,0) ; ( 1,1) 
  ] in
  let height = bmp#height  in
  let width = bmp#width  in  
  let getter x y =
    if (x >= width || x < 0) then 
      false
    else if (y >= height || y < 0) then
      false
    else
      is_text (bmp#get x y)
  in
  let not_text_getter x y = not (getter x y) in
  prerr_endline ((string_of_int width) ^ " " ^(string_of_int height));
  let l = ref [] in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
	if (getter x y) then
	  let b = List.exists (
	    fun (dx,dy) ->
	      let x' = dx + x in
	      let y' = dy + y in
		    not_text_getter x' y'
	  ) d in
	    if (b) then
	      l := (x,y) :: !l
	    else
	      ()
      done;
    done;
    !l
;;
(*let follow_path l =
  let l = List.sort compare l in
  let rec find_most (x,l) ol = function
      if (
*)
let avg_pt l = 
  let (x,y,n) = List.fold_left ( fun (ox,oy,n) (nx,ny) ->
			    (ox + nx, nx + ny, n + 1)
			) (0,0,0) l in
  let avgx = (float_of_int x) /. (float_of_int n) in
  let avgy = (float_of_int y) /. (float_of_int n) in
    (avgx,avgy)
;;
let fold = List.fold_left ;;
let total_distance (x,y) l =
  let d' = fold (
    fun d (x',y') ->
      let dx = x' - x in
      let dy = y' - y in
	d + dx*dx + dy*dy
  ) 0 l in
    d'
;;
  
let center_pt l =
  let (x,y,d) = fold (
    fun (x',y',d') (x,y) ->
      let d = total_distance (x,y) l in
	if (d' < d) then
	  (x',y',d')
	else
	  (x,y,d)
  ) (0,0,0) l
  in
    (x,y)
;;
(* let to_vector l =
  let (cx,cy) = center_pt l in
    
  let 
    
*)

let bmp_to_points bmp =
  let height = bmp#height  in
  let width = bmp#width  in  
  let pts = ref [] in
    for y = 0 to (height - 1) do
      for x = 0 to (width - 1) do
	if (is_text (bmp#get x y)) then
	  pts := (float_of_int x,float_of_int y)  :: !pts 
      done;
    done;
    !pts
;;
let points_to_bmp pts =
  let arr = [| 10000000. ; 10000000. ; -1. ; -1. |] in
  let white_col = { r = 255 ; g = 255; b = 255 } in
  let black_col = { r = 0 ; g = 0; b = 0 } in

  List.iter (fun (x,y) ->
	       if (x < arr.(0)) then
		 arr.(0) <- x
	       else if (x > arr.(2)) then
		 arr.(2) <- x
	       ;
	       if (y < arr.(1)) then
		 arr.(1) <- y
	       else if (y > arr.(3)) then
		 arr.(3) <- y
	       ;
	    ) pts;
    let w = (int_of_float (arr.(2) -. arr.(0))) + 1 in
    let h = (int_of_float (arr.(3) -. arr.(1))) + 1 in
    let bmp = new rgb24 w h in
      for y = 0 to (h - 1) do
	for x = 0 to (w - 1) do
	  bmp#unsafe_set x y white_col
	done
      done;
	
      List.iter (fun (x,y) ->
		   let x' = int_of_float  (x -. arr.(0)) in
		   let y' = int_of_float  (y -. arr.(1)) in
		     bmp#unsafe_set x' y' black_col
		) pts;
      (bmp,
       (
	 int_of_float arr.(0),
	 int_of_float arr.(1),
	 int_of_float arr.(2),
	 int_of_float arr.(3)))
;;
	    

let scale_points min_pt max_pt pts =
  let min_pt' = 
    let x::xs =  pts in
    List.fold_left min x xs
  in
  let max_pt' = 
    let x::xs =  pts in
    List.fold_left max x xs
  in
  let safediv alt x y =
    x /.
      (if (y = 0.) then alt else y)
  in
  let scalar_mult s (x,y) =
    (s *. x , s *. y)
  in
  let scalar2_mult u v (x,y) =
    (u *. x , v *. y)
  in

  let safediv = safediv 1.0 in
  let (x,y) = min_pt in
  let (x',y') = min_pt' in
  let minx =  safediv x  x' in
  let miny =  safediv y  y' in

  let (x,y) = max_pt in
  let (x',y') = max_pt' in
  let maxx =  safediv x  x' in
  let maxy =  safediv y  y' in
    
  let scalex =   min maxx minx in
  let scaley =   min maxx minx in

    List.map (scalar2_mult scalex scaley) pts
;;
