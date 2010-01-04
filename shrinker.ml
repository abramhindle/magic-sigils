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
open Shape;;
open Captchas;;

let get_red bmp x y =
  (bmp#get x y).r
;;

let get_getter_of_bmp white bmp = 
    let height = bmp#height  in
    let width = bmp#width  in  
    let getter x y =
      if (x >= width || x < 0) then
	white
      else if (y >= height || y < 0) then
	white
      else
	get_red bmp x y
    in
      (height,width,getter)
;;

let shrink ?n:(npix=8) bmp =
    let white = 255 in
    let (height,width,getter) = get_getter_of_bmp white bmp in
    let bmp' = new rgb24 width height in

    let minwhite = 128 in
    let white_col = { r = 255 ; g = 255; b = 255 } in
    let black_col = { r = 0 ; g = 0; b = 0 } in
    let dxdy = [ (-1,-1) ; (-1,0) ; (-1,1) ;
		 (0,-1) ;  (0,1) ;
		 (1,-1) ; (1,0) ; (1,1) ;
	       ] in
(*    let dxdy = [ (-1,0) ; (1,0) ; (0,-1) ; (0,1) ] in
    let dxdy = [ (-1,0) ; (1,0) ; (0,-1) ; (0,1) ] in *)
(*    let dxdy = [ (0,-1) ; (0,1) ] in *)
(*    let n = List.length dxdy in *)
    let n = npix in
    Abez.for_xy width height (
      fun x y ->
	
	let r = List.fold_left (fun o (dx,dy) ->
			  if ((getter (x+dx) (y+dy)) > minwhite) then
			    o
			  else
			    o + 1
			       ) 0 dxdy in
	  if ((getter x y) < minwhite && r >= n) then
	    bmp'#unsafe_set x y black_col
	  else
	    bmp'#unsafe_set x y white_col
    );
    bmp'
;;

let shrink_and_grow ?just_grow:(only_grow=false) npix bmp =
    let white = 255 in
    let (height,width,getter) = get_getter_of_bmp white bmp in
    let bmp' = new rgb24 width height in

    let minwhite = 128 in
    let white_col = { r = 255 ; g = 255; b = 255 } in
    let black_col = { r = 0 ; g = 0; b = 0 } in
    let dxdy = [ (-1,-1) ; (-1,0) ; (-1,1) ;
		 (0,-1) ;  (0,1) ;
		 (1,-1) ; (1,0) ; (1,1) ;
	       ] in
(*    let dxdy = [ (-1,0) ; (1,0) ; (0,-1) ; (0,1) ] in *)
(*    let dxdy = [ (-1,0) ; (1,0) ; (0,-1) ; (0,1) ] in  *)
(*    let dxdy = [ (0,-1) ; (0,1) ] in *)
    let get_black_or_white x y = 
      if (getter x y > minwhite) then
	white_col
      else
	black_col
    in
    let n = List.length dxdy in
    Abez.for_xy width height (
      fun x y ->
	
	let r = List.fold_left (fun o (dx,dy) ->
			  if ((getter (x+dx) (y+dy)) > minwhite) then
			    o
			  else
			    o + 1
			       ) 0 dxdy in
(*	  if ((getter x y) < minwhite && r >= n) then   *)
	    if (r >= npix) then
	      bmp'#unsafe_set x y black_col
	    else if (not only_grow) then
	      bmp'#unsafe_set x y white_col
	    else
	      bmp'#unsafe_set x y (get_black_or_white x y)
    );
    bmp'
;;

let just_grow npix bmp = shrink_and_grow ~just_grow:true npix bmp ;;


type seg = NoSegment | Segment of int;;


let cumulative_seg bmp fileout =
  let white = 255 in
  let (height,width,getter) = get_getter_of_bmp white bmp in
  let minwhite = 64 in
  let arr = Array.create width 0 in
    for i = 0 to (width-1) do
      for j = 0 to (height-1) do
	if ((getter i j) < minwhite) then
	  arr.(i) <- arr.(i) + 1;
      done
    done;
    let fd = open_out fileout in
      Array.iteri (fun i x -> 
		     output_string fd ((string_of_int i) ^ " " ^ (string_of_int x) ^"\n");
		  ) arr;
      close_out fd;
      let sum = Array.fold_left (+) 0 arr in
      let n = (int_of_float ((float_of_int sum) /. (float_of_int width))) in
      let find_segments arr =
	let rec f currseg i = 
	  if (i < width) then
	    match currseg with
	      NoSegment ->
		if (arr.(i) >= n) then
		  f (Segment(i)) (i+1)
		else
		  f NoSegment (i+1)
	      | Segment(j) ->
		  if (arr.(i) >= n) then
		    f (Segment(j)) (i+1)
		  else
		    (j,0,i-1,height-1) :: f NoSegment (i+1)
	  else
	    []
	in
	  f NoSegment 0
      in
      let shrink_segment (x1,y1,x2,y2) =
	let arr = Array.create height 0 in
	  for i = 0 to (height - 1) do
	    for j = x1 to x2 do
	      arr.(i) <- 
		arr.(i) + 
		(if ((getter j i) < minwhite) then 1 else 0)
	      ;
	    done;
	  done;
	  let y = [| 0 ; height-1 |] in
	  let index = ref 0 in
	  for i = 0 to (height - 1) do
	    if (!index = 0) then (* coming down *)
	      if (arr.(i) = 0) then
		y.(0) <- i 
	      else
		begin
		  index := 1;
		  y.(1) <- i;
		  y.(0) <- i;
		end
	    else
	      if (arr.(i) = 0) then
		()
	      else
		y.(1) <- i
	  done;
	    (x1,y.(0),x2,y.(1))
      in

      let hor_segs =   find_segments arr in
	List.map shrink_segment hor_segs
;;
      

