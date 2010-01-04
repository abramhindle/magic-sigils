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
open Rotter;;
open Contour;;
open Kmeans;;

let digg_chars = [ "2" ; "3" ; "4" ; "6" ; "8" ; "A" ; "a" ; "B" ; "b" ; "C" ; "D" ; "d" ; "E" ; "e" ; "F" ; "f" ; "G" ; "H" ; "h" ; "J" ; "K" ; "L" ; "M" ; "m" ; "n" ; "P" ; "p" ; "q" ; "R" ; "r" ; "S" ; "T" ; "t" ; "U" ; "V" ; "W" ; "X" ; "Y" ];;
let load_digg () = 
  let label_bitmaps = List.map (fun (s,(b,f)) -> (s,b)) (Captchas.load_dir_chars digg_chars  "fonts/digg") in
  List.map (fun (s,b) -> 
	      let pts = Rotter.rotter b in	  
		(s,pts)
	   ) label_bitmaps
;;
let pnt_dist (x,y) (x2,y2) =
  let x' =  (x2 -. x) in
  let y' =  (y2 -. y) in
    x' *. x'  +. y' *. y'
;;
let mindist pl pnt =
  let x::xs = pl in
  let d = pnt_dist x pnt in
  List.fold_left (fun (d,p) p' ->
		    let d' = pnt_dist pnt p' in
		      if (d < d') then
			(d,p)
		      else
			(d',p')
		 ) (d,x) xs
;;
let pnt_list_distance pt_list1 pt_list2 =
  let l = List.map (fun pt ->
		      let (d,p) =  mindist pt_list2 pt in
			(d,pt,p)
		   ) pt_list1 in
  let sum = List.fold_left (fun s (d,p,p') ->
			      s +. d
			   ) 0. l in
    sum
;;
		    
let find_closest label_bitmaps pt_list =
  let x::xs = label_bitmaps in
  let (l,p) = x in
  let d = pnt_list_distance p pt_list in
  let (d,l,p) =  List.fold_left (fun (d,label,pl) (label',pl')  ->
				   let d' = pnt_list_distance pt_list pl' in
				     if (d < d') then
				       (d,label,pl)
				     else
				       (d',label',pl')
				) (d,l,p) xs in
    l
;;


let digg_segmenter file = 
	  let outfile1 = "segments/" ^ (get_basename file) ^ ".shrink1.jpg" in
	  let plotfile = "segments/" ^ (get_basename file) ^ ".data" in
	  let digg_letters = 5 in
	  let shrunk = 4 in
	  let rgb = Captchas.load_rgb_file file in
	  let rgb = Shrinker.shrink ~n:shrunk rgb in  
(*	  let rgb = Shrinker.shrink ~n:shrunk rgb in   *)
	(*  let rgb = Shrinker.shrink_and_grow 5 rgb in *)
	  let rgb = Shrinker.just_grow (shrunk+1) rgb in 
(*	  let rgb = Shrinker.just_grow 6 rgb in *)
	    rgb#save outfile1 (Some Png) [];
	    let width = rgb#width in
	    let height = rgb#height in
(*
	  let rgb = Shrinker.shrink_and_grow shrunk rgb in

*)

(*	  let segments = Shrinker.cumulative_seg rgb plotfile in  *)
(*	  let segments = Captchas.fill_segmenter ~diag:true ~istext:is_black rgb in  *)
	  

(*	  let segments = Captchas.segmenter ~istext:is_black rgb in  *)
	    let pts =  Contour.bmp_to_points rgb in
	    let segments = Kmeans.kmeans digg_letters (0.,(float_of_int (width-1))) (0.,(float_of_int (height-1))) pts in
	    let segments = List.map Contour.points_to_bmp segments in
	      
	    let segments = segment_filter 200 segments in
	    let segments = segment_sort segments in 
	      
	    let segbmps = List.map (fun (b,_) -> b) segments in 
	      (*	  let segbmps = List.map (Captchas.segment_of rgb) segments in *)
	    let segbmps = List.filter (
	      fun bmp ->
		let width = bmp#width in
		let height = bmp#height in

		  (width * height > 100)
	    ) segbmps in
	      segbmps
;;

let arg_parse_files () = 
  let files = ref [] in
    Arg.parse [] (fun s -> files := s :: !files) "bitmap files";
    let files = List.rev !files in
      files
;;


let solver digg files = 
  List.iter (
    fun file ->
      print_endline file;
      let segbmps = digg_segmenter file in
      let pts_list = Abez.mapi (
	fun i x ->
	  let out = "segments/" ^ (get_basename file) ^ ".seg."^(string_of_int i)^".pts" in				      
	  let pts = Rotter.rotter x in	  
	    Rotter.print_rotter_to_file out pts;
	    pts) segbmps in
      let labels = List.map 
	( find_closest digg ) 
	pts_list in
      let out = String.concat "" labels in
	print_endline out;
  ) files
;;

let digg_style_solver segmenter db files =
  List.iter (
    fun file ->
      print_endline file;
      let segbmps = segmenter file in
      let pts_list = Abez.mapi (
	fun i x ->
          (* rotter might not be the best, we might just try bitmap comparison *)
	  let out = "segments/" ^ (get_basename file) ^ ".seg."^(string_of_int i)^".pts" in				      
	  let pts = Rotter.rotter x in	  
	    Rotter.print_rotter_to_file out pts;
	    pts) segbmps in
      let labels = List.map 
	( find_closest db ) 
	pts_list in
      let out = String.concat "" labels in
	print_endline out;
  ) files
;;

let segmenter_main segmenter = 
  let files = arg_parse_files () in
    List.iter (
      fun file ->
	print_endline file;
	let segbmps = segmenter file in
	  Abez.iteri (fun i x ->
			let out = "segments/" ^ (get_basename file) ^ ".seg."^(string_of_int i)^".png" in
			  x#save out (Some Png) [];
		     ) segbmps;
    ) files
;;
