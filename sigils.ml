(*
 *  Copyright (c) 2005, 2006, 2007, 2009 Abram Hindle
 *  
 *  This file is part of Sigils
 *  This file was part of CaptchaBreaker (digg.ml)

 *  Sigils is free software; you can redistribute it and/or modify
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
open Cmlib;;
open Sobel;;



(* sigils/ohm-font/ *)
let sigils_chars = [ "Do" ; "Ka"; "Ohm"; "Xs" ] ;;
let sigil_font = "sigils/ohm-font4";;
let our_rotter b =
  let pts = Rotter.rotter b in
    Contour.scale_points (-100.0,-100.0) (100.0,100.0) pts
;;

let load_sigils () = 
  let label_bitmaps = List.map (fun (s,(b,f)) -> (s,b)) (Captchas.load_dir_chars sigils_chars  sigil_font) in
    Abez.mapi (fun i (s,b) -> 
                 let out = "segments/Char." ^ (s) ^ "."^(string_of_int i)^".pts" in				      
                   prerr_endline out;
                   let pts = our_rotter b in
	             Rotter.print_rotter_to_file out pts;
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
    (l,d)
;;

let find_closest_all_reflections labels pt_list =
  let dists =
    List.map (
      find_closest labels)
      (
        pt_list :: (
          List.map (fun (x,y) -> List.map (fun (x',y') -> (x *. x', y*. y')) pt_list)
            [(-1.0,1.0) ; (-1.0,-1.0) ; (1.0,-1.0)]
        )
      )
  in
    List.fold_left 
      (fun (l,d) (l',d') ->
         if (d < d') then
           (l,d)
         else
           (l',d')) (List.hd dists) dists
;;

let arg_parse_files () = 
  let files = ref [] in
    Arg.parse [] (fun s -> files := s :: !files) "bitmap files";
    let files = List.rev !files in
      files
;;

let sigil_segmenter rgb =
  Captchas.fill_segmenter ~diag:false ~istext:is_black rgb
;;

let safe_basename file =
  try
    get_basename file
  with 
      _ -> file
;;
  

let sigil_identify db file rgb =
  (* prerr_endline "Segment rgb"; *)
  let segments = sigil_segmenter rgb in
  let segments = segment_filter (15*15) segments in

    if (List.length segments > 30) then
      []
    else
      begin
        (* prerr_endline "Sort Segments rgb"; *)
        let segments = segment_sort segments in 
          (* prerr_endline "Rotter segments"; *)
          let pts_list = Abez.mapi (
            fun i (x,region) ->
              (* rotter might not be the best, we might just try bitmap comparison *)
              let out = "segments/" ^ (get_basename file) ^ ".seg."^(string_of_int i) in				    

              (* prerr_endline "Run our rotter"; *)
                let pts = our_rotter x in
                  (* prerr_endline "Print our rotter"; *)
                  
	          (* Rotter.print_rotter_to_file out pts; *)
	          (pts,region,x,out)) segments in
            (* prerr_endline "Find closest"; *)
            let labels = List.map 
              (fun (pts,region,bmp,out) -> 
                 let (m,d) = find_closest_all_reflections db pts in
                   (* let out = (out^"."^m^".png") in 
                      prerr_endline ("writing "^out);
	              bmp#save out (Some Png) [];   *)
                   (m,d,region) ) 
              pts_list in
              labels
      end
;;
    


let sigil_main () =
  Random.self_init ();
  let files = arg_parse_files () in
  let db = load_sigils () in
    List.iter (
      fun file ->
        let sigil_identify = sigil_identify db file in
	  print_endline ("File: "^file);
        let rgb = Captchas.load_rgb_file file in
          List.iter
            (fun (best_match,distance,region) ->
               print_endline best_match;               
               print_float distance;
               print_endline "";
               Captchas.print_region region;
               print_endline "";
            )
            (sigil_identify rgb)
    ) files
;;

let cleanup bmp =
  let width = bmp#width in
  let height = bmp#height in
  for y = 0 to 4 do
    for x = 0 to (width - 1) do
      bmp#unsafe_set x y { r = 255; g = 255; b = 255 }
    done;
  done;
    bmp
;;

let sigil_webcam () =
  let width = 324 in
  let height = 248 in
  let keyname = "324x248_rgb_int_webcam" in
  let db = load_sigils () in
  let (sock,fdi,fdo) = Cmlib.cm_connect "127.0.0.1" 9911 in
  let frame = ref 0 in
    Cmlib.cm_lossyrequire fdo keyname;
    while(true) do
      let (buffer:int array) = Cmlib.cm_read_buffer fdi in
        (* prerr_endline ("Got a buffer "^(string_of_int !frame)); *)
        let bmp = Captchas.flatarray_to_bmp ~f:rgb width height buffer in
        let filename = "segments/Webcam-frame-"^(string_of_int !frame)^".png" in
          (* now we clean up the image! *)
          (* prerr_endline "Shrink buffer!"; *)
          let shrunk = Shrinker.shrink bmp in
          let shrunk = cleanup shrunk in
            (*  prerr_endline ("Saving buffer " ^ filename);
	        shrunk#save filename (Some Png) [];    *)
            (* prerr_endline "Identify"; *)
            let identities = sigil_identify db filename  shrunk in
              (* prerr_endline "Report";  *)
              List.iter
                (fun (best_match,distance,region) ->
                   let (x1,y1,x2,y2) = region in
                     print_endline 
                     (String.concat " "
                       [ string_of_float (Unix.time ()) ;
                         best_match ;
                         string_of_float distance ;
                         string_of_int x1;
                         string_of_int y1;
                         string_of_int x2 ;
                         string_of_int y2
                       ]);
                ) identities;
                
              frame := !frame + 1;
    done;
;;
        
