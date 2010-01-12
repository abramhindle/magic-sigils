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

let arg_parse_files () = 
  let files = ref [] in
    Arg.parse [] (fun s -> files := s :: !files) "bitmap files";
    let files = List.rev !files in
      files
;;

let safe_basename file =
  try
    get_basename file
  with 
      _ -> file
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

let make_veve width height =
  Array.make (height*width) 0
;;

let copy_veve veve =
  Array.copy veve
;;


let age_combine width height arr bmp =
  let veve = copy_veve arr in
    for yi = 0 to height - 1 do
      let base = width * yi in
      for xi = 0 to width - 1 do
        let i = base + xi in
        let pixel = bmp#get xi yi in
        let vi = veve.(i) in
        let n = if (pixel.r > 215) then 
          (* vi - 2 *) (* (9 * vi / 10) - 1 *)
            vi - 4
        else
          vi + 1
        in
          (*
            let n = 100 * veve.(i) 
            (pixel.r > 215)
          (* negative *)
            
            else
            2
            )
        in *)
          veve.(i) <- if (n < 0) then 0 else n
      done;
    done;
    veve
;;

let arr_max veve = 
  let len = Array.length veve in
  let m = ref veve.(0) in
    for i = 0 to (len - 1) do
      if (veve.(i) > !m) then
      m := veve.(i)
    done;
    let m = !m in
      m
;;
(* csv output *)
let veve_to_file width height veve file =
  let fd = open_out file in
    for yi = 0 to height - 1 do
      let base = width * yi in
        for xi = 0 to width - 1 do
          let i = base + xi in
            output_string fd ((string_of_int veve.(i))^ " ")
        done;
        output_string fd "\n";
    done;
    close_out fd;
;;

let veve_to_bmp width height veve =
  let rgb24 = new rgb24 width height in
  let m = max 1 (arr_max veve) in
    for yi = 0 to height - 1 do
      let base = width * yi in
        for xi = 0 to width - 1 do
          let i = base + xi in
          let c = 255 * (max 0 veve.(i)) / m in 
            rgb24#unsafe_set xi yi { r = c ; g = c; b = c } 
        done;
    done;
    rgb24
;;

let veve_to_bmp_file width height veve file =
  let bmp = veve_to_bmp width height veve in
    bmp#save file (Some Png) [];  
;;


let veve_to_rgbintarr width height veve =
  let m = max 1 (arr_max veve) in
    Array.init (Array.length veve) 
      (fun i -> 
         let v = 255 * veve.(i) / m in
         let v2 = 256 * v in
         let v3 = 256 * v2 in
           v + v2 + v3
      )
;;

let sort_veve width veve =
  let sorted_veve =
    Array.init (Array.length veve) 
      (fun i ->
         (veve.(i),(i/width),(i mod width)))
  in
    Array.sort (fun ((x:int),_,_) ((y:int),_,_) -> 
                  if (x > y) then 
                    1
                  else if (y > x) then
                    -1
                  else 
                    0)
      sorted_veve;
    
    sorted_veve
;;

(* produces a path of (age,x,y) *)
let vectorify_veve width height veve =
  let sveve = sort_veve width veve in
  let len = Array.length sveve in
  let rec helper acc oldage (sumx,sumy) n i =
    if (i >= len) then
      if (n = 0) then acc
      else ((oldage,(sumx/n),(sumy/n))::acc)
    else
      let (age,x,y) = sveve.(i) in
        if (age = oldage) then
          helper acc age (sumx+x,sumy+y) (n+1) (i+1)
        else if (n = 0) then
          helper acc age (x,y) 1 (i+1)
        else
          let acc' = (oldage,sumx/n,sumy/n) :: acc in
            helper acc' age (x,y) 1 (i+1)
  in
  let age_centroids = helper [] (-1) (0,0) 0 0 in
    age_centroids
;;
let pi = 4.0 *. atan 1.0;;
let fabs x = 
  if (x >= 0.0) then x else ((-1.0)*.x)
;;
let angle_between (ax,ay) (cx,cy) (bx,by) =
  let ax' = ax -. cx in
  let ay' = ay -. cy in
  let bx' = bx -. cx in
  let by' = by -. cy in
  (* let dot = ax' * bx' + ay' * by' in *)
    (atan2 by' bx') -. (atan2 ay' ax') *. 180.0 /. pi
      
let simplify_path centroids =
  let rec helper acc (oa,ox,oy)  = function
      [] -> (oa,ox,oy) :: acc
    | (a,x,y)::l ->
        let (ooa,oox,ooy) = List.hd acc in
          (* if the angle between oox,ooy and x,y around vertex ox,oy
             is 180 +- 10, we simplify *)
        let angle = angle_between (float_of_int oox, float_of_int ooy) 
          (float_of_int ox,float_of_int oy) (float_of_int x,float_of_int y) in
          (* prerr_endline (string_of_float angle); *)
          if (10.0 > angle) then
            helper acc (a,x,y) l (* simplify ! *)
          else
            helper ((oa,ox,oy)::acc) (a,x,y) l
  in
    match centroids with
        [] -> []
      | x::[] -> centroids
      | x::y::l -> helper [x] y l
;;


  
let centroid_path_to_string centroids =
  let len = List.length centroids in
  let b = Buffer.create (len * 10) in
    List.iter (fun (age,x,y) ->
                 Buffer.add_string b (string_of_int x);
                 Buffer.add_char b ' ';
                 Buffer.add_string b (string_of_int y);
                 Buffer.add_char b ' ';
              ) centroids;
    Buffer.contents b
;;

let veve_webcam () =
  Random.self_init ();
  let id = Random.int 100000 in
  let sid = string_of_int id in
  let width = 324 in
  let height = 248 in
  let keyname = "324x248_rgb_int_webcam" in
  let okeyname = "324x248_rgb_int_vevecam" in

  let (sock,fdi,fdo) = Cmlib.cm_connect "127.0.0.1" 9911 in
  let (vsock,vfdi,vfdo) = Cmlib.cm_connect "127.0.0.1" 9911 in

  let frame = ref 0 in
  let age_combine = age_combine width height in
    
  let veve_to_file = veve_to_file width height in
  let veve_to_bmp_file = veve_to_bmp_file width height in
  let veve_to_rgbintarr = veve_to_rgbintarr width height in
    prerr_endline "Prepare require!";
    Cmlib.cm_interactiverequire fdo keyname;
    prerr_endline "Prepare provide!";
    Cmlib.cm_provide vfdo okeyname;
    let rec loop age_veve framei =
      prerr_endline (string_of_int framei);
      (* prerr_endline "Read buffer"; *)
      let (buffer:int array) = Cmlib.cm_interactive_read_buffer_last fdo fdi in
        (* prerr_endline "Flatten array"; *)
        let bmp = Captchas.flatarray_to_bmp ~f:rgb width height buffer in
          (* prerr_endline "Shrink"; *)
          let shrunk = Shrinker.shrink bmp in
            (* prerr_endline "Clean"; *)
            let shrunk = cleanup shrunk in
              (* prerr_endline "Combine"; *)
              let new_age_veve = age_combine age_veve shrunk in
                assert ((Array.length new_age_veve) = (Array.length age_veve));
                (* let fname = Format.sprintf "segments/veve.%s.%06d.png" sid framei in
                   prerr_endline ("To file: "^fname);
                   veve_to_bmp_file new_age_veve fname; *)
        
        (* prerr_endline ("To "^okeyname); *)
        Cmlib.cm_send_buffer vfdo okeyname (veve_to_rgbintarr new_age_veve);

        let vveve = vectorify_veve width height new_age_veve in
        let svveve = simplify_path vveve in
        let str = centroid_path_to_string svveve in
          print_endline str;
        loop new_age_veve (framei + 1)
    in
      prerr_endline "Run loop!";
      loop (make_veve width height) 0 
;;
        
