(*
Copyright (c) 2005, ABRAM HINDLE
All rights reserved.

 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 * <abez@abez.ca>
*)

let stdo = stdout;
open Grabcaml;;
open Unix;;
open Sobel;;
let width = 324 ;;
let height = 248 ;;
let twidth = 75 ;;
let theight = 20;;
let size = width*height;;
let buffer = Array.create  size 0;;
let buffer2 = Array.create size 0;;
let chars = "@8Oo:. ";;
let nchars = String.length chars;;

Grabcaml.init ();;

let cam = Grabcaml.open_camera "/dev/video0" width height;;
Grabcaml.print cam ;;
Grabcaml.set_contrast cam (32000);;
Grabcaml.set_brightness cam (32000);;  
Grabcaml.set_hue cam (32000);;
Grabcaml.set_colour cam (32000);;  
for i = 0 to 100 do
	let contrast = Grabcaml.get_contrast cam in
	let brightness = Grabcaml.get_brightness cam in
	let colour = Grabcaml.get_colour cam in
	let hue = Grabcaml.get_hue cam in
	List.iter (fun x -> print_int x; print_string "\t";) [contrast;brightness;colour;hue];
	print_string "\n"; 
	if (Grabcaml.mmap_read cam buffer) then
		begin
			Grabcaml.set_contrast cam (contrast+256) ;
			Grabcaml.set_brightness cam (brightness+256) ;
			(* sobel_edge_detect width height buffer2 buffer; *)
			let buffer2 = buffer in
			(*
			for y = 0 to theight do
				let yy = (height-1) - (height-1)*y/theight in
				for x = 0 to twidth do
					let xx = (width-1)*x/twidth in
					let (r,g,b) = Grabcaml.rgb (buffer2.(yy*width + xx)) in
					let index = (nchars-1)*r/255 in
					print_char (String.get chars index);
				done;
				print_string "\n";
			done;
			print_string "\n";
			flush stdo; *)
			let out = open_out  ("frame"^(string_of_int i)^".ppm") in
			output_string out "P6\n";
			output_string out ((string_of_int width)^" "^(string_of_int height)^"\n");
			output_string out "255\n";
			for i = 0 to (size-1) do
				let (r1,g1,b1) = Grabcaml.rgb buffer2.(i) in
				let (r2,g2,b2) = Grabcaml.rgb buffer.(i) in
				let r = ( b1 + r2 ) / 2 in
				let g = ( b1 + g2 ) / 2 in
				let b = ( b1 + b2 ) / 2 in
				(*output_byte out buffer2.(i);		*)
				output_byte out r;
				output_byte out g;
				output_byte out b;
			done;
			close_out out;
		end
done;;
