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
open Shape;;
open Captchas;;
open Abez;;

(* use classic segmenter and a sub segmenter? *)
(* debug segmenter ;_; *)
let diag = ref false in
let files = ref [] in
Arg.parse [("-d",(Arg.Set(diag)),"Diagonal fill?")] (fun s -> files := s :: !files) "edge files"; 
let files = List.rev !files in
(* let files = ["./fill_segmenter.png"] in (* @ List.rev !files in *) *)
List.iter (fun file ->		    
	     let outfile = "segments/" ^ (get_basename file) ^ ".gray.jpg" in
	     let rgb = Captchas.load_rgb_file file in
	     let segments = Captchas.fill_segmenter ~diag:(!diag) ~istext:is_black rgb in
	     let ext = Captchas.get_extension file in
	     let body = String.sub file 0   (String.length file - String.length ext - 1) in
	     let body = (get_basename body) in
	       Captchas.iteri (fun i (b,_) ->
			let outfile = "segments/" ^ body ^ "." ^ (string_of_int i)  ^ ".segment.jpg" in
			  pj [S(outfile)];
			  b#save outfile (Some Jpeg) [];
		) segments ;

	  ) files
;;

