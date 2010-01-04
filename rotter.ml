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
(* This module tries to normalize bitmaps *)

open Contour;;
open Pca;;
open Abez;;
let status x = ();;
(*   prerr_endline ("[Rotter] "^x)
;; *)
let statusi n x = status (n ^" "^(string_of_int x)) ;;
let fi = float_of_int ;;
let rotter bmp =
  status "Contour";
  let contour = Contour.contour bmp in
  let n = List.length contour in
  status "Create matrix";
  let matrix = Pca.create_matrix n 2 in
    Abez.iteri (fun i (x,y) ->
		  Pca.matrix_set matrix i 0 (fi x);
		  Pca.matrix_set matrix i 1 (fi y);
	       ) contour;
  status "PCA";
    let (finaldata,eigenvalues,eigenvectors,meanmatrix) = Pca.pca matrix in
  status "Dims";
    let (x,y) = Pca.dims finaldata in
      statusi "rows" x;
      statusi "cols" y;
      Abez.for_map (fun i ->
		 let x = Pca.matrix_get finaldata 0 i in
		 let y = Pca.matrix_get finaldata 1 i in
		   (x,y)
	      ) y
;;

let print_rotter_to_file f l =
  let fd = open_out f in
    List.iter (fun (x,y) ->
		 output_string fd ((string_of_float x) ^ " " ^ (string_of_float y) ^ "\n")
	      )
      l;
      close_out fd;
;;
