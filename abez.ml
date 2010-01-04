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
module Abez = 
struct

  let for_fold f max dfl =
    let rec ffold last = function
	x when x >= max -> last
      | x -> 
	  let last = f x last in
	    ffold last (x+1)
    in
      ffold dfl 0



  let fold_each_xy f width height dfl =
    for_fold (fun y yl ->
		for_fold (fun x xl ->
			    f x y xl  
			 ) width yl
	     ) height dfl


  let iti width (x,y) = let (x',y') = (x+1,y) in
    if (x' >= width) then
      (0,(y'+1))
    else
      (x',y')

  let for_xy width height f =
    let rec iterate = function
	(x,y) when y >= height -> ()
      | (x,y) -> 
	  f x y;
	  iterate (iti width (x,y))
    in      iterate (0,0)

  let for_map_xy width height f =
    let rec iterate = function
	(x,y) when y >= height -> []
      | (x,y) -> 
	  (f x y) :: ( iterate (iti width (x,y)) )
    in
      iterate (0,0)

  let fori f max =
    for i = 0 to (max - 1) do
      f i;
    done
    

  let get_extension s =
    let dotpos = String.rindex s '.' in
      String.sub s (dotpos + 1) (String.length s - dotpos - 1) 


  let get_basename s =
    let dotpos = String.rindex s '/' in
      String.sub s (dotpos + 1) (String.length s - dotpos - 1) 

  let for_map f max = 
    let rec foreach = function
	x when x >= max -> []
      | x -> (f x) :: (foreach (x+1))
    in
      foreach 0

  let join l = 
    String.concat " " l

  let iteri f l =
    let rec itt i = function
	[] -> ()
      | x::xs -> 
	  let _ = f i x in
	    itt (i+1) xs
    in
      itt 0 l

  let rec shortcircuit f = function
      [] -> false
    | x::xs -> if (f x) then
	true
      else
	shortcircuit f xs

  let shortcircuit_c l =
    shortcircuit (fun x -> x) l

  let sum_f l =
    List.fold_left (fun x o ->  x +. o) 0. l

  let mapi f l =
    let rec r n = function 
	[] -> []
      | x::xs -> 
	  (f n x)::(r (n+1) xs)
    in
      r 0 l

  let add_array a b =
    let l = Array.length a in
      for i = 0 to (l-1) do
	a.(i) <- a.(i) +. b.(i);
      done;


end;;

