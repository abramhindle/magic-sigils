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
let kmeans ?epsilon:(epsilon=1.0) ?iters:(iters=60) k xrange yrange pntlist =
  let arr = Array.create k (0.,0.) in
  let (xs,xe) = xrange in
  let xr = xe -. xs in
  let (ys,ye) = yrange in
  let yr = ye -. ys in
  let rand_coord () = 
    (((Random.float xr) +. xs),
     ((Random.float yr) +. ys))
  in
  for i = 0 to (k - 1) do
    arr.(i) <- rand_coord ();
  done;
    let dist (x,y) (x2,y2) =
      let x' = x -. x2  in
      let y' = y -. y2 in
	x'*.x' +. y'*.y'
    in
    let find_closest_centroid arr (x,y) =
      let m  = ref (dist arr.(0) (x,y)) in
      let mi = ref 0 in
	for i = 1 to (k-1) do
	  let d = dist arr.(i) (x,y) in
	    if (d < !m) then
	      begin
		m := d;
		mi := i;
	      end;
	done;
	!mi
    in
    let pntlist = List.map (fun (x,y) -> ((find_closest_centroid arr (x,y)),(x,y))) pntlist in
    let split pntlist =
      let arr = Array.create k [] in
	List.iter (fun (c,(x,y)) ->
		     arr.(c) <- (x,y)::(arr.(c))
		  ) pntlist;
	Array.to_list arr
    in
    let tolerance arr1 arr2 =
      let t = ref 0. in
	for i = 0 to (k-1) do
	  t := !t +. (dist arr1.(i) arr2.(i))
	done;
	let avg = !t /. (float_of_int k) in
	  (avg < epsilon)
    in
    let rec kmeans n oldarr pntlist =
      let arr = Array.create k (0.,0.) in
      let counts = Array.create k 0 in
	List.iter (fun (c,(x,y)) ->
		   counts.(c) <- counts.(c) + 1 ;
		     let (x',y') = arr.(c) in
		       arr.(c) <- (x' +. x,y' +. y);
		  ) pntlist;
	for i = 0 to (k-1) do
	  let (x,y) = arr.(i) in
	    if (counts.(i) = 0) then
	      ()
	    else
	      arr.(i) <- 	    (x /. (float_of_int counts.(i)),	     y /. (float_of_int counts.(i)))
	done;

	let pntlist = List.map (fun (c,(x,y)) -> ((find_closest_centroid arr (x,y)),(x,y))) pntlist in

	  for i = 0 to (k - 1) do
	    let (x,y) = arr.(i) in
	      print_endline (String.concat "\t" [(string_of_int i) ; (string_of_float x) ; (string_of_float y)])
	  done;
	  if (tolerance oldarr arr || n > iters) then 
	    split pntlist
	  else
	    kmeans (n+1) arr pntlist
    in
      kmeans 0 arr pntlist
;;

