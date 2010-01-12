(*
 *  Copyright (c) 2005, 2006, 2007, 2009 Abram Hindle
 *  
 *  This file has some code dealing with point sets
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

