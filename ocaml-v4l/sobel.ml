(*
Sobel Edge Detection
Copyright (C) 2005 Abram Hindle

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

*)

let rgb x = 
	let r = (0x00FF0000 land x) lsr 16 in
	let g = (0x0000FF00 land x) lsr 8 in
	let b = (0x000000FF land x) in
	(r,g,b)

;;


let sobel_edge_detect w h outbuff inbuff =
	let sobelgx = [| [| -1 ; 0 ; 1 |] ; [| -2 ; 0 ; 2 |] ; [|-1 ; 0 ; 1 |] |] in
	let sobelgy = [| [|  1 ; 2 ; 1 |] ; [| 0 ; 0 ; 0 |] ; [|-1 ; -2 ; -1 |] |] in
	let c x y = w * y + x in
	let avg (a,b,c) = (a+b+c)/3 in
	let mix x y = avg (rgb (inbuff.(c x y))) in
	let inner x y row = 
		let m1 = mix (x-1) y in
		let m2 = mix (x) y in
		let m3 = mix (x+1) y in
		(row.(0)) * m1 + (row.(1)) * m2 + (row.(2)) * m3
 	in
	let bound = function
		x when x > 255 -> 255
               | x when x < 0 -> 0
               | x -> x
        in
	let sumx x y arr =
		bound (
			(inner x (y-1) arr.(0)) + 
			(inner x (y)   arr.(1)) +
			(inner x (y+1) arr.(2))
		)
	in
	let get_sum x y =
		if (y = 0 || y = (h-1)) || (x = 0 || x = (w-1)) then 
			0
		else
			(sumx x y sobelgx) + (sumx x y sobelgy)
	in
	for y = 0 to (h-1) do
		for x = 0 to (w-1) do
			let sum = get_sum x y in
			outbuff.(c x y) <- sum ;
		done;
	done;
;;
