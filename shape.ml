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
open Abez;;
open Munkres;;

exception HistogramException;;
exception HistogramCompareException;;
exception AngleException;;
exception NoAveragePt;;
exception HistogramIndexException;;
exception SampleException;;

type point = Point of float * float ;;

let mk_pt x y = 
  Point(x,y)
;;

let mk_pt_t (x,y) =
  Point(x,y)
;;

let mk_pti x y =
  mk_pt (float_of_int x) (float_of_int y)
;;

let xy (Point(x,y)) = (x,y) ;;

let string_of_point p = 
  let (x,y) = xy p in
    "Point("^(string_of_float x)^","^(string_of_float y)^")"
;;

(* p1 - p2 *) 
let pnt_sub pnt1 pnt2 =
  let (x1,y1) = xy pnt1 in
  let (x2,y2) = xy pnt2 in
  mk_pt (x1 -. x2) (y1 -. y2) 
;;

let pnt_eq (Point(a,b)) (Point(c,d)) =
  a = c && b = d
;;
let fabs y = if (y >= 0.) then y else (-1.0) *. y ;;
let feq a b = fabs ( a -. b ) < 0.00000001 ;;
let pnt_feq (Point(a,b)) (Point(c,d)) =
  let f = feq in
  (f a c) && (f b d)
;;


let pi = 4. *. atan(1.) ;;
let twopi = 8. *. atan(1.) ;;
let halfpi = 2. *. atan(1.) ;;
let quarterpi = atan(1.) ;;


let calcangle x y length = 
  let angle = acos(fabs(y) /. length ) in

  if (x >= 0. && y >= 0.) then 
    halfpi -. angle
  else if (x <= 0. && y >= 0.) then
    angle +. halfpi
  else if  (x <= 0. && y <= 0.) then
    (halfpi -. angle) +. pi
  else if (x >= 0. && y <= 0.) then
    ( angle +. pi +. halfpi )
  else
    raise AngleException 
;;

let calcangle2 x y =
  let hyp = sqrt (x*.x +. y *. y) in
    calcangle x y hyp
;;

let calcangle_test () =
  let input = [ (1.0,0.0) ; (1.0,0.1) ; (1.0,1.0) ;
                (0.1,1.0) ; (0.0,1.0) ; ((-0.1),1.0);
                ((-1.0),1.0); ((-1.0),(0.1)); ((-1.0),0.0) ;
                ((-1.0),(-0.1)); ((-1.0),(-1.0)); ((-0.1),(-1.0)) ;
                ((0.1),(-1.0)); ((1.0),(-1.0)); ((1.0),(-0.1)); (1.0,0.0) ] in
(* [0.; 0.0996686524911620797; 0.785398163397448168; 1.47112767430373581;
    1.57079632679489656; 1.67046497928605731; 2.35619449019234484;
    3.04192400109863126; 3.14159265358979312; 3.24126130608095497;
    3.92699081698724139; 4.6127203278935287; 4.81205763287585064;
   5.49778714378213795; 6.18351665468842437; 0.] *)

    List.map (fun (x,y) -> calcangle2 x y) input
;;
  

let div_pt (Point(x,y)) (Point(u,v)) =
  mk_pt (x /. u) (y /. v) 
;;

let average_pt pts =
  let n = List.length pts in
  div_pt (List.fold_left (
      fun n o ->
	let (x,y) = xy n in
	let (lx,ly) = xy o in
	  mk_pt (x +. lx) (y +. ly)
    ) (mk_pt 0. 0.) pts) (mk_pti n n)
;;

let pnt_dist (Point(x1,y1)) (Point(x2,y2)) =
  let d1 = x1 -. x2 in
  let d2 = y1 -. y2 in
  d1 *. d1 +. d2 *. d2 
;;

let most_average_pt pts =
  let avg_pt = average_pt pts in
  match pts with
      [] -> raise NoAveragePt 
    | x::[] -> x
    | x::xs ->
	List.fold_left (
	  fun n p ->
	    let (x,y) = xy n in
	    let d' = pnt_dist n avg_pt in
	    let d  = pnt_dist p avg_pt in
	      if   (d' < d) 	then
		n
	      else 
		p
	) x xs 
	
;;
let log_of_2 = log 2.0 ;;
let log_2 x = (log x) /. log_of_2 ;;
(* let log_histogram angles llength pnt pts =
  let fangles = float_of_int angles in
  let fllength = float_of_int llength in
  let anglechunk = twopi /. fangles in
  let pts = List.map (fun x -> pnt_sub x pnt) pts in
  let histogram = Array.create_matrix llength angles 0. in
  let _ = List.iter (
    fun o  ->
      let (x,y) = xy o in
      let length = ( log_2 (x*.x +. y*.y) ) in
      let angle = calcangle x y length in
      let ilength = int_of_float length in
      let iangle = int_of_float ( angle /. anglechunk ) in
      let ilength = if (ilength >= llength) then (llength -1) else ilength in
	histogram.(ilength).(iangle) <- (histogram.(ilength).(iangle)) +. 1.;
  ) pts in
  let cnt = List.length pts in
  let fcnt = float_of_int cnt in
  for y = 0 to (llength-1) do
    for x = 0 to (angles - 1 ) do
      histogram.(y).(x) <- histogram.(y).(x) /. fcnt;
    done
  done;.
  (llength,angles,histogram)
;;  
*)
type histogram = { npts: int ; angles: int ; llength: int ; histogram: float array array };;

let biglog_histogram angles llength cnt pts =
  let biglen = angles * llength in
  let fangles = float_of_int angles in
  let fllength = float_of_int llength in
  let anglechunk = twopi /. fangles in
    (*  let pts = List.map (fun x -> pnt_sub x pnt) pts in *)
  let ipts = cnt in
  let histogram = Array.create_matrix ipts biglen 0. in
  let l_a_index l a = a + angles * l in
  let inc_hist_pt ipt length angle =
    let index = (l_a_index length angle) in
      (* START ERROR DETECTION FOR DEBUGGING *)
      if index >= biglen then
	begin
	  print_string ("index >= biglen "^(string_of_int index)^"\n");
	  raise HistogramIndexException;
	end;

      if ipt >= ipts then 
	begin
	  print_string ("Ipt >= Ipts "^(string_of_int ipt)^"\n");
	  raise HistogramIndexException;
	end;
      (* END ERROR DETECTION FOR DEBUGGING *)
      histogram.(ipt).(index) <- histogram.(ipt).(index) +. 1.;
  in
    Abez.iteri (
      fun i pnt ->
	List.iter (
	  fun o  ->
	    let (x,y) = xy (pnt_sub o pnt) in
	    let length = ( log_2 (x*.x +. y*.y) ) in
	    let angle = calcangle x y length in
	    let ilength = int_of_float length in
	    let iangle = int_of_float ( angle /. anglechunk ) in
	    let ilength = if (ilength >= llength) then (llength -1) else ilength in
              
	      inc_hist_pt i ilength iangle;
	) pts;
    ) pts;
(*     print_string "Inc\n"; *)
    let cnt = List.length pts in
    let fcnt = float_of_int cnt in
      (* should this be divided by the sum? *)
      for y = 0 to (ipts-1) do
	for x = 0 to (biglen - 1 ) do
	  histogram.(y).(x) <- histogram.(y).(x) /. fcnt;
	done;
      done;
(*      let arr_cmp l a1 a2 =
	let rec icmp i =
	  if (i >= l) then 0
	  else 
	      let c = compare a1.(i) a2.(i) in
		if ( c = 0 ) then
		  icmp (i+1)
		else
		  c
	in
	  icmp 0
      in

	  Array.sort (fun x y -> arr_cmp biglen x y) histogram; *)

	  let o = { npts = ipts ; angles = angles ; llength = llength ; histogram = histogram } in
	    o
;;  


let hist_size hist =
  (hist.npts , hist.angles * hist.llength )
;;

let hist_attrs hist =
  let (l,a) = hist_size hist in
    (l,a,hist.histogram)
;;

let bighistocompare hist1 hist2 =
  let (l,a,h1) =   hist_attrs hist1 in
  let (l',a',h2) = hist_attrs hist2 in
    if (not(l' = l && a = a')) then
      begin
	print_int l; print_string " ";
	print_int l'; print_string " ";
	print_int a; print_string " ";
	print_int a'; print_string "\n";
	raise HistogramCompareException;
      end;
  let res = Abez.fold_each_xy (
    fun x y o ->
      let hi = h1.(y).(x) in
      let hj = h2.(y).(x) in
      let num = hi -. hj in
      let num = num *. num in
      let denom = hi +. hj in
      let e = 0.0000001 in
	if (denom < e && num > e) then
	  o +. 1.0
	else if (denom < e) then
	  o
	else
	  o +. num /. denom (* where is o what are we calculating? *)
  )  a l 0. in
    
  let res = 0.5 *. res /. (float_of_int l) in
    res
;;

let lapjv_bighistocompare hist1 hist2 =
  let (l,a,h1) =   hist_attrs hist1 in
  let (l',a',h2) = hist_attrs hist2 in
    if (not(l' = l && a = a')) then
      begin
	print_int l; print_string " ";
	print_int l'; print_string " ";
	print_int a; print_string " ";
	print_int a'; print_string "\n";
	raise HistogramCompareException;
      end;
  let res = Abez.fold_each_xy (
    fun x y o ->
      let hi = h1.(y).(x) in
      let hj = h2.(y).(x) in
      let num = hi -. hj in
      let num = num *. num in
      let denom = hi +. hj in
      let e = 0.0000001 in
	if (denom < e && num > e) then
	  o +. 1.0
	else if (denom < e) then
	  o
	else
	  o +. num /. denom (* where is o what are we calculating? *)
  )  a l 0. in
    
  let res = 0.5 *. res /. (float_of_int l) in
    res
;;



(*  
let histocompare hist1 hist2 =
  let (l,a,h1) = hist1 in
  let (l',a',h2) = hist2 in
    if (not(l' = l && a = a')) then
      begin
	print_int l; print_string " ";
	print_int l'; print_string " ";
	print_int a; print_string " ";
	print_int a'; print_string "\n";
	raise HistogramCompareException;
      end;
  let res = Abez.fold_each_xy (
    fun x y o ->
      let hi = h1.(y).(x) in
      let hj = h2.(y).(x) in
      let num = hi -. hj in
      let num = num *. num in
      let denom = hi +. hj in
      let e = 0.0000001 in
	if (denom < e && num > e) then
	  o +. 1.0
	else if (denom < e) then
	  o
	else
	  o +. num /. denom (* where is o what are we calculating? *)
  )  a l 0. in
  let res = 0.5 *. res in
    res
;;

let rotate_histogram histogram =
  let (l,a,hist) = histogram in
    
    for y = 0 to (l - 1) do
      let tmp = hist.(y).(0) in
	for x = 0 to (a - 2) do
	  hist.(y).(x) <- (hist.(y).(x+1));
	done;
	hist.(y).(a-1) <- tmp;
     done;
	
;;
*)
let num_max = function
    [] -> raise HistogramException
  | x::xs ->
      let r = List.fold_left (fun n o ->
	       if (n > o || o <> o) then n else o
			     ) x xs in
	r
;;
let num_min = function
    [] -> raise HistogramException
  | x::xs ->
      let r = List.fold_left (fun n o ->
	       if (n < o || o <> o) then n else o
			     ) x xs in
	r
;;


let tuple_max f = function
    [] -> raise HistogramException
  | x::xs ->
  List.fold_left (
    fun n o ->
      let fn = f n in
      let fo = f o in
	       if (fn > fo || fo <> fo) then n else o
	    ) x xs
;;

let tuple_min f = function
    [] -> raise HistogramException
  | x::xs ->
  List.fold_left (
    fun n o ->
      let fn = f n in
      let fo = f o in
	       if (fn < fo || fo <> fo) then n else o
	    ) x xs
;;

(*
let hist_length_angle (a,b,h) = (a,b);; *)
let bhist_length_angle hist = (hist.llength,hist.angles);;
(*
let compare_pts_to_hist pts histogram =
  let (llength,angles) = hist_length_angle histogram in
  let l = List.map ( (* for each pt *)
    fun pnt ->
      let hist = log_histogram angles llength pnt pts in
	(* ROTATION *)
       let scores = num_min (Abez.for_map 
	 (      fun x ->
				rotate_histogram hist;
				histocompare hist histogram	      
			    ) angles
			   ) in 

(*      let scores = histocompare hist histogram in *)
							  

(*      let _ = print_string ((string_of_float scores) ^ " \n") in *)
	scores
  ) pts in
(*    num_max l *)
    num_min l
;;
*)

let compare_pts_to_bighist hist histogram =
  let score = bighistocompare hist histogram in
    score
;;
(*
let find_closest pts histograms =
  let l = List.map (
    fun (sym,hist,filename,_,_) ->
(*      print_string (sym^"\n"); *)
      let score = compare_pts_to_hist pts hist in
(*	print_string ((string_of_float score)^sym^"\n"); *)
	(score,sym,hist,filename)
  ) histograms
  in
  let l2 = List.sort (fun (x,_,_,_) (y,_,_,_) -> compare x y) l in
    List.iter (fun (x,s,_,f) ->
		 print_string s;
		 print_string " ";
		 print_float x;
		 print_string " ";
		 print_string f;
		 print_string "\n";
	      ) l2;
  let (score,sym,_,_,_) = 
 (*   tuple_max *)
    tuple_min 
      (fun (v,_,_,_) -> v)
      (l ) in
    (score,sym)
;;
*)
let bfind_closest histogram histograms =
  let l = List.map (
    fun (sym,hist,filename) ->
      let score = compare_pts_to_bighist histogram hist in
	(score,sym,hist,filename)
  ) histograms
  in
  let l2 = List.sort (fun (x,_,_,_) (y,_,_,_) -> compare x y) l in
    List.iter (fun (x,s,_,f) ->
		 print_string s;
		 print_string " ";
		 print_float x;
		 print_string " ";
		 print_string f;
		 print_string "\n";
	      ) l2;
  let (score,sym,_,_) = 
    tuple_min 
      (fun (v,_,_,_) -> v)
      (l ) in
    (score,sym)
;;



let sample_n n pts = 
  let arr = Array.of_list pts in
  let l = Array.length arr in
    Abez.for_map (fun x ->
		    arr.(Random.int l)
		 ) n
;;
let list_minus l1 l2 =
  List.filter (fun x -> not (List.mem x l2)) l1
;;

	
let match_points hist1 hist2  =
  assert (hist1.npts = hist2.npts);
  assert (hist1.angles = hist2.angles);
  assert (hist1.llength = hist2.llength);
  let len = hist1.llength * hist1.angles in
  let n = hist1.npts in
  let b = max_float in
  let matrix = Array.make_matrix n n b in
  let distance arr1 arr2 =
    let res = Abez.for_fold (
      fun x o ->
	let hi = arr1.(x) in
	let hj = arr2.(x) in
	let num = hi -. hj in
	let num = num *. num in
(*	let denom = hi +. hj in
	let e = 0.0000001 in
	  if (denom < e && num > e) then
	    o +. 1.0
	  else if (denom < e) then
	    o
	  else
	    o +. num /. denom (* where is o what are we calculating? *) *)
	o +. num
    )  len 0. in
    let res = 0.5 *. res in
      res
  in

    Abez.for_xy n n (fun x y ->
		  if (x > y) then
		    ()
		  else
		    let h1 = hist1.histogram.(x) in
		    let h2 = hist2.histogram.(y) in
		    let dist = distance h1 h2 in
		      matrix.(x).(y) <- dist;
		      matrix.(y).(x) <- dist;
	       );
    (* matrix is filled with distances *)
(*    let nf = float_of_int n in
    let nlist = Abez.for_map (fun x -> x) n in
    let v i x = matrix.(i).(x) in
    let sum_path path =
      Abez.sum_f (Abez.mapi v path)
    in
    let init_sum = sum_path nlist in
*)
      (*
    let rec best_path path total (btotal,bpath) i =
      let possible = list_minus nlist path in
	match possible with
	    [] -> 
	      if (total < btotal) then
		(total,path)
	      else
		(btotal,bpath)
	  | x::xs ->
	      (* for each find the min path *)
	      List.fold_left ( 
		fun (bt,bp) x ->
		  let t = matrix.(i).(x) in
		  let total' = total +. t in
		    if (total' < bt) then
		      best_path (x::path) total' (btotal,bpath) (i+1)
		    else
		      (bt,bp)

	      ) (btotal,bpath)  (x::xs)
    in
    let out =  List.map (fun x -> best_path [x] (v x 0) (init_sum,nlist) 0) nlist in

    let (total,path) = List.fold_left (fun (total,path) (ototal,opath) ->
					 if (total < ototal) then
					   (total,path)
					 else
					   (ototal,opath)
				      ) (init_sum,nlist) out in
      total
      *)

(*
    let quick_min_path () =
      let arr = Array.create n true in
      let findmin k =
	Abez.for_fold (
	  fun x o ->
	    if (arr.(x) && (v k x) < (v k o)) then x else o
	) n 0
      in
	Abez.for_fold (
	  fun x o ->
	    let a = findmin x in
	      arr.(a) <- false;
	      o +. (v x a)
	) n 0.
    in
	quick_min_path () *)
(*    for y = 0 to (n-1) do
      for x = 0 to (n - 1) do
	matrix.(y).(x) <- 1000. *. matrix.(y).(x);
	print_float matrix.(y).(x);
	print_string " ";
      done;
      print_endline "";
    done;
*)
(*
    for i = 0 to (n-1) do
      for j = 0 to (n-1) do
	matrix.(i).(j) <- 100.*.(1.0 -. Random.float 0.001) *. matrix.(i).(j);
      done;
    done;
	*)  
   (* Lapjv.lapjv n matrix   *)
   (* Lapjv.abez_permuter n matrix  *)
      (* Lapjv.min_cost_assignment n matrix *)
      Munkres.munkres_permutation n matrix
;;

let output_match filename h1pts pts x =
  let fd = open_out filename in
  let sf = string_of_float in
    Abez.iteri (
      fun i pnt ->
	let y = x.(i) in
	let yv = List.nth pts y in
	let (x1,y1) = xy pnt in
	let (x2,y2) = xy yv  in
	  output_string fd ((sf x1) ^ " " ^ (sf y1 ) ^ " 0.0\n" ^ (sf x2 ) ^ " " ^(sf y2) ^ " 1.0\n\n\n");
    ) h1pts;
    close_out fd
;;
		
let find_closest h1pts histogram histograms =

  let l = List.map (
    fun (sym,hist,filename,pts,bmp) ->
      print_endline sym;
      let (score,m,_) = match_points histogram hist in
	output_match ("./matches/"^(string_of_float score)^"_"^sym) h1pts pts m;
	print_float score;
	print_newline ();
	(score,sym,hist,filename)
  ) histograms
  in
  let l2 = List.sort (fun (x,_,_,_) (y,_,_,_) -> compare x y) l in
    List.iter (fun (x,s,_,f) ->
		 print_string s;
		 print_string " ";
		 print_float x;
		 print_string " ";
		 print_string f;
		 print_string "\n";
	      ) l2;
  let (score,sym,_,_) = 
    tuple_min 
      (fun (v,_,_,_) -> v)
      (l ) in
    (score,sym)
;;

let dump_matrix_to_file file arr =
  let fd = open_out file in
  let w = Array.length arr in
  let h = Array.length arr.(0) in
    for x = 0 to (w - 1) do
      for y = 0 to (h - 1) do
        output_string fd (string_of_float arr.(x).(y));
        output_string fd "\t";
      done;
     output_string fd "\n";
    done;
    close_out fd;

;;

let dump_integer_matrix_to_file file arr =
  let fd = open_out file in
  let w = Array.length arr in
  let h = Array.length arr.(0) in
    for x = 0 to (w - 1) do
      for y = 0 to (h - 1) do
        output_string fd (string_of_int arr.(x).(y));
        output_string fd "\t";
      done;
     output_string fd "\n";
    done;
    close_out fd;

;;


let hist_acc hist bins angle bin = hist.(angle * bins + bin) ;;
let flat_hist_to_matrix hist angles bins =
  let m = Array.create_matrix angles bins 0. in
    for i = 0 to angles - 1 do
      for j = 0 to bins - 1 do
        m.(i).(j) <- hist_acc hist bins i j 
      done;
    done;
    m
;;

let pnt_hist 
    ?maxdistance:(maxdist=225.)
    ~angles:angles
    ~bins:bins
    ~points:pts 
    point
    =
  (* potentially unnecessary step? *)
  (* sort them by some normal order *)
  (* let pts = List.sort (fun p1 p2 -> let (x,y) = xy p1 in let (x',y') = xy p2 in compare (x*.x +. y*.y) (x'*.x' +. y'*.y')) pts in *)
  let fbins = float_of_int bins in
  let fangles = float_of_int angles in
  let calc_len x = ( log_2 (1.0 +. (min maxdist x))) in
  let max_len = calc_len maxdist in
  let biglen = angles * bins in
  let m = Array.create biglen 0 in
  let minc iangle ilength =
    m.(iangle * bins + ilength) <- 
      m.(iangle * bins + ilength) + 1
  in

    List.iter (fun pt ->
                 let (x,y) = xy (pnt_sub pt point) in
                   (* prerr_endline ((string_of_float x) ^ " , " ^ (string_of_float y)); *)
                 if (not (x = 0. && y = 0.)) then
                   begin
                     let hyp = sqrt(x *. x +. y *. y) in
                     let length = calc_len hyp in
                     let ilength = int_of_float ((fbins -. 1.0) *. length /. max_len) in
                     let angle = calcangle x y hyp in
                     let iangle = int_of_float ((fangles -. 1.0) *. (angle /. twopi)) in
                       minc iangle ilength
                   end
              ) pts;
    m
;;


let abez_histogram angles llength cnt pts =
  List.map 
    (fun pt -> pnt_hist ~angles:angles ~bins:llength ~points:pts pt)
    pts
;;    
let real_histogram angles llength cnt pts =
  let hists = abez_histogram angles llength cnt pts in
  let hist = Array.make_matrix cnt (angles * llength) 0. in
  Abez.iteri (fun i hist' ->
                for j = 0 to (angles * llength) - 1 do
                  hist.(i).(j) <- float_of_int hist'.(j)
                done;
             ) hists;
    let o = { npts = cnt ; angles = angles ; llength = llength ; histogram = hist } in
      o
;;
let print_array p arr =
  let l = Array.length arr in
    for i = 0 to (l - 1) do
      p arr.(i);
      print_string "\t";
    done;
    print_endline "";
;;

let line_array_to_file filename arr =
  let fd = open_out filename in
  let len = Array.length arr in
    for i = 0 to (len - 1) do
      let ((a,b),(c,d)) = arr.(i) in
        output_string fd ((string_of_float a) ^"\t"^(string_of_float b)^"\n"^
          (string_of_float c) ^ "\t"^(string_of_float d)^"\n\n\n");
    done;
    close_out fd;
;;

