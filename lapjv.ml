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
type lapjv_struct = {
  mutable f: int;
  mutable h: float;
  mutable i: int;
  mutable k: int;
  mutable j: int;
  mutable f0: int;
  mutable i1: int;
  mutable j1: int;
  mutable j2: int; 
  mutable u1: float;
  mutable u2: float;
  mutable min: float;
  mutable imin: int;
  mutable last: int;
  mutable low: int;
  mutable up: int;
  mutable first: bool;
  mutable res: float;
    x : int array ;     (* cols -> row *)
    y : int array ;     (* rows -> cols *)
    matches : int array;
    u : float array ;     (* dual row vars *)
    v : float array ;     (* dual column vars *)
    col : int array ;   (* array of cols *)
    d : float array ;     (* shortest path lengths *)
    free : int array ;  (* unassigned rows *)
    pred : int array ;  (* pred-array - shortest path tree *)
};;

let create_lapjv n =
  let q = {
    f = 0;
    h = 0.;
    i = 0;
    k = 0;
    j = 0;
    f0 = 0;
    i1 = 0;
    j1 = 0;
    j2 = 0; 
    u1 = 0.;
    u2 = 0.;
    min = 0.;
    imin = 0;
    last = 0;
    low = 0;
    up = 0;
    first = true;
    res = 0.;
    x = Array.create n 0;     (* cols -> row *)
    y = Array.create n 0;     (* rows -> cols *)
    u = Array.create n 0.;     (* dual row vars *)
    v = Array.create n 0.;     (* dual column vars *)
    matches = Array.create n 0;   (* array of cols *)
    col = Array.create n 0;   (* array of cols *)
    d = Array.create n 0.;     (* shortest path lengths *)
    free = Array.create n 0;  (* unassigned rows *)
    pred = Array.create n 0;  (* pred-array - shortest path tree *)

  } in
    q
;;

exception Augment;;
exception Zero_Error;;
exception GotoAugment of int;;
exception BadMatches;;
exception LoopAgain of lapjv_struct ;;
exception ReturnValue of float;;

let zero_to n =
  let rec r = function
      k when k >= n -> []
    | k when k >= 0 -> k :: (r (k+1))
    | _ -> raise Zero_Error
  in
    r 0
;;



let s_lapjv n costmatrix q =
  let inf = max_float in
  let zero_to_n = zero_to n in

	
  let n_to_zero = List.rev zero_to_n in
  let cost x y = costmatrix.(x).(y) in
    (*  let (f,h,i,j,k,f0,i1,j1,j2,u1,u2,min,last,low,up)  *)

    (*      for i = 0 to (n-1) do x.(i) <- 0 done *)
    (* COLUMN REDUCTION *)

  List.iter (
  fun j_ ->
    (* q.col.(j) <- j; *)
    q.j <- j_;
    q.i1 <- 0;
    q.min <- cost 0 q.j;
    for i = 1 to (n-1) do (* #1 *)
      q.i <- i;
      if (cost q.i q.j) < q.h then
	begin
	  q.h <- cost q.i q.j;
	  q.i1 <- i;
	end;
    done; (* #1 *)
    q.v.(q.j) <- q.min;
    q.matches.(q.i1) <- q.matches.(q.i1) + 1;
    if (q.matches.(q.i1) = 1) then
      begin
	q.x.(q.i1) <- q.j;
	q.y.(q.j) <- q.i1;
      end
    else
      begin
(*	q.x.(q.i) <- (-1)* abs q.x.(q.i); *)
	q.y.(q.j) <- (-1);
      end;
 ) n_to_zero;
  
  prerr_endline "Inited"; 

  for i = 0 to (n-1) do (* #2 *)
    q.i <- i;
    if (q.matches.(q.i) = 0) then
      begin q.free.(q.f) <- q.i; q.f <- q.f + 1; end
(*    else if (q.x.(i) < 0) then
      begin q.x.(i) <- (-1) * q.x.(i) end *)
    else if (q.matches.(i) = 1) then
      begin
	q.j1 <- q.x.(i);
	q.min <- inf;
	for j = 0 to (n-1) do (* #3 *)
	  q.j <- j;
	  if (not(q.j = q.j1)) then
	    begin      
	      (* if c[i,j]-v[j]<min then min:=c[i,j]-v[j];      v[j1]:=v[j1]-min *)
	      let diff = (cost q.i q.j) -. q.v.(q.j) in
	      if (diff < q.min) then begin q.min <- diff end;
	    end;
	done; (* #3 *)
	q.v.(q.j1) <- q.v.(q.j1) -. q.min;	    
      end
    else
      ()
  done;
  prerr_endline "Blk1";
  let cnt = ref 0 in
    
  let block cnt =
      prerr_endline "Blk2";
    q.k <- 0;
    q.f0 <- q.f;
    q.f <- 0;
    while (q.k < q.f0) 
    do (* WHILE#1 *)

      q.i <- q.free.(q.k) ;
      q.k <- q.k + 1; (* quit here Line 69 of pas *)
      q.u1 <- ((cost q.i 0) -. q.v.(0));
      q.j1 <- 0;
      q.u2 <- inf;
      for j = 1 to (n-1) do (* #4 *)
	q.j <- j;
	q.h <- (cost q.i q.j) -. q.v.(q.j);
	if q.h < q.u2 then
	  begin
	    if q.h >= q.u1 then begin q.u2 <- q.h; q.j2 <- q.j; end
	    else begin q.u2 <- q.u1; q.u1 <- q.h; q.j2 <- q.j1; q.j1 <- q.j end;
	  end;
      done; (* #4 *)
      
      q.i1 <- q.y.(q.j1) ;
      if (q.u1 < q.u2) then begin q.v.(q.j1) <- q.v.(q.j1) -. q.u2  +. q.u1 end
      else if (q.i1 >= 0) then begin q.j1 <- q.j2; q.i1 <- q.y.(q.j2) end;

      q.x.(q.i) <- q.j1;
      q.y.(q.j1) <- q.i;
      (* prerr_endline ((string_of_int q.k) ^ " " ^ (string_of_int q.i)  ^ " " ^(string_of_float q.u1) ^ " " ^(string_of_float q.u2));  *)
      let fabs x = if (x >= 0.) then x else (-1.0) *. x in
      let near x y = (fabs (x -. y)) < ((fabs (x +. y)) *. 0.0000001) in  
(*       let near x y = ((fabs (x -. y)) < 0.0000001) in  *)
      if (q.i1 >= 0) then
	begin
	  if ((not (near q.u1 q.u2)) && (q.u1 < q.u2)) then begin q.k <- q.k - 1; q.free.(q.k) <- q.i1; end  
(* 	  if (q.u1 < q.u2) then begin q.k <- q.k - 1; q.free.(q.k) <- q.i1; end  *)
	  else begin  q.f <- q.f + 1;q.free.(q.f) <- q.i1 ; end;
	  (* from cpp *)
	end;
    done; (* WHILE#1 *)
  in (* block end *)
  
  (* repeat block twice *) 
  block 0; 
  block 1; 
  prerr_endline "Blk2";
  (* augmentation *)
  q.f0 <- q.f;
  
  
      
  for f = 0 to (q.f0 - 1) do (* #5 *)
    
    q.f <- f;
    q.i1 <- q.free.(q.f);
    for j_ = 0 to (n-1) (* #6 *)
    do 
      q.j <- j_; q.d.(q.j) <- (cost q.i1 q.j) -. q.v.(q.j); q.pred.(q.j) <- q.i1 
    done; (* #6 *)
    (* might not need this initalization but didn't feel safe *)
    q.low <- 0;
    q.up <- 0;
(*    prerr_endline "Blk3"; *)
    try
      while (true) do (* WHILE #2 *)
	if (q.up = q.low) then
	  begin

	    q.last <- q.low - 1;
	    if (q.up >= n) then
	      raise (GotoAugment 0); 
	    q.min <- q.d.(q.col.(q.up));
	    q.up <- q.up + 1;

	    for k_ = q.up to (n-1) do (* #7 *)
	      q.k <- k_;
	      q.j <- q.col.(q.k);
	      q.h <- q.d.(q.j);
	      if (q.h <= q.min) then
		begin
		  if (q.h < q.min) then begin q.up <- q.low ; q.min <- q.h end;
		  q.col.(q.k) <- q.col.(q.up);
		  q.col.(q.up) <- q.j;
		  q.up <- q.up + 1;
		end;
	    done; (* #7 *)

	    for r = q.low to (q.up - 1) do (* #8 *)
	      if (q.y.(q.col.(r)) < 0) then raise (GotoAugment q.col.(r)); (* end of path *)
	    done; (* #8 *)
	      
	  end;
	(* FYI Still in the while loop *)

	q.j1 <- q.col.(q.low);
	q.low <- q.low + 1;
	q.i <- q.y.(q.j1); 
	q.h <- (cost q.i q.j1) -. q.v.(q.j1) -. q.min;
(*	prerr_endline "Blk4"; *)
	
	  (* CHECK WAS THIS IN THE PAPER?
	     q.u1 <- (cost q.i q.j1) -. q.v.(q.j1) -. q.min;
	   *)

	for k_ = q.up to (n-1) do (* #9 *)
	  q.k <- k_;

	  q.j <- q.col.(q.k);
	  q.u2 <- (cost q.i q.j) -. q.v.(q.j) -. q.h;

	  if (q.u2 < q.d.(q.j)) then
	    begin
	      q.pred.(q.j) <- q.i;
	      if (q.u2 = q.min) then
		begin
		  if (q.y.(q.j) < 0) then 
		    raise (GotoAugment (q.j)) (* end path *)
		  else
		    begin
		      q.col.(q.k) <- q.col.(q.up);
		      q.col.(q.up) <- q.j;
		      q.up <- q.up + 1;
		    end;
		end;
		q.d.(q.j) <- q.u2;
	    end;
	done; (* #9 *) 
	
      done; (* #WHILE 2 *)
    with (GotoAugment(x)) -> q.j <- x;
      (* label Augment *)
      
	  (* prerr_endline "Augmented"; *)
(*       prerr_endline "Augmented"; *)
      for k = 0 to q.last do (* #10 *)
	q.k <- k;
	q.j1 <- q.col.(q.k);
	q.v.(q.j1) <- q.v.(q.j1) +. q.d.(q.j1) -. q.min;
      done; (* #10 *)
	    
      q.first <- true ; (* let the first pass work *)
      while (q.first || not(q.i = q.i1)) do (* start while *)
	q.first <- false;
	q.i <- q.pred.(q.j);
	q.y.(q.j) <- q.i;
	q.j1 <- q.j;
	q.j <- q.x.(q.i);
	q.x.(q.i) <- q.j1;
      done; (* end while *)
    done;
    
    (* augmentation done *)
    
    (* check with pascal *)
    q.res <- 0.;
    for i_ = 0 to (n-1) do (* #11 *)
      q.i <- i_;
      q.j <- q.x.(q.i);
      q.u.(q.i) <- (cost q.i q.j) -. q.v.(q.j);
      q.res <- q.res +. (cost q.i q.j);
    done; (* #11 *)
    q.res
      
;; 

let for_map f max = 
    let rec foreach = function
	x when x >= max -> []
      | x -> (f x) :: (foreach (x+1))
    in
    foreach 0
;;

let cost_matrix_to_string n matrix =
  let strs = for_map (fun i -> 
    let strs = for_map 
	(fun j -> 
	  string_of_float matrix.(i).(j)
	) n in
    (String.concat "\t" strs)
		     ) n in
  (String.concat "\n" strs)^"\n"
;;

let lapjv n costmatrix =
  let q = create_lapjv n  in
    try 
      let res =  s_lapjv n costmatrix q in
	(res,q.x,q.y)
    with (Invalid_argument(x)) -> 
      let cm = cost_matrix_to_string n costmatrix in
      let _ = print_endline cm in
      let fd = open_out "./cost_matrix.debug" in
      let _ = output_string fd cm in
      let _ = close_out fd in
      raise (Invalid_argument (" Bad matrix " ^ x ));
      
;;

let verify_solution n x y =
  let si = string_of_int in
    for i = 0 to (n - 1) do
      if (x.(y.(i)) != i) then print_endline ("  x o y " ^ (si i) ^ " "^(si (x.(y.(i)))));
      if (y.(x.(i)) != i) then print_endline ("  y o x " ^ (si i) ^ " "^(si (y.(x.(i)))));
    done;
;;
      

let lapjv_test () =
  Random.self_init ();
  let n = 30 in
  let c = Array.make_matrix n n 1. in
    for x = 0 to (n-1) do
      for y = 0 to (n-1) do
	c.(x).(y) <- 1. +. (Random.float 10.);
      done;
    done;
    print_endline "OK READY";
    let (res, l,l2) = lapjv n c in
    print_float res;
    print_newline ();
    Array.iter (fun x -> print_int x; print_string " "; ) l;
    print_newline ();
    Array.iter (fun x -> print_int x; print_string " "; ) l2;
    print_newline ();
    verify_solution n l l2;
;;

let lapjv_test_2 () =
  let c = [|
    [| 0.0 ; 1.0 ; 2.0 ; 2.0 |] ;
    [| 1.0 ; 0.0 ; 2.0 ; 2.0 |] ;
    [| 2.0 ; 2.0 ; 0.0 ; 1.0 |] ;
    [| 2.0 ; 2.0 ; 1.0 ; 0.0 |]
  |] in
  let (res,l,l') = lapjv 4 c in 
    l = [| 0 ; 1 ; 2 ; 3 |] 
;;

let lapjv_test_2 () =
  (* 
     234_ __21
     1___ _3__
     ____ _4__

     

  *)
  let c = [|
    [| 4.0 ; 3.0 ; 2.0 ; 3.0 |] ;
    [| 3.0 ; 2.0 ; 2.0 ; 3.0 |] ;
    [| 2.0 ; 1.0 ; 1.0 ; 2.0 |] ;
    [| 1.0 ; 0.0 ; 1.0 ; 2.0 |]
  |] in
   (* 3 0 1 1 ?? *)
   (* 3 2 1 0 *) 
   (* 3 2 0 1 *) 
    

    lapjv 4 c

(*
  let (res,l,l') = lapjv 4 c in 
    l = [| 0 ; 1 ; 2 ; 3 |]  *)
;;



let shuffle n perm =
  for i = 0 to (n-1) do
    let r = Random.int n in
    let i' = perm.(i) in
    perm.(i) <- perm.(r);
    perm.(r) <- i';
  done;
;;
let abez_permuter n costmatrix =
  let perm = Array.of_list (zero_to n) in
  let not_chosen = Array.create n true in
  let finalperm = Array.of_list (zero_to n) in
  shuffle n perm;
  let sum = ref 0. in
  for i = 0 to (n-1) do
    let j = perm.(i) in
    let d = ref infinity in
    let di = ref 0 in
    for k = 0 to (n - 1) do
      let d' = costmatrix.(i).(k) in
      if (d' < !d) then
	begin
	  d := d';
	  di := k;
	end;
    done;
    not_chosen.(!di) <- false;
    finalperm.(j) <- !di;
    sum := !sum +. !d;
  done;
  (!sum,finalperm,finalperm)
;;
let min_cost_assignment n costmatrix =
  let finalperm = Array.of_list (zero_to n) in
  let sum = ref 0. in
  for i = 0 to (n - 1) do
    let minarg = ref 0 in
    for k = 0 to (n - 1) do
      if (costmatrix.(i).(k) < costmatrix.(i).(!minarg)) then
        minarg := k
    done;
      finalperm.(i) <- !minarg;
      sum := !sum +. costmatrix.(i).(!minarg);
  done;
    (!sum, finalperm, finalperm)
;;
    
