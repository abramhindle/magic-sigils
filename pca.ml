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

open Gsl_linalg;;
open Gsl_stats;;
open Gsl_matrix;;
open Gsl_blas;;
open Gsl_vector;;
open Abez;;


let status x = ();;
(*  prerr_endline ("[PCA] "^x) 
;; *)
let si = string_of_int ;;
let sf = string_of_float ;;


(*
Gsl_linalg
val invert_LU : ?protect:bool ->
       ?result:Gsl_vectmat.mat ->
       [< `A of float array * int * int
        | `AA of float array array
        | `M of Gsl_matrix.matrix
        | `MF of Gsl_matrix_flat.matrix ] ->
       Gsl_vectmat.mat

Gsl_stats

val covariance : float array -> float array -> float

*)

let dims matrix = 
  let (rows,cols) = Gsl_matrix.dims matrix in
    (rows,cols)
;;

let matrix_get matrix row col =
  Gsl_matrix.get matrix row col
;;

let matrix_set matrix row col v =
  Gsl_matrix.set matrix row col v
;;

let print_matrix matrix =
  let (rows,cols) = dims matrix in
  for i = 0 to (rows - 1) do
    for j = 0 to (cols - 1)  do 
      print_float (matrix_get matrix i j);
      print_string "\t";
    done;
    print_endline "";
  done;
;;

let print_vector matrix =
  let rows = Gsl_vector.length matrix in
  for i = 0 to (rows - 1) do
    print_float (Gsl_vector.get matrix i);
    print_endline "";
  done;
;;


let print_array array =
  let l = Array.length array in
  for i = 0 to (l - 1) do
    print_float array.(i);
    print_string "\t";
  done;
    print_endline "";
;;


    
let get_column_inplace arr matrix col =
  let (rows,cols) = dims matrix in
    for i = 0 to rows - 1 do
      arr.(i) <-  matrix_get matrix i col;
    done;
    arr
;;

let create_matrix rows cols =
  Gsl_matrix.create rows cols 
;;


let get_column matrix col =
  let (rows,cols) = dims matrix in
  let arr = Array.create rows 0. in
    get_column_inplace arr matrix col
;;

let covariance_matrix_in_place cov_matrix matrix =
  let (rows,cols) = dims matrix in
  let cov_matrix = create_matrix cols cols in
    for i = 0 to cols - 1 do
      let icol = get_column matrix i in
	for j = 0  to cols - 1 do
	  matrix_set cov_matrix i j (Gsl_stats.covariance icol (get_column matrix j));
	done;
    done;
    cov_matrix
;;

let covariance_matrix matrix =
  let (rows,cols) = dims matrix in
  let cov_matrix = create_matrix cols cols in
    covariance_matrix_in_place cov_matrix matrix 
;;
      
let eigen_vector_values matrix =
  Gsl_eigen.symmv (`M(matrix))
;;

let eigen_sort (v,m) =
  Gsl_eigen.symmv_sort (v,m) Gsl_eigen.VAL_DESC;
  (v,m)
;;

let mean_of_column matrix col =
  let (rows,cols) = dims matrix in
  let sum = ref 0. in
  for i = 0 to (rows - 1) do
    sum := !sum +. (matrix_get matrix i col);
  done;
    (!sum) /. (float_of_int rows)
;;

let apply_column matrix f col =
  let (rows,cols) = dims matrix in
  for i = 0 to (rows - 1) do
    matrix_set matrix i col (f (matrix_get matrix i col));
  done;
    ()
;;

let matrix_multiply m1 m2 =
  let (rows1,cols1) = dims m1 in
  let (rows2,cols2) = dims m2 in
  let c =   create_matrix rows1 cols2 in
  let  _ = Gsl_blas.gemm ~ta:Gsl_blas.NoTrans ~tb:Gsl_blas.NoTrans ~alpha:1.0 ~a:m1 ~b:m2 ~beta:0.0 ~c:c in
    c
;;

let transpose_matrix matrix =
  let (rows,cols) = dims matrix in
  let m = create_matrix cols rows in
  Gsl_matrix.transpose m matrix;
    m
;;
let vector_set = Gsl_vector.set ;;
let vector_get = Gsl_vector.get ;;
let avg_matrix inmat =
  let outmat = Gsl_matrix.copy inmat in
  let (rows,cols) = dims outmat in
  let avg_vector = Gsl_vector.create cols in (*dims*)
  let (rows,cols) = dims inmat in
    status "Calc avg cols";
    for i = 0 to (cols - 1) do
      let avg = ref 0. in
	for j = 0 to (rows - 1) do
	  avg := !avg +. (matrix_get inmat j i);
	done;
	avg := !avg /. (float_of_int rows) ;
	vector_set avg_vector i !avg;
    done;
    status "Set matrix";
    for j = 0 to (rows - 1) do
      for i = 0 to (cols - 1) do
	let rowavg = vector_get avg_vector i in
	  matrix_set outmat j i ((matrix_get inmat j i) -. rowavg);
	done;
    done;
    (outmat,avg_vector)
;;

let pca matrix =
  (* subtract the mean *)
  let (rows,cols) = dims matrix in
    status ("PCA on matrix of rows: "^(si rows)^" cols: "^(si cols));
  let (adjmatrix,avg_vector) = avg_matrix matrix in
(*    print_endline "Adj Matrix";
    print_matrix adjmatrix; *)

  (* for i = 0 to (cols - 1) do
    let mean = mean_of_column adjmatrix i in
      apply_column adjmatrix (fun x -> x -. mean) i;
  done; *)
    status "Covariance Matrix";
  let cv = covariance_matrix adjmatrix in
  let (values,vectors) = eigen_vector_values cv in
  let (values,vectors) = eigen_sort (values,vectors) in
  let rowvectors = transpose_matrix vectors in
  let rowmatrix  = transpose_matrix adjmatrix in
  let multi      = matrix_multiply rowvectors rowmatrix in
    (* WHAT ARE WE RETURNING? *)
  (multi,values,vectors,avg_vector)
;;

(* 

let project_to_space eigenvectors avgvector image =
   flatten image into vector
   img' = subtract avg image
   make img'' (M elements) dot img' against each row of eigenmatrix
   let norm = root sum_i=1..M img''[i] x img''[i]
   divide img'' by norm

Identify(Image): Image is $W\times H$ pixels in size.

   1. Load the saved known projected faces from the database.
   2. proj = Project to Face Space(Image)
   3. Take the dot product of proj against each known projected face. Call this the score.
   4. The known projected face that gets the highest score is considered the identity of the test image.

*)

let convert_to_vector l =
  let n = List.length l in
  let v = Gsl_vector.create n in
  let l' = ref l in
  for i = 0 to (n-1) do
    let (x::xs) = !l' in
      l' := xs;
      vector_set v i x
  done;
      v
;;

(* image is grayscale array *)
let project_to_space ev avgvector image =
  (* flattened image *)
  let (m,n) = dims ev in

    status ("DIMS ROWS:"^(si m)^" Cols:"^(si n));
  let img' = Gsl_vector.of_array image in
    status ("img' length"^(si (Gsl_vector.length img')));
    status ("avgvector length"^(si (Gsl_vector.length avgvector)));
  Gsl_vector.sub img' avgvector;
  let img'' = Gsl_vector.create (Gsl_vector.length img') in (* m or n ?????? *)
  status "Projection dot product?";
    for i = 0 to (m-1) do
      let row = Gsl_matrix.row ev i in
      let dot = Gsl_blas.dot img' row  in
      Gsl_vector.set img'' i dot;
    done;
    status "Normalize"; 
    let norm = Gsl_blas.nrm2 img'' in
      Gsl_blas.scal norm img'';
  img''
;;
let vector_create = Gsl_vector.create ;;
let vector_length = Gsl_vector.length ;;
let get_column_vector_inplace matrix col outvector =
  let n = vector_length outvector in
  for i = 0 to ( n - 1 ) do
    vector_set outvector i (matrix_get matrix i col);
  done;
  outvector 
;;
let identify ev av projs image =
  status "Project to space!";
  let proj = project_to_space ev av image in
  let (n,n') = dims projs in
    status ("Rows of projs "^(string_of_int n));
    status ("Cols of projs "^(string_of_int n'));
    let outvector = vector_create n in
  let f i (a,b,c) =
    status "GET COLUMN";
      let row = get_column_vector_inplace projs i outvector in
      let dot = Gsl_blas.dot proj row  in
      let (v,d,j) = (a,b,c) in
	if (dot > d) then (row,dot,i) else (v,d,j)
  in
  let m = ref ((Gsl_vector.create 1),0.,0) in
    status "Row dot proj";
    for i = 0 to (n'-1) do
      m := f i !m
    done;
    let (a,b,c) = !m in
      (a,b,proj,c)
;;


let pca_test () =
  let x = [| 2.5 ; 0.5 ; 2.2 ; 1.9 ; 3.1 ; 2.3 ; 2.  ; 1. ; 1.5 ; 1.1 |] in
  let y = [| 2.4 ; 0.7 ; 2.9 ; 2.2 ; 3.0 ; 2.7 ; 1.6 ; 1.1; 1.6 ; 0.9 |] in
    status "Matrix create";
  let matrix = create_matrix (Array.length x) 2 in
    status "Matrix set";
  for i = 0 to ((Array.length x) - 1) do
    matrix_set matrix i 0 (x.(i));
    matrix_set matrix i 1 (y.(i));
  done;
    status "PCA";
    let (finaldata,eigenvalues,eigenvectors,meanmatrix) = pca matrix in
      print_endline "Init Data";
      print_matrix matrix;
      print_endline "Mean Data";
(*      print_matrix meanmatrix; *)
      print_vector meanmatrix;
      print_endline "Eigenvalues";
      print_vector eigenvalues;
      print_endline "Eigenvectors";
      print_matrix eigenvectors;
      print_endline "Final Data";
      print_matrix finaldata;
      ()
;;      

(* pca_test();; *)

