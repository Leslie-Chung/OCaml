(*
  Usage: ocaml lec08owl.ml
*)

#use "topfind";;
#require "owl"
#load "lec08.cmo"
open Owl
open Lec08
open Printf

let _ = matrix_a_test3 () 


(** solve linear equation to get optimal point. **)

(* solution of analytic placement equation. *)
type tpCoordAry = (float * float) array

let pr_fpairs (v:tpCoordAry) =
  let prf ((x,y):float*float) = 
    printf "(%-4.2f,%4.2f) " x y in
    Array.iter prf v


(* main solver of analytic placement equations AX=bx and BX=by. *)
let eqn_solve 
    (c:tpAary) (p:tpAary) (bx:tpBvec) (by:tpBvec) : tpCoordAry =
  let n  = Array.length bx in
  let a  = matrix_A c p in
  let a  = Mat.of_arrays a in
  let bx = Mat.of_array bx n 1 in
  let by = Mat.of_array by n 1 in
    (* solution of linea equations AX = bx, AY = by. *)
  let xv = Mat.to_array (Linalg.D.linsolve a bx) in 
  let yv = Mat.to_array (Linalg.D.linsolve a by) in 
    Array.combine xv yv

(* this test calculates the matrix A and use it wtih bx by
   to form two linear equations. an example from slide 63 *)
let matrix_a_test4 () =
  let c : tpAary = matrix_i2f [|
    [| 0;1;10;0;0|];
    [| 1;0;1;1;1 |];
    [| 10;1;0;1;0|];
    [| 0;1;1;0;1 |];
    [| 0;1;0;1;0 |];
  |]
  in
  let p : tpAary =  (* 1-10;2-0;3-1;4-1;5-1 *)
    ary_of_pad_node_list [10;0;1;1;1]
  in
  let bx = [|0.;0.;1.;1.;0.5|] in
  let by = vec_i2f [|10;0;0;1;0|] in
  let solution = eqn_solve c p bx by in
    print_endline "\nSolution of AX=bx and AY=by";
    pr_fpairs solution;
    print_newline ()

let _ = matrix_a_test4 ()

(*
utop lec08owl.ml
Solution of AX=bx and AY=by
(0.13,0.89) (0.33,0.74) (0.24,0.80) (0.50,0.76) (0.44,0.50) 
*)
  
