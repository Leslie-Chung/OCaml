
open Owl
open Lec08
open Lec09
open Printf


(** solve linear equation to get optimal point. **)

(* solution of analytic placement equation. *)
type tpCoordAry = (float * float) array

let pr_fpairs (v:tpCoordAry) =
  let prf ((x,y):float*float) = 
    printf "(%-4.2f,%4.2f) " x y in
    Array.iter prf v

(* since the first row and the first columne are all zeros, so we need 
   to remove thenm before solving the equation, otherwise the matrix
   is not of full rank. *)

(* remove first element of a vector. *)
let rm_vec_first (v:'a array) : 'a array =
  Array.of_list (List.tl (Array.to_list v))

let rm_first_row_col (a: 'a array array) =
  let a = rm_vec_first a in
  let a = Array.map rm_vec_first a in
    a

let normalizeAbxy (a: tpAary) (bx:tpBvec) (by:tpBvec) =
  let a  = rm_first_row_col a in
  let bx = rm_vec_first bx in
  let by = rm_vec_first by in
    a,bx,by

let graph_eqn_solve (g:tpGraph) : tpCoordAry =
  let a,bx,by = graph2matrixA g in
  let a,bx,by = normalizeAbxy a bx by in
  let n  = Array.length bx in
  let a  = Mat.of_arrays a in
  let bx = Mat.of_array bx n 1 in
  let by = Mat.of_array by n 1 in
    (* solution of linea equations AX = bx, AY = by. *)
  let xv = Mat.to_array (Linalg.D.linsolve a bx) in 
  let yv = Mat.to_array (Linalg.D.linsolve a by) in 
    Array.combine xv yv
    
let five_node_graph_test () =
  let solution = graph_eqn_solve five_node_graph in
    print_endline "\nSolution of AX=bx and AY=by";
    pr_fpairs solution;
    print_newline ()

let _ = five_node_graph_test ()
  
