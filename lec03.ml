(* 
   Compilation: ocamlopt -o lec03.exe lec03.ml
   Execution: ./lec03.exe
   Goals of lec03.ml
   extend lec02.ml by:
   1. calculuate HPWL for whole nets;
   2. optimization of HPWL computation;
   3. test and compare HPWL algorithms.

   Author: Gang Chen
   Date:   Sept. 5, 2022
   Main function:
   total_hpwl, calc_int_time
   mian
*)

open Printf
open List

(* a cell is a pair of integers. *)
type tpGate = int * int (* x * y = cols & rows *)

(* the type of net is a list of cells. *)
type tpNet = tpGate list

(** Examples of wires **)


(* slide 1 *)
(*  A "2-point net" *)
let two_point_net : tpNet = [(1,4);(3,1)]

(* A "4-point net" *)
let four_point_net : tpNet = 
  two_point_net @ [(3,3);(4,5)]

(* slide 16 - 19 *)

(** Half Perimeter Wire Length (HPWL) or Bounding Box (BBWL) **)

(* split the net into the list of column numbers and row numbers. *)
let net_cols_rows (net:tpNet) : (int list) * (int list) =
  List.split net


(* example: cols and rows of 2-point net *)
let two_point_net_xy = net_cols_rows two_point_net

let four_point_net_xy = net_cols_rows four_point_net

(*
# List.fold_left;;
- : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = <fun>
*)

(* return the maximu of an int list. *)
let int_list_max (ilist : int list) : int =
  List.fold_left max 0 ilist

(* the minmum of an int list. *)
let int_list_min (ilist : int list) : int =
  List.fold_left min max_int ilist

(* definition using fold_right. *)
let int_list_max' (ilist : int list) : int =
  List.fold_right max ilist 0
;;
int_list_max' [1;4;2;3];;

(* return the coordinates of lower left and upper right gates in the net. *)

(* return the Half Perimeter Wire Length (HPWL) *)
let hpwl (net:tpNet) : int = 
  let (cols,rows) = net_cols_rows net in
  let max_col, max_row = int_list_max cols, int_list_max rows in
  let min_col, min_row = int_list_min cols, int_list_min rows in
  let width,height = (max_col - min_col, max_row - min_row) in
  let hpwl = width + height in
   hpwl

let two_point_net_hpwl  = hpwl two_point_net    (* 5 = 2+3 *)
let four_point_net_hpwl = hpwl four_point_net  (* 7 = 3+4 *)

(** incremental wire length computation for a gate swap. **)

(* slide 24 *)

(* type for a set of nets. *)
type tpNets = tpNet list

(* an example of 4 gates and 3 nets. *)

(* the following nets will be redefined below by using gate names. *)
let net_i :tpNet = [(1,4);(3,3);(4,5)]
let net_j :tpNet = [(3,3);(2,1)]
let net_k :tpNet = [(2,1);(4,5)]
let three_nets : tpNets = [net_i;net_j;net_k] 

(* compute the total HPWL wire length. *)
(* the sum of an int list. *)
let int_list_sum (ilist : int list) : int =
  List.fold_left (+) 0 ilist

(* total HPWL wire length of a nets. *)
let total_hpwl (nets:tpNets) : int =
  int_list_sum (List.map hpwl nets)

(* HPWL v 1.0 *)
let total_hpwl_v1 = total_hpwl

let three_nets_hpwls = List.map hpwl three_nets
let three_nets_hpwl = total_hpwl three_nets

(** a more efficient hpwl by eliminating intermediate memory allocation. **)

(* type for a pair of gates for bottom-left and top-right coordinates *)
type tpGate2 = tpGate * tpGate

(* return lower left and uppper right coordinates for two nets. *)
let add_gate_left (gate2:tpGate2)  (gate:tpGate) : tpGate2 =
  let (x,y) = gate in
  let ((min_col,min_row), (max_col,max_row)) = gate2 in
  let min_col = min min_col x and max_col = max max_col x in
  let min_row = min y min_row and max_row = max y max_row in
    ((min_col,min_row),(max_col,max_row))
  

(* a version of hpwl avoiding the generation of intermediate lists. *)
let hpwl' (net:tpNet) =
  let init = ((max_int,max_int),(0,0)) in
  let ((min_col,min_row),(max_col,max_row)) =
    List.fold_left add_gate_left init net  in
    (max_col - min_col) + (max_row - min_row)

(* total HPWL wire length of a nets. *)
let total_hpwl (nets:tpNets) : int =
  int_list_sum (List.map hpwl' nets)

(*  HPWL v 2.0 *)
let total_hpwl_v2 = total_hpwl

let three_nets_hpwls = List.map hpwl' three_nets
let three_nets_hpwl = total_hpwl three_nets (* 14 *)

(** HPWL v 3.0 -- eliminated List.map **)

(* add the hpwl of a net to the accumulated hpwl*)
let add_net_hpwl (hpwl_sum:int) (net:tpNet) : int =
  hpwl_sum + (hpwl' net)

(* total hpwl redefined without creating intermediate list. *)
let total_hpwl (nets:tpNets) : int =
  List.fold_left add_net_hpwl 0 nets

(*  HPWL v 3.0 *)
let total_hpwl_v3 = total_hpwl

let three_nets_hpwl = total_hpwl three_nets (* 14 *)




(** test the performance of hpwl algorithm **)

(* calculate the computation time of executing a function. *)
let calc_int_time (f : 'a -> int) (n:'a) =
  let start_time = Sys.time () in
  let v = f n in
  let end_time = Sys.time () in
  let elapsed = end_time -. start_time in
    printf "Execution of f () = %i takes %6.2g seconds\n" v elapsed
      

(* generate a random gate. *)
let random_gate (gate_bound:int) : tpGate = 
  (Random.int gate_bound, Random.int gate_bound)

let random_net (gate_bound:int) : tpNet =
  (* a net has at least 2 gates *)
  let total_nets = 2 + Random.int gate_bound in
  let rec mk_net n result =
    if n=0 then result else
      mk_net (n-1) ((random_gate gate_bound) :: result)
  in mk_net total_nets []

(* return a randomly generated nets, each net has at most gate_bound gates. *)
let random_nets (gate_bound:int) (total_nets:int) : tpNets =
  let rec mk_nets n result =
    if n=0 then result else
      mk_nets (n-1) ((random_net gate_bound) :: result)
  in mk_nets total_nets []

let test_hpwl gate_bound total_nets =
  let nets = random_nets gate_bound total_nets in
  let _ = calc_int_time total_hpwl_v1 nets in
  let _ = calc_int_time total_hpwl_v2 nets in
  let _ = calc_int_time total_hpwl_v3 nets in
    ()
  
(*
# test_hpwl 100 100000;;
Execution of f () = 18519994 taks   0.87 seconds
Execution of f () = 18519994 taks   0.81 seconds
Execution of f () = 18519994 taks   0.59 seconds
*)

let main () =
  test_hpwl 100 100000
;;

main();;
