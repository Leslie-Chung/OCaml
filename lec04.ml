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
    printf "Execution of f () = %i takes %6.2g seconds\n" v elapsed;
    v
      
(* generate a gate_id in [1..gate_bound] inclusive. *)
let random_gate_id (gate_bound:int): int =
   (Random.int (gate_bound-1)) + 1

(* generate a random gate. *)
let random_gate (gate_bound:int) : tpGate = 
  (random_gate_id gate_bound, random_gate_id gate_bound)
;;

(*  random_gate 5;; *)

let random_net (gate_bound:int) : tpNet =
  (* a net has at least 2 gates *)
  let total_nets = 2 + Random.int gate_bound in
  let rec mk_net n result =
    if n=0 then result else
      let new_gate = (random_gate gate_bound) in
	if List.mem new_gate result
	then mk_net (n-1) result
        else mk_net (n-1) (new_gate::result)
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
;; 

(*
test_hpwl 100 100000;;
Execution of f () = 18519994 taks   0.87 seconds
Execution of f () = 18519994 taks   0.81 seconds
Execution of f () = 18519994 taks   0.59 seconds
*)
(*
let main () =
  test_hpwl 100 100000
;;

main();;
*)

(** lec04.ml extension **)


(* name all gates in the nets. *)
let g1 = (3,3)  (* middle *)
let g2 = (2,1)  (* bottom *)
let g3 = (1,4)  (* up left *)
let g4 = (4,5)  (* up right *)

(* swap  g1 and g2 *)

let net_i :tpNet = [g3;g1;g4]
let net_j :tpNet = [g1;g2]
let net_k :tpNet = [g2;g4] 
let three_nets : tpNets = [net_i;net_j;net_k] 
let three_nets_hpwl = total_hpwl three_nets
  

(* return  the maximum gate id of a net. *)
let max_gate_id_in_net (net:tpNet) : int =
  let max_id m (i,j) = max (max i j) m in
    List.fold_left max_id 0 net

(* return the maximum gate id of a nets. *)
let max_gate_id (nets:tpNets) : int =
  let max_id m net = max m (max_gate_id_in_net net) in
    List.fold_left max_id 0 nets

let max_gate_in_three_nets = max_gate_id three_nets

(** normalization of nets to its unique form. **)

(* compare two pairs. *)
let pair_compare (i1,j1) (i2,j2) : int =
  let b = Stdlib.compare i1 i2 in
  if b <> 0
  then b
  else Stdlib.compare j1 j2

(* sort a list of pairs. *)
let pair_list_sort (pl : ('a * 'b) list) : ('a * 'b) list =
  List.sort pair_compare pl

(* normzalize nets to a unique form. *)
let nets_normalization (nets : tpNets) : tpNets =
  List.map pair_list_sort nets

(** extract a list of all gates from nets. *)

let list_add (e: 'a) (l: 'a list) : 'a list =
  if List.mem e l then l else e::l

(* union of two list sets. *)
let list_union (l1: 'a list) (l2: 'a list) : 'a list =
  List.fold_right list_add l1 l2

(* return the list of all gates. *)
let all_gates (nets:tpNets) : tpGate list =
  List.fold_left list_union [] nets

(* test all_gates *)
let all_gates_in_three_nets = all_gates  three_nets


(* normalzation helps to compare two nets and to check the correctness
   of a function, but it has a high cost. *)

(** swap gate in a nets. **)

(* gate swap version 1.0 *)

(* swap gates gi and gj in net, with the assumption that
   each net *)
(* swap gates gi and gj in net, gi and gj might be in the same net. *)
let rec net_gate_swap' 
  (gi:tpGate) (gj:tpGate) (net:tpNet) result : tpNet =
  match net with
    | g::tl ->
      if g=gi 
      then net_gate_swap' gi gj tl (gj::result)
      else if g=gj
      then net_gate_swap' gi gj tl (gi::result)
      else net_gate_swap' gi gj tl (g::result)
    | [] -> result


let net_gate_swap 
    (gi:tpGate) (gj:tpGate) (net:tpNet) : tpNet =
   net_gate_swap' gi gj net []

(* swap gates gi and gj in nets. *)
let nets_gate_swap 
 (gi:tpGate) (gj:tpGate) (nets:tpNets) : tpNets =
  let swap net = 
    if List.mem gi net || List.mem gj net
    then List.rev (net_gate_swap gi gj net)
    else net
  in
    List.map swap nets

let three_nets_swap_g1_g2 = nets_gate_swap g1 g2 three_nets
(*
 three_nets = [[(1, 4); (3, 3); (4, 5)]; [(3, 3); (2, 1)]; [(2, 1); (4, 5)]]
 output =     [[(1, 4); (2, 1); (4, 5)]; [(2, 1); (3, 3)]; [(3, 3); (4, 5)]]
*)
let three_nets_swap_g1_g2 = 
  nets_normalization (nets_gate_swap g1 g2 three_nets)
(* normalization makes it easy to compare the change *)

let swap_test_nets1 = 
  nets_normalization (random_nets 4 4)

let swap_test1 () : tpNets =
  let g1 = (3,1) and g2 = (1,2) in
  (nets_gate_swap g1 g2 swap_test_nets1)
;;
(* swap_test1 ();; *)

let swap_test_nets2 =
 [[(1, 1); (3, 3); (4, 3)]; [(1, 2); (3, 3)]; [(4, 1); (4, 3); (4, 4)];
   [(1, 1); (1, 3); (2, 2); (4, 4)]];;

let swap_test2 () : tpNets =
  let g1 = (3,3) and g2 = (4,1) in
  nets_normalization (nets_gate_swap g1 g2 swap_test_nets2)

;;
(* swap_test2 ();; *)

let swap_test_nets3 = 
  nets_normalization (random_nets 4 40)

let swap_test3 () : tpNets =
  let g1 = (3,3) and g2 = (4,1) in
   (nets_gate_swap g1 g2 swap_test_nets3)
;;
(* swap_test3 ();; *)

(** nets swap version 1.0 **)

(* return new nets and delta of wire length after swap. *)
let nets_gate_swap_wl_delta 
    (gi:tpGate) (gj:tpGate) (nets:tpNets) : tpNets * int =
  let new_nets = nets_gate_swap gi gj nets in
  let delta_wl (net, new_net) = (hpwl new_net) - (hpwl net) in
  let new_old_pairs = List.combine new_nets nets in
  let deltas = List.map delta_wl new_old_pairs in
  let total_delta = int_list_sum deltas in
    new_nets, total_delta

let three_nets_wl_delta = 
  nets_gate_swap_wl_delta g1 g2 three_nets
 

let three_nets_wl_delta = 
  let nets, i = (nets_gate_swap_wl_delta g1 g2 three_nets) in
    nets_normalization nets, i


(* randomly pick up two gates. *)
let random_two_gates (gates:tpGate list) (len:int) : tpGate * tpGate =
  let i = Random.int len and j = Random.int len in
  let gi = List.nth gates i and gj = List.nth gates j in
    gi,gj


(* random swap placer v1.0 *)

(* return optimized nets and the final hpwl. *)
let swap_placer_v1 (nets:tpNets) (uplimit:int) : int*tpNets =
  let gates = all_gates nets in
  let total_gates = List.length gates in
  let hpwl_val = total_hpwl nets in
  let rec sp nets delta_hpwl n = 
    if n>=uplimit
    then (hpwl_val+delta_hpwl),nets
    else 
      let gi,gj = random_two_gates gates total_gates in
      let new_nets,delta = nets_gate_swap_wl_delta gi gj nets in
	if delta>=0
	then sp nets delta_hpwl (n+1)
	else sp new_nets (delta_hpwl+delta) (n+1)
  in sp nets 0 0
;;
(*
three_nets;;
total_hpwl three_nets;;
swap_placer_v1 three_nets 10;;

Result:
# three_nets;;
- : tpNets = [[(1, 4); (3, 3); (4, 5)]; [(3, 3); (2, 1)]; [(2, 1); (4, 5)]]
# total_hpwl three_nets;;
- : int = 14
# swap_placer_v1 three_nets 10;;
- : int * tpNets/2 =
(13, [[(1, 4); (3, 3); (4, 5)]; [(1, 4); (2, 1)]; [(2, 1); (4, 5)]])
*)
(*
let place_test_nets1 = random_nets 10 10;;
total_hpwl place_test_nets1;;
swap_placer_v1 place_test_nets1 100;;

let place_test_nets2 = random_nets 20 100;;
total_hpwl place_test_nets2;;
swap_placer_v1 place_test_nets2 200;;
*)
let test_swap_placer swap_placer 
    gate_bound total_nets swap_times =
  let nets = random_nets gate_bound total_nets in
  let hpwl_val = total_hpwl nets in
  let swap_placer swap_times = 
    let i,_ = swap_placer nets swap_times in  i
  in
  let i1 = calc_int_time swap_placer 10 in
  let i2 = calc_int_time swap_placer 200 in
  let i3 = calc_int_time swap_placer swap_times in
    printf "original hpwl = %i,  hpwl after 10,200,%i swaps = %i,%i,%i\n" 
      hpwl_val swap_times i1 i2 i3;
    printf "improvements = %i,%i,%i\n" 
      (hpwl_val-i1) (hpwl_val-i2) (hpwl_val-i3)
;;

test_swap_placer swap_placer_v1 20 1000 10000;;

