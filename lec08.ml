(*
  Ionia is an experimental layout software by OCaml
  Ionia version 1.6 -- nets_v6.ml
  Initial implementation of 
  Current implemented components:

  Data structure:  netlist represented by array of gates and array of nets;
  Major Functions:
  1. mk_netlist -- build array based netlist from list based nets;
  2. recover_netlist -- recover list based nets from array based netlist;
  3. swap_gates -- gate swap;
  4. total_hpwl -- total HPWL calculation;
  5. swap_gates_hpwl -- swap gates and return changed hpwl;
  6. main_test  -- main test scripts
  
  Current Status: main_test () can show random placement process for a tiny example.
  Remaining problem2: 
  1. need to run bigger examples with compiled executable with intermediate 
     result printing disabled;
  2. add exchange between one gate and an empty location.

*)
open Printf

(** supporting functions **)

(* compare two pairs. *)
let pair_compare (i1,j1) (i2,j2) : int =
  let b = Stdlib.compare i1 i2 in
  if b <> 0
  then b
  else Stdlib.compare j1 j2

(* sort a list of pairs. *)
let pair_list_sort (pl : ('a * 'b) list) : ('a * 'b) list =
  List.sort pair_compare pl


(** array based gates and nets. **)

(* a gate has an id, a location and a list of connected nets. *)
type tpGate = {
  mutable coord  : int * int;
  mutable nets   : tpNet list
}
and tpNet = {
   mutable gates  : tpGate list
 }

let empty_gate :tpGate = { 
  coord = (0,0);
  nets  = []
}

let empty_net : tpNet = {
  gates = []
}

(* convert a coordination to a gate without connecting to any net. *)
let pair2gate ((i,j):int*int) : tpGate =
  {
    coord = (i,j);
    nets  = []
  }

(* convert a list of pairs a net record. *)
let ilist2net (ilist:(int*int) list) : tpNet =
  {
    gates = List.map pair2gate ilist;
  }


(* array of gates, in which the indexes are gate id's *)
type tpGateAry = tpGate array

(* array of nets, in which indexes are net id's *)
type tpNetAry = tpNet array

(* the main data structure of a net list which is composed of 
   a gate array and a net array. *)
type tpNetList = tpGateAry * tpNetAry

(* return an uninitialized gate array of length n. *)
let mk_gate_ary (n:int) : tpGateAry =
  Array.make n empty_gate

let mk_empty_net_ary (n:int) : tpNetAry =
  Array.make n empty_net

(** general list manipulation functions **)

let list_add (e: 'a) (l: 'a list) : 'a list =
  if List.mem e l then l else e::l

(* union of two list sets. *)
let list_union (l1: 'a list) (l2: 'a list) : 'a list =
  List.fold_right list_add l1 l2

(* return the maximu of an int list. *)
let int_list_max (ilist : int list) : int =
  List.fold_left max 0 ilist

(* the minmum of an int list. *)
let int_list_min (ilist : int list) : int =
  List.fold_left min max_int ilist

(** module of list form tpNets **)
(* 
   the module of list representation of nets. 
   it can be  used to construct a simple nets which can be transformed
   to tpNetAry and tpGateAry
*)
module ListNetsMod = struct

(* a cell is a pair of integers. *)
type tpGate = int * int (* x * y = cols & rows *)

(* the type of net is a list of cells. *)
type tpNet = tpGate list

type tpNets = tpNet list

(* return  the maximum gate id of a net. *)
let max_gate_id_in_net (net:tpNet) : int =
  let max_id m (i,j) = max (max i j) m in
    List.fold_left max_id 0 net

(* return the maximum gate id of a nets. *)
let max_gate_id (nets:tpNets) : int =
  let max_id m net = max m (max_gate_id_in_net net) in
    List.fold_left max_id 0 nets

(* return the list of all gates. *)
let all_gates (nets:tpNets) : tpGate list =
  List.fold_left list_union [] nets

(* normzalize nets to a unique form. *)
let nets_normalization (nets : tpNets) : tpNets =
  List.map pair_list_sort nets


(** calculating hpwl **)

(* split the net into the list of column numbers and row numbers. *)
let net_cols_rows (net:tpNet) : (int list) * (int list) =
  List.split net

(* return the Half Perimeter Wire Length (HPWL) *)
let hpwl (net:tpNet) = 
  let (cols,rows) = net_cols_rows net in
  let max_col, max_row = int_list_max cols, int_list_max rows in
  let min_col, min_row = int_list_min cols, int_list_min rows in
  let width,height = (max_col - min_col, max_row - min_row) in
  let hpwl = width + height in
   hpwl

(* the sum of an int list. *)
let int_list_sum (ilist : int list) : int =
  List.fold_left (+) 0 ilist

(* total HPWL wire length of a nets. *)
let total_hpwl (nets:tpNets) : int =
  int_list_sum (List.map hpwl nets)

end

module M = ListNetsMod

(** Analytical Placement **)

(* quadratic wirelength model *)

(* quadratic wire length *)
let quad_len (a:M.tpGate) (b:M.tpGate) : int = 
  let (a1,a2) = a and (b1,b2) = b in
  let x = a1-b1 and y = a2-b2 in
    x*x + y*y


(* convert a list to a fully connected set of pairs
   and compute their quadratic sum divided by k-1.
   e.g. [g1;g2;g3] -> [(g1,g2);(g1,g3);(g2,g3)]
   slide 50
*)

(* convert a list to paring elements list. *)
let paring (a :'a list) : ('a * 'a) list =
  let paring (e:'a) (a:'a list) =
    let pair x  = (x,e)in
      List.map pair a
  in
  let rec pa a result =
    match a with
      | hd::tl -> pa tl ((paring hd tl) @ result)
      | [] -> result
  in
    pa a []
;;
(* paring [1;2;3;4];; *)

(** create two-point nets from tpInputNetList **)

type tpInputNet = int list
type tpInputNetList = tpInputNet list
type tpTwoPointNet = (int * int) list

(* convert an M.tpNets object to a list of pairs. *)
let mnets2two_point_net (nets:tpInputNetList) : tpTwoPointNet =
  List.flatten (List.map paring nets)
;; 
(** test scripts **)

let three_inets : tpInputNetList =
  let inet_i = [2;0;3] in
  let inet_j = [0;2] in
  let inet_k = [1;3] in
    [inet_i;inet_j;inet_k] 
;;
mnets2two_point_net three_inets;;

(* construct a weighted list of 2-point-nets. *)

(* coordinates are represented in floats in analytic placement
   because function solutions are. *)

(* edge between two gate id's. *)
type tpEdge          = int * int 
type tpWeightedEdge  = tpEdge * float
type tpWeightedGraph = tpWeightedEdge list

(* calculation of total number of edges *)
let calc_edges (inet:tpInputNet) =
  let k = List.length inet in k * (k-1)

(* make a list of weighted pairs from input net list. *)
let weighted_paring (inet:tpInputNet) : tpWeightedGraph =
  let k1 = (List.length inet) - 1 in
  let w = 1.0 /. (float_of_int k1) in
  let addw a = (a,w) in
    List.map addw (paring inet)
;;

let mnets2weighted_graph
    (nets:tpInputNetList) : tpWeightedGraph =
  List.flatten (List.map weighted_paring nets)
;;
let weighted_4_point_net = mnets2weighted_graph [[1;2;3;4]];;
(*
val weighted_4_point_net : tpWeightedGraph =
  [((4, 3), 3); ((3, 2), 3); ((4, 2), 3); ((2, 1), 3); ((3, 1), 3);
   ((4, 1), 3)]
*)
let weighted_three_nets = mnets2weighted_graph three_inets;;

(* construct a connectivity matrix C. *)

(* return  the maximum gate id of a net. *)
let max_gate_id_wnet (wnet:tpWeightedGraph) : int =
  let max_id m ((i,j),w) = max (max i j) m in
    List.fold_left max_id 0 wnet
;;
let max_id_three_nets = 
  max_gate_id_wnet weighted_three_nets;;

(* type of connectivity array. *)
type tpAary = float array array

(* assign weights to connectivity array. *)
let assign_weights 
    (wnet:tpWeightedGraph) (cm:tpAary) (width:int) =
  let rec aw wnet cm =
    match wnet with
      | ((x,y),w)::tl ->
	  if (x>=width || y>=width)
	  then 
	    let msg = sprintf "exceed C matrix width %i\n" width in
	    failwith ("assign_weights: "^msg)
	  else begin
	    cm.(x).(y) <- w;
	    cm.(y).(x) <- w;
	    aw tl cm
	  end
      | [] -> cm
  in aw wnet cm
     
let _ = Array.make_matrix 2 3 1.

(* mk_cmatrix v1.0 create an empty matrix. *)
let mk_cmatrix (wnet:tpWeightedGraph) : tpAary =
  let width = 1+(max_gate_id_wnet wnet) in
  let cm = Array.make_matrix width width 0. in
    cm

let _ = weighted_three_nets;;
let _ = mk_cmatrix weighted_three_nets;;

(* mk_cmatrix v 2.0 final version sets all c elements. *)
(* C matrix is a connectivity matrix for weights on internel connections. *)
(* make connectivity matrix from weighted graph. *)
let mk_cmatrix (wnet:tpWeightedGraph) : tpAary =
  let width = 1+(max_gate_id_wnet wnet) in
  let cm = Array.make_matrix width width 0. in
    ignore(assign_weights wnet cm width);
    cm

(* if index range between [1..n], change thenm to [0..n-1] *)
let reduce_wg_index (wnet:tpWeightedGraph) : tpWeightedGraph =
  let rec rwi wnet result =
    match wnet with
      | ((x,y),w)::tl -> rwi tl (((x-1,y-1),w)::result)
      | _ -> List.rev result
  in rwi wnet []
;;

mk_cmatrix [((0,1),1.);((1,2),4.)];;
mk_cmatrix [((1,2),1.);((2,3),4.)];;


(* A array is modified from C array by mutable updating. *)

(* return a summary of all elements in a vector. *)
let vec_sum (v:float array) : float =
  Array.fold_left (+.) 0. v

(* diagnal elements (i,i) of A is the sum of weights of port connections
   plus the sum of weights of internal connections. *)
let set_diagnal (m:tpAary) =
  let set_diagnal_i i e =
    m.(i).(i) <- vec_sum m.(i)
  in
    Array.iteri set_diagnal_i m

(* negate all elements of a vector except ith element. *)
let vec_negate_except_i (i:int) (v:float array) =
  let v_negate j e = 
    if j<>i then v.(j) <- -. e else ()
  in
    Array.iteri v_negate v
    
(* set all elements of a matrix to its negation except diagnol ones. *)
let matrix_negate_ij (m:tpAary) =
  Array.iteri vec_negate_except_i m

(** add diagnel with weights of connections with fixed pads. **)

(* add a vector of size n to diagnal of a n*n matrix. *)
let add_vec2mat_diag (m:tpAary) (p:tpAary) =
  let get_pi i = try p.(i) with _ -> [||] in
  let add_v i e = 
    m.(i).(i) <- m.(i).(i) +. (vec_sum (get_pi i))
  in
    Array.iteri add_v m

type tpBvec = float array
(* convert a n*n C matrix to A matrix with port array P of size m*k. *)
let matrix_c2a (m:tpAary) (p:tpAary)  =
  set_diagnal m;
  matrix_negate_ij m;
  add_vec2mat_diag m p

(** convert a C matrix to an A matrix by updating the input. **)

(* print a vector of floats. *)
let pr_fvec (v:float array) =
  let prf (f:float) = printf "%8.2f" f in
    Array.iter prf v

(* print a matrix of floats. *)
let pr_fmatrix (m:float array array) =
  let pr_row v = pr_fvec v; print_newline () in
    Array.iter pr_row m

(* slide 60 of weighted matrix example. *)
let wg1 : tpWeightedGraph = [((1,2),1.0);((2,3),4.0)];;
(* slide 60: [(1,5);(2,0);(3,0)] indexes reduced to 0,1,2. *)
let bx : tpWeightedGraph = [((0,0),5.)]


(* p[i] is the weight from node i to a pad, wg is connectivity graph. *)
let matrix_a_test2 (wg:tpWeightedGraph) (p:tpAary) =
  (* normalize to [0..n-1,0..n-1] *)
  let ng = reduce_wg_index wg in 
  let a = mk_cmatrix ng in
  let _ = pr_fmatrix a in
  let _ = print_endline "==========================================" in
  let _ = matrix_c2a a p in  
  let _ = pr_fmatrix a in
    ()
(*
let _ = mk_pmatrix bx
let _ = matrix_a_test2 wg1 (mk_pmatrix bx)
*)
(* ref. slide 60
    0.00    1.00    0.00
    1.00    0.00    4.00
    0.00    4.00    0.00
==========================================
    6.00   -1.00   -0.00
   -1.00    5.00   -4.00
   -0.00   -4.00    4.00
*)

(* mutabblly convert an int vector to a float vector. *)
let vec_i2f (v:int array) : float array =
  Array.map float_of_int v

(* mutablly convert an int matrix to a float matrix. *)
let matrix_i2f (a:int array array) : float array array =
  Array.map vec_i2f a

(* test if an array copy really returns a fresh new vector: succeeds. *)
let vec_copy_test c =
  let a = Array.copy c in
  let b = Array.copy c in
    a.(0) <- 1.;
    b.(0) <- 2.;
    a,b
let _ = vec_copy_test [|0.;0.|]

(* test if an array copy really returns a fresh new matrix: failed. *)
let array_copy_test c =
  let a = Array.copy c in
  let b = Array.copy c in
    a.(0).(0) <- 1.;
    b.(0).(0) <- 2.;
    a,b

(*
let _ = array_copy_test [|[|0.;0.|];[|0.;0.|]|]
*)

(* define a matrix copy. *)
let matrix_copy (m: 'a array array) : 'a array array =
  Array.map Array.copy m

let matrix_copy_test c =
  let a = matrix_copy c in
  let b = matrix_copy c in
    a.(0).(0) <- 1.;
    b.(0).(0) <- 2.;
    a,b

(*
let _ =  matrix_copy_test [|[|0.;0.|];[|0.;0.|]|] 
*)
(* C is the connectiviity matrix of internal nodes;
   p is the connectivity vector between internal
   node to a pad, assume a node connects at 
   most one pad. *)
(* given matrix C and vector p, return matrix A. *)
let matrix_A (c:tpAary) (p:tpAary) : tpAary =
  let a = matrix_copy c in
  let _ = matrix_c2a a p in
    a
    
(* In the simplest case, node-pad connection has at most one element,
   so it can be represented as a list. The following fucntion
   convert this list to tpAary. *)
let ary_of_pad_node_list (p : int list) : tpAary =
  let mk_ary a = [|float_of_int a|] in
  Array.of_list (List.map mk_ary p)

let _ = ary_of_pad_node_list [10;0;1;1;1]

(* this test implements the example in slide 63, 
   the generation of C from A and the pad weight list P. *)
let matrix_a_test3 () =
(* an example of C to A conversion from slide 63. *)
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
  let a = matrix_A c p in
    print_endline "Sample matrix C and generated A matrix";
    pr_fmatrix c;
    print_endline "==============================================";
    pr_fmatrix a;
    a

let _ = matrix_a_test3 () 


(* this test implements the example in slide 63, 
   the generation of C from A and the pad weight list P. *)
let matrix_a_test3 () =
(* an example of C to A conversion from slide 63. *)
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
(*
  let bx = [|0.;0.;1.;1.;0.5|] in
  let by = vec_i2f [|10;0;0;1;0|] in
*)
  let a = matrix_A c p in
    print_endline "Sample matrix C and the generated matrix A";
    pr_fmatrix c;
    print_endline "==============================================";
    pr_fmatrix a;
    a

(*
let _ = matrix_a_test3 () 
*)

