
(* a cell is a pair of integers. *)2
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
