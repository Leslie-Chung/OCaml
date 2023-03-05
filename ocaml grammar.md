```ocaml

(* a cell is a pair of integers. *)2
type tpGate = int * int (* x * y = cols & rows *)
(*类型声明，*表示笛卡尔，是一种二元关系 *)
(* the type of net is a list of cells. *)
type tpNet = tpGate list
(*类型声明，list表示是数组 *)
(** Examples of wires **)


(* slide 1 *)
(*  A "2-point net" *)
let two_point_net : tpNet = [(1,4);(3,1)]

(* A "4-point net" *)
let four_point_net : tpNet = 
  two_point_net @ [(3,3);(4,5)]
(* @表示在two_point_net的基础上添加数据 *)
(* slide 16 - 19 *)

(** Half Perimeter Wire Length (HPWL) or Bounding Box (BBWL) **)

(* split the net into the list of column numbers and row numbers. *)
let net_cols_rows (net:tpNet) : (int list) * (int list) =
  List.split net
(* 函数声明，左面是参数列表和函数名，右边是函数实现，冒号后边是返回类型 *)
(* List.split是把cols和rows分解开来 *)

(* example: cols and rows of 2-point net *)
let two_point_net_xy = net_cols_rows two_point_net
(* 调用函数 *)
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
(*关键字in表示等号前面的变量要在后面的表达式中用到*)
let two_point_net_hpwl  = hpwl two_point_net    (* 5 = 2+3 *)
let four_point_net_hpwl = hpwl four_point_net  (* 7 = 3+4 *)

```

```ocaml
List.map hpwl nets
(*相当于[hpwl net1, hpwl net2...]*)

random_gate gate_bound :: result
(*是把gate_bound放入添加到result里*)

rec
(*递归函数的关键字*)
```

