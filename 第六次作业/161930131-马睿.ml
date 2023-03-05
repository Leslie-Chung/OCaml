(* 
eval $(opam config env)
ocamlfind ocamlopt -o 5debug.exe -package owl -linkpkg lec08.ml lec09.ml 5debug.ml
*)

(* 分割算法没有问题，
  但是不知道如何设置递归的终止条件，可能是因为这个原因 导致在调用graph_eqn_solve时报错
*)

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
    

(* 获取x方向的分割点 *)
let get_devide_x (g:tpGraph): float = 
  let nary,eary,pary = g in
  node_ary_sort_x nary;
  let x = node_ary_middle_x nary in
  x

let get_devide_y (g:tpGraph): float = 
  let nary,eary,pary = g in
  node_ary_sort_y nary;
  let y = node_ary_middle_y nary in
  y



(* 先把跨越的边全部删了，再把新的pad加进去，再把边加进去，构成子图 *)


let edge_remove (e: tpEdge) (l: tpEdge list) result : tpEdge list =
  if List.mem e l then result else e::result
(* 把跨过边界的边从原来的集合中删掉 *)
let delete_edges (elist :tpEdge list) (a_edges :tpEdge list) :tpWedges = 
  let rec delete_edges' elist' result =
    match elist' with
    | e::rest -> delete_edges' rest (edge_remove e a_edges result) 
    | [] -> result
  in Array.of_list (List.map edge2wedge (delete_edges' elist []))
;;


(* print_endline "Original Edges:";;
print_endline (edges2str eary);;
print_endline "across_edges_x: ";;
print_endline (edges2str (Array.of_list a_edges_x));;
let te = delete_edges (Array.to_list eary) a_edges_x;;
print_endline "After delete across_edges_x: ";;
print_endline (edges2str (Array.of_list te));;  *)

let v_pads2nodeary (e:tpEndPoints) (lens: int array) : tpNode array =
  Array.map2 pad2node lens e

let update_id_withlen (id:int) (len:int): int = 
  id + len
let update_ids_withlens (lens: int array): int array = 
  Array.mapi update_id_withlen lens 

let clear_edge (n:tpNode) : tpNode =
  { id = n.id; point = n.point; edges = [] }
(* 把原来图中的pad的edges清空 *)
let clear_edges (nary:tpNode array) :tpNode array = 
  Array.map clear_edge nary
(* 把新加的vpad加到原图中padary的后面 *)
let create_new_padsary (pary:tpPad array) (vpads :tpEndPoints) :tpNode array = 
  let lens = Array.make (Array.length vpads) (Array.length pary) in 
  let pad_ary = v_pads2nodeary vpads (update_ids_withlens lens) in 
  let ori_pad_ary = clear_edges pary in
  Array.append ori_pad_ary pad_ary


let find_pad_x (a_edge_x :tpEdge) (pads_x_ary :tpNode array) :int = 
  let find_pad_x' i pad_x =
    let e_y = edge_middle_y a_edge_x in
    if e_y = (snd pad_x.point) then i
    else 0 in
  let res = Array.to_list (Array.mapi find_pad_x' pads_x_ary) in
  let eax e = e <> 0 in
  List.hd (List.filter eax res)
(* 找到跨边界的边所在的两个顶点 需要连接到哪个pad上，找到对应pad的下标 *)
let rec find_pads_x (a_edges_x :tpEdge list) (pads_x_ary :tpNode array) result:int list = 
  match a_edges_x with
  | [] -> result
  | a_edge_x::rest -> find_pads_x rest pads_x_ary ((find_pad_x a_edge_x pads_x_ary) ::result)

let find_pad_y (a_edge_y :tpEdge) (pads_y_ary :tpNode array) :int = 
  let find_pad_y' i pad_y =
    let e_x = edge_middle_x a_edge_y in
    if e_x = (fst pad_y.point) then i
    else 0 in
  let res = Array.to_list (Array.mapi find_pad_y' pads_y_ary) in
  let eax e = e <> 0 in
  List.hd (List.filter eax res)

let rec find_pads_y (a_edges_y :tpEdge list) (pads_y_ary :tpNode array) result:int list = 
  match a_edges_y with
  | [] -> result
  | a_edge_y::rest -> find_pads_y rest pads_y_ary ((find_pad_y a_edge_y pads_y_ary) ::result)
  
let change_wedge_id (a_wedge :tpWedge) (pad_id :int): tpWedge = 
  let n1_id = fst (fst a_wedge) and w = snd a_wedge in 
  ((n1_id, pad_id), w)

let connect_node2vpad (a_wedges_x_ary :tpWedge array) (pads_x_ary :tpNode array) (indexs_pad_x :int array) :tpWedges = 
  let connect_node2vpad' i a_wedge_x = 
    change_wedge_id a_wedge_x pads_x_ary.(indexs_pad_x.(i)).id in
  Array.mapi connect_node2vpad' a_wedges_x_ary

  
let mk_kid_graph 
    (edges:tpWedges) (node_ary:tpNode array) (pad_ary:tpPad array) : tpGraph =
  let mk_edge we = wedge2edge we node_ary pad_ary in    
  let edge_ary = Array.map mk_edge edges in
    update_node_ary node_ary edge_ary;
    update_node_ary pad_ary edge_ary;
    node_ary, edge_ary, pad_ary

let wedge2str (we:tpWedge) : string =
  let ((a,b), w) = we in
  sprintf "((%i,%i),%6.2f)" a b w

(* convert edge list to string. *)
let wedges2str (wedges:tpWedge array) : string =
  strary2str (Array.map wedge2str wedges)
(* print_endline (graph2str five_node_graph);; *)
(* print_endline (graph2str g);; *)
(* let graph_test () =
  let solution = graph_eqn_solve g in
    print_endline "\nSolution of AX=bx and AY=by";
    pr_fpairs solution;

print_newline ();;

graph_test (); *)
let node_compare_id (n1:tpNode) (n2:tpNode) =
  compare n1.id n2.id

(* sort node array by id *)
let node_ary_sort_id (nary:tpNode array) =
  Array.sort node_compare_id nary

let pad_compare_id (p1:tpPad) (p2:tpPad) =
  compare p2.id p1.id

(* sort pad array by id *)
let pad_ary_sort_id (pary:tpPad array) =
  Array.sort pad_compare_id pary
(* 
  先求得X方向的分割点
  再求所有跨过x分割点的边的集合(收集跨边界的边)
  然后构造x分割线上的pad点(构造边界虚拟pad)
  然后将这些边的顶点与虚拟pad相连接(构造到虚拟pad的边)

  再求得y方向的分割点
  再求所有跨过y分割点的边的集合(收集跨边界的边)
  然后构造y分割线上的pad点(构造边界虚拟pad)
  然后将这些边的顶点与虚拟pad相连接(构造到虚拟pad的边)
*)
(* 分割子图的nodeary padary eary *)
let devide_node_pad_ary_x (node_pads:tpNode list) (x:float) = 
  let devide_node_pad_ary2left n = (fst n.point) <= x and devide_node_pad_ary2right n = (fst n.point) >= x in
  let left_ary = Array.of_list (List.filter devide_node_pad_ary2left node_pads) 
  and right_ary = Array.of_list (List.filter devide_node_pad_ary2right node_pads)
  in left_ary, right_ary

let devide_weary_x (wedges: tpWedge list) (x:float) (nary:tpNode array) (pary: tpPad array):tpWedges*tpWedges =  
  let devide_weary2left we = 
    let n1id = fst (fst we) and n2id = snd (fst we) in
    if n1id < 0 || n2id < 0 then (*if is pad_edge*)
      if n1id < 0 then ((fst (nary.(n2id)).point) <= x) && ((fst (pary.(-n1id)).point) <= x)  (*if first point on the left*)
      else ((fst (nary.(n1id)).point) <= x) && ((fst (pary.(-n2id)).point) <= x)
    else (fst (nary.(n1id)).point) <= x && (fst (nary.(n2id)).point) <= x
  and devide_weary2right we = 
    let n1id = fst (fst we) and n2id = snd (fst we) in
    if n1id < 0 || n2id < 0 then (*if is pad_edge*)
      if n1id < 0 then ((fst (nary.(n2id)).point) >= x) && ((fst (pary.(-n1id)).point) >= x)  (*if first point on the left*)
      else ((fst (nary.(n1id)).point) >= x) && ((fst (pary.(-n2id)).point) >= x)
    else (fst (nary.(n1id)).point) >= x && (fst (nary.(n2id)).point) >= x
  in
  let left_ary = Array.of_list (List.filter devide_weary2left wedges) 
  and right_ary = Array.of_list (List.filter devide_weary2right wedges)
  in left_ary, right_ary

let devide_node_pad_ary_y (node_pads:tpNode list) (y:float) = 
  let devide_node_pad_ary2up n = (snd n.point) >= y and devide_node_pad_ary2down n = (snd n.point) <= y in
  let up_ary = Array.of_list (List.filter devide_node_pad_ary2up node_pads) 
  and down_ary = Array.of_list (List.filter devide_node_pad_ary2down node_pads)
  in down_ary, up_ary

let devide_weary_y (wedges: tpWedge list) (y:float) (nary:tpNode array) (pary: tpPad array):tpWedges*tpWedges =  
  let devide_weary2down we = 
    let n1id = (fst (fst we)) and n2id = (snd (fst we)) in
    if n1id < 0 || n2id < 0 then (*if is pad_edge*)
      if n1id < 0 then ((snd (nary.(n2id)).point) <= y) && ((snd (pary.(-n1id)).point) <= y)  (*if first point on the left*)
      else ((snd (nary.(n1id)).point) <= y) && ((snd (pary.(-n2id)).point) <= y)
    else (snd (nary.(n1id)).point) <= y && (snd (nary.(n2id)).point) <= y
  and devide_weary2up we = 
    let n1id = (fst (fst we)) and n2id = (snd (fst we)) in
    if n1id < 0 || n2id < 0 then (*if is pad_edge*)
      if n1id < 0 then ((snd (nary.(n2id)).point) >= y) && ((snd (pary.(-n1id)).point) >= y)  (*if first point on the left*)
      else ((snd (nary.(n1id)).point) >= y) && ((snd (pary.(-n2id)).point) >= y)
    else (snd (nary.(n1id)).point) >= y && (snd (nary.(n2id)).point) >= y
  in
  let up_ary = Array.of_list (List.filter devide_weary2up wedges) 
  and down_ary = Array.of_list (List.filter devide_weary2down wedges)
  in down_ary, up_ary

let emtpy_node = {id = 0; point = (0., 0.); edges = []};;

(* 把原来的node放到一个数组里,其坐标是node.id,这个位置的node的id是原来node在其原来数组的下标 *)
let get_tmp_nodes_pads (n_pary: tpNode array) (total_len:int) :tpNode array = 
  let tmp_node_pad_ary = Array.make total_len emtpy_node in
    let get_tmp_node_pad (i:int) (n_p: tpNode) = 
      if is_pad n_p then Array.set tmp_node_pad_ary (-n_p.id) { id = if n_p.point <> (0., 0.) then -i else 0; point = n_p.point; edges = [] }
      else Array.set tmp_node_pad_ary n_p.id { id = if n_p.point <> (0., 0.) then i else 0; point = n_p.point; edges = [] }
  in Array.iteri get_tmp_node_pad n_pary;
  tmp_node_pad_ary

(* 获得更新id后的node array *)
let change_nodes_pads (tmp_node_pad_ary: tpNode array) :tpNode array = 
  let rec change_node_pad n_p_list result = 
    match n_p_list with
    | [] -> result
    | n_p::rest -> change_node_pad rest (if n_p.id <> 0 then n_p::result else result)
  in 
  let res = Array.of_list (change_node_pad (Array.to_list tmp_node_pad_ary) [emtpy_node]) in
  if (Array.length res) > 1 then 
    if is_pad res.(1) then pad_ary_sort_id res
    else node_ary_sort_id res;
  res

let change_wedges (weary: tpWedge array) (tmp_node_ary: tpNode array) (tmp_pad_ary: tpNode array) : tpWedge array=
  let change_wedge (we: tpWedge) :tpWedge = 
    let n1id = (fst (fst we)) and n2id = (snd (fst we)) in
    if (n1id < 0) && (n2id >= 0) then ((tmp_pad_ary.(-n1id).id, tmp_node_ary.(n2id).id), snd we)
    else 
      if (n1id >= 0) && (n2id < 0) then ((tmp_node_ary.(n1id).id, tmp_pad_ary.(-n2id).id), snd we)
      else ((tmp_node_ary.(n1id).id, tmp_node_ary.(n2id).id), snd we)
  in Array.map change_wedge weary


let add_empty_node n_pary =
  if n_pary.(0).id <> 0 then Array.append [|emtpy_node|] n_pary
  else n_pary

let devide_graph_x (g:tpGraph) = 
  let devide_x = get_devide_x g in
  let nary, eary, pary = g in 
  let a_edges_x :tpEdge list = across_edges_x eary devide_x in
  let vpads_x :tpPoint list = mk_vpads_x eary devide_x in
  let deleted_wedges_x_ary :tpWedges = delete_edges (Array.to_list eary) a_edges_x in
  let new_pads_x_ary = create_new_padsary pary (Array.of_list vpads_x) in
  let a_wedges_x = List.map edge2wedge a_edges_x in
  let indexs_pad_x :int array = Array.of_list (List.rev (find_pads_x a_edges_x new_pads_x_ary [])) in
  let new_wedges_x = Array.append deleted_wedges_x_ary (connect_node2vpad (Array.of_list a_wedges_x) new_pads_x_ary indexs_pad_x) in
  let cleared_nary = clear_edges nary in
  node_ary_sort_id cleared_nary;
  let left_nary', right_nary' = devide_node_pad_ary_x (Array.to_list cleared_nary) devide_x and
  left_pary', right_pary' = devide_node_pad_ary_x (Array.to_list new_pads_x_ary) devide_x and
  left_weary', right_weary' = devide_weary_x (Array.to_list new_wedges_x) devide_x cleared_nary new_pads_x_ary in 
  node_ary_sort_id left_nary'; 
  node_ary_sort_id right_nary'; 
  pad_ary_sort_id left_pary'; 
  pad_ary_sort_id right_pary';

  let right_nary' = add_empty_node right_nary' and right_pary' = add_empty_node right_pary' 
  and left_nary' = add_empty_node left_nary' and left_pary' = add_empty_node left_pary' 
  in 
  (* print_endline (nodes2str left_nary'); 
  print_endline (nodes2str left_pary'); 
  print_endline (nodes2str right_nary'); 
  print_endline (nodes2str right_pary');  *)
  let left_nary' = get_tmp_nodes_pads left_nary' (Array.length nary) 
  and right_nary' = get_tmp_nodes_pads right_nary' (Array.length nary) 
  and left_pary' = get_tmp_nodes_pads left_pary' (Array.length new_pads_x_ary) 
  and right_pary' = get_tmp_nodes_pads right_pary' (Array.length new_pads_x_ary) in

  let left_nary = change_nodes_pads left_nary' and right_nary = change_nodes_pads right_nary'
  and left_pary = change_nodes_pads left_pary' and right_pary = change_nodes_pads right_pary' in 

  let left_weary = change_wedges left_weary' left_nary' left_pary' and
  right_weary = change_wedges right_weary' right_nary' right_pary' in
  (* print_endline "left:";
  print_endline (wedges2str left_weary);
  print_endline (nodes2str left_nary);
  print_endline (nodes2str left_pary);
  print_endline "right:";
  print_endline (wedges2str right_weary);
  print_endline (nodes2str right_nary); 
  print_endline (nodes2str right_pary);  *)
  let g_x_left = mk_kid_graph left_weary left_nary left_pary and g_x_right = mk_kid_graph right_weary right_nary right_pary in
  g_x_left, g_x_right 

let devide_graph_y (g:tpGraph) = 
  let devide_y = get_devide_y g in
  let nary, eary, pary = g in
  let a_edges_y :tpEdge list = across_edges_y eary devide_y in
  let vpads_y :tpPoint list = mk_vpads_y eary devide_y in
  let deleted_wedges_y_ary :tpWedges = delete_edges (Array.to_list eary) a_edges_y in
  let new_pads_y_ary = create_new_padsary pary (Array.of_list vpads_y) in
  let a_wedges_y = List.map edge2wedge a_edges_y in
  let indexs_pad_y :int array = Array.of_list (List.rev (find_pads_y a_edges_y new_pads_y_ary [])) in
  let new_wedges_y = Array.append deleted_wedges_y_ary (connect_node2vpad (Array.of_list a_wedges_y) new_pads_y_ary indexs_pad_y) in
  let cleared_nary = clear_edges nary in
  node_ary_sort_id cleared_nary;
  let down_nary', up_nary' = devide_node_pad_ary_y (Array.to_list cleared_nary) devide_y and
  down_pary', up_pary' = devide_node_pad_ary_y (Array.to_list new_pads_y_ary) devide_y and
  down_weary', up_weary' = devide_weary_y (Array.to_list new_wedges_y) devide_y cleared_nary new_pads_y_ary in 
  node_ary_sort_id down_nary'; 
  node_ary_sort_id up_nary'; 
  pad_ary_sort_id down_pary';
  pad_ary_sort_id up_pary'; 
  let down_nary' = add_empty_node down_nary' and down_pary' = add_empty_node down_pary' 
  and up_nary' = add_empty_node up_nary' and up_pary' = add_empty_node up_pary' 
  in
  (* print_endline (nodes2str down_nary'); 
  print_endline (nodes2str down_pary'); 
  print_endline (nodes2str up_nary'); 
  print_endline (nodes2str up_pary');  *)
  let down_nary' = get_tmp_nodes_pads down_nary' (Array.length nary) 
  and up_nary' = get_tmp_nodes_pads up_nary' (Array.length nary) 
  and down_pary' = get_tmp_nodes_pads down_pary' (Array.length new_pads_y_ary) 
  and up_pary' = get_tmp_nodes_pads up_pary' (Array.length new_pads_y_ary) in

  let down_nary = change_nodes_pads down_nary' and up_nary = change_nodes_pads up_nary'
  and down_pary = change_nodes_pads down_pary' and up_pary = change_nodes_pads up_pary' in 
  (* print_endline "down:";
  print_endline (wedges2str down_weary');
  print_endline (nodes2str down_nary'); 
  print_endline (nodes2str down_pary'); 
  print_endline "up:";
  print_endline (wedges2str up_weary'); 
  print_endline (nodes2str up_nary');
  print_endline (nodes2str up_pary');  *)
  let down_weary = change_wedges down_weary' down_nary' down_pary' and
  up_weary = change_wedges up_weary' up_nary' up_pary' in
  (* print_endline "down:";
  print_endline (wedges2str down_weary);
  print_endline (nodes2str down_nary); 
  print_endline (nodes2str down_pary); 
  print_endline "up:";
  print_endline (wedges2str up_weary); 
  print_endline (nodes2str up_nary);
  print_endline (nodes2str up_pary);  *)
  let g_y_down = mk_kid_graph down_weary down_nary down_pary and g_y_up = mk_kid_graph up_weary up_nary up_pary in
  g_y_down, g_y_up 


(* let g_x_left, g_x_right = devide_graph_x five_node_graph;; *)

(* print_endline (graph2str five_node_graph);; *)
(* print_endline (graph2str g_x_left);;
print_endline (graph2str g_x_right);; *)


let rec devide_graph (g:tpGraph) (dir:bool) = 
  let nary, eary, pary = g in 
  if (Array.length eary) > 3 then begin 
    (* print_int (Array.length eary);
    print_int (Array.length nary); *)
    if dir then begin
      print_endline "X\n";
      let g_x_left, g_x_right = devide_graph_x g in
      (* print_endline (graph2str g_x_left);
      print_endline (graph2str g_x_right); *)
      devide_graph g_x_left (not dir);
      devide_graph g_x_right (not dir);
    end
    else begin
      print_endline "Y\n";
      let g_y_down, g_y_up = devide_graph_y g in
      (* print_endline (graph2str g_y_down);
      print_endline (graph2str g_y_up); *)
      devide_graph g_y_down (not dir);
      devide_graph g_y_up (not dir);
    end
  end
  else begin
    print_endline (graph2str g);
    let solution = graph_eqn_solve g in
    print_endline "\nSolution of AX=bx and AY=by";
    pr_fpairs solution;  
    (* let a,bx,by = graph2matrixA g in
    print_endline "Sample matrix C and the generated matrix A";
    pr_fmatrix a;
    print_endline "The generated bx and by";
    printf "bx=%s, by=%s\n"
      (floatary2str bx) (floatary2str by); *)
    print_endline "";
  end
;;
let main () = 
  devide_graph five_node_graph true

;;
main ()


