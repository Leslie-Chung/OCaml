(*
  Recursive Partitioning

  Input: 
  a list of floating point coordinates in a region [0..1.0, 0..1.0];
  Action:
  recursively split the region into grids such that each grid
  contains almost same number of coordinates.
  Output: 
  a grid with evenly placed coordinates
*)

(* vertically divide a region [0..h, 0..w] so that each has almost
   the same number of coordinates. *)

(* The 3rd version of tpGate and tpNet. *)

module A = Lec08

type tpAary    = A.tpAary 
type tpBvec = float array

open Printf

(** data structure for analytic placement graph. **)

(* import functions developed in lec08.ml *)
(* let matrix_i2f = Lec08.matrix_i2f *)
let list_add = A.list_add

(* a point is a coordinate of a point. *) 
type tpPoint = float * float

(* a node can be a gate or a pad. *)
type tpNode = {
  id             : int; (* node id >=0, pad id < 0 *)
  mutable point  : tpPoint;
  mutable edges  : tpEdge list;
}
(* an edge connects two nodes. *)
and tpEdge = {
  mutable endpoints : tpNode * tpNode;
  mutable weight    : float
 }

type tpPad = tpNode

(* analytic placement graph. *)
type tpGraph = (tpNode array) * (tpEdge array) * (tpPad array)

(* given a node, return the list of id's of direct connecting nodes. *)
let connected_nodes (n:tpNode) : int list =
  let id = n.id in
  let get_id result (edge:tpEdge) =
    let (a,b) = edge.endpoints in
    if id = a.id then b.id::result
    else if id = b.id then a.id::result
    else result
  in List.fold_left get_id [] n.edges

(** convert graph to string **)

(* point to string. *)
let point2str (x,y) = sprintf "(%6.2f,%6.2f)" x y

(* convert an int list to string. *)
let intlist2str ?(sep=',') (ilist:int list) : string =
  let i2s s i = 
    if s="" then string_of_int i else sprintf "%s%c%i" s sep i 
  in
    List.fold_left i2s "" ilist

let floatlist2str ?(sep=',') (alist:float list) : string =
  let i2s s i = 
    if s="" 
    then string_of_float i 
    else sprintf "%s%c%s" s sep (string_of_float i)
  in
    List.fold_left i2s "" alist

let floatary2str (alist: float array) : string =
  floatlist2str (Array.to_list alist)

let _ = floatlist2str [1.;2.;3.123]
let _ = intlist2str [1;2;3]
let _ = intlist2str ~sep:' '[1;2;3]

(* string list to string. *)
let strlist2str ?(sep=',') (slist:string list) : string =
  let s2s s s1 = 
    if s="" then s1 else (sprintf "%s%c%s" s sep s1)
  in
    List.fold_left s2s "" slist

let strary2str  ?(sep=',') (slist:string array) : string =
  let s2s s s1 = 
    if s="" then s1 else (sprintf "%s%c%s" s sep s1)
  in
    Array.fold_left s2s "" slist

(* convert edges to string. *)
let connected_nodes_str (n:tpNode) : string =
  intlist2str (connected_nodes n)

(* convert a node to string without printting connected edges *)
let node2str (n:tpNode) = 
  sprintf "{ id=%i;p=%s; c=%s }\n"
    n.id (point2str n.point) 
    (connected_nodes_str n)

(* convert a node array to string. *)
let nodes2str (nodes:tpNode array) : string =
  strary2str ~sep:' '(Array.map node2str nodes)

(* convert an edge to string. *)
let edge2str (e:tpEdge) : string =
  let (a,b) = e.endpoints in
  sprintf "((%i,%i),%6.2f)" a.id b.id e.weight

(* convert edge list to string. *)
let edges2str (edges:tpEdge array) : string =
  strary2str (Array.map edge2str edges)

(* convert a graph to a string. *)
let graph2str (g:tpGraph) : string =
  let nary,eary,pary = g in
    sprintf "%s\n%s\n%s\n" 
      (nodes2str nary) (edges2str eary) (nodes2str pary)

(** creation of sample graph. **)

(* input graph type. *)

(* a weighted edge consists of two node id's and a weight. *)
type tpWedge  = (int * int) * float
(* a weighted graph is a list of weighted edges. *)
type tpWedges = tpWedge array
(* A nod location array is an array coordinates. *)
type tpEndPoints = tpPoint array

(** build graph from weighted graph and end points. **)
let endpoint2node (id:int) (p:tpPoint) : tpNode =
    { id = id; point = p; edges = [] }

(* make nodes from endpoints. *)
let endpoints2nodeary (e:tpEndPoints) : tpNode array =
  Array.mapi endpoint2node e

let pad2node (id:int) (p:tpPoint) : tpNode =
    { id = -id; point = p; edges = [] }

(* make nodes from endpoints. *)
let pads2nodeary (e:tpEndPoints) : tpNode array =
  Array.mapi pad2node e

(* get node with id>0 and pad with id<0. *)
let get_node_pad (i:int) (nary:tpNode array) (pary:tpPad array)
    : tpNode =
  if i>=0  then nary.(i) else pary.(-i)

(* create a tpEdge element  from weighted edge. *)
let wedge2edge (we:tpWedge) 
    (nary:tpNode array) (pary:tpPad array) : tpEdge =
  let get_np i = get_node_pad i nary pary in
  let ((i,j),w) = we in
      try
	let ni = get_np i and nj = get_np j in
	  { endpoints = (ni,nj); weight = w }
      with _ -> let msg = 
	sprintf "wedge2edge i=%i,j=%i exceed array length %i\n"
	  i j (Array.length nary)
      in failwith msg
	
(* create tpEdge array from tpWedge array. *)
let wedges2edges (ws:tpWedge array) 
    (nary:tpNode array) (pary:tpPad array): tpEdge array =
  let w2e (we:tpWedge) : tpEdge = wedge2edge we nary pary in
    Array.map w2e ws

(* use edge array to update node array, nary will be updated. *)
let update_node_ary (nary:tpNode array) (eary:tpEdge array) =
  let update_node (e:tpEdge) : unit =
    let (n1,n2) = e.endpoints in
      n1.edges <- list_add e n1.edges;
      n2.edges <- list_add e n2.edges;
  in
    Array.iter update_node eary

(* build graph from weighted graph, internal and external edges. *)
let mk_graph 
    (edges:tpWedges) (e:tpEndPoints) (p:tpEndPoints) : tpGraph =
  let node_ary = endpoints2nodeary e in
  let pad_ary  = pads2nodeary p in
  let mk_edge we = wedge2edge we node_ary pad_ary in    
  let edge_ary = Array.map mk_edge edges in
    update_node_ary node_ary edge_ary;
    update_node_ary pad_ary edge_ary;
    node_ary, edge_ary, pad_ary

(** construct a tpGraph from a weighted graph. **)

(* an example from slide 63 *)

(* 1) weighted internal edges between nodes. *)    
let five_node_nets : tpWedges =
  (* weighted edges *)
  let e13 = ((1,3),10.) in
  let e12 = ((1,2),1.)  in
  let e23 = ((2,3),1.)  in
  let e24 = ((2,4),1.)  in
  let e25 = ((2,5),1.)  in
  let e34 = ((3,4),1.)  in
  let e45 = ((4,5),1.)  in
  (* edges between internal nodes and external pads, which are negative integers. *)
  let p11 = ((1,-1),10.) in (* pads are ordered from clock direction. *)
  let p42 = ((4,-2),1.) in
  let p33 = ((3,-3),1.) in
  let p54 = ((5,-4),1.) in
  let edges : tpWedges = 
    [|e13;e12;e23;e24;e25;e34;e45;
      p11;p42;p33;p54|]
  in
    edges

(* coordinates of the five point example. *)
let five_node_locs : tpEndPoints = Array.of_list
  [(0.,0.);(0.13,0.89);(0.33,0.74);(0.24,0.80);(0.50,0.76);(0.44,0.50)]

let four_pads_locs : tpEndPoints = Array.of_list
  [(0.,0.);(0.,1.);(1.,1.);(1.,0.);(0.5,0.)]

(* convert pad location list to node array. ??? *)

let five_node_graph =
  mk_graph five_node_nets five_node_locs four_pads_locs

let five_node_g = printf "\n ";
  print_endline (graph2str five_node_graph)

(* step by step test
let edges = five_node_nets
let p = four_pads_locs
let e = five_node_locs
let node_ary = endpoints2nodeary e 
let pad_ary  = pads2nodeary p 
let mk_edge we = wedge2edge we node_ary 
let edge_ary = Array.map mk_edge edges
let _ =  update_node_ary node_ary edge_ary
let _ = (mk_graph five_node_nets five_node_locs four_pads_locs)
*)

(** splite the data set. **)

(* order of nodes. *)
let node_compare_x (n1:tpNode) (n2:tpNode) =
  let x1,_ = n1.point and x2,_ = n1.point in
    compare x1 x2

let node_compare_y (n1:tpNode) (n2:tpNode) =
  let y1,_ = n1.point and y2,_ = n1.point in
    compare y1 y2

(* sort node array vetically *)
let node_ary_sort_x (nary:tpNode array) =
  Array.sort node_compare_x nary

(* sort node array horizontally *)
let node_ary_sort_y (nary:tpNode array) =
  Array.sort node_compare_y nary

(*
let five_node_graph_divide_x () =
  let five_node_graph =
    mk_graph five_node_nets five_node_locs four_pads_locs
*)

(* return the middle position that divides the set into almost euqal subsets. *)
let node_ary_middle_x (nary:tpNode array) : float =
  let half_pos = (Array.length nary) / 2 in
    if half_pos = 0 then 0. else  fst (nary.(half_pos).point )

let node_ary_middle_y (nary:tpNode array) : float =
  let half_pos = (Array.length nary) / 2 in
    if half_pos = 0 then 0. else  snd (nary.(half_pos).point )

(* return true if the edge e across the vertical line at x. *)
let endpoints_across_x (e:tpEdge) (x:float) : bool =
  let n1,n2 = e.endpoints in
  let x1 = fst n1.point and x2 = fst n1.point in
    (x > min x1 x2) && (x < max x1 x2)

let endpoints_across_y (e:tpEdge) (y:float) : bool =
  let n1,n2 = e.endpoints in
  let y1 = snd n1.point and y2 = snd n1.point in
    (y > min y1 y2) && (y < max y1 y2)

(* return the list of edges across the vertical divider. *)
let across_edges_x (eary:tpEdge array)  (x:float) : tpEdge list =
  let eax e = endpoints_across_x e x in
    List.filter eax (Array.to_list eary)

let across_edges_y (eary:tpEdge array)  (y:float) : tpEdge list =
  let eax e = endpoints_across_y e y in
    List.filter eax (Array.to_list eary)

(* return the endpoints locations of an edge. *)
let end_locations (e:tpEdge) : tpPoint * tpPoint =
  let n1,n2 = e.endpoints in
    n1.point,n1.point 

(* x positions of end points of the edge. *)
let edge_x (e:tpEdge) : float * float =
  let (x1,_),(x2,_) = end_locations e in
    x1,x2

let edge_y (e:tpEdge) : float * float =
  let (_,y1),(_,y2) = end_locations e in
    y1,y2

(* return the middle point of an edge. *)
let edge_middle (e:tpEdge) : tpPoint =
  let (x1,y1),(x2,y2) = end_locations e in
    (x1 +. x2)/. 2.,  (y1 +. y2) /. 2.

(* x position of an edge. *)
let edge_middle_x (e:tpEdge) : float =
  let (x1,_),(x2,_) = end_locations e in
    (x1 +. x2)/. 2.

(* y position of an edge. *)
let edge_middle_y (e:tpEdge) : float =
  let (_,y1),(_,y2) = end_locations e in
    (y1 +. y2) /. 2.

(* return horizontal virtual pad locations. *)
let mk_vpads_x (eary:tpEdge array) (x:float) : tpPoint list =
  let mk_pad_loc e = x, edge_middle_y e in
    List.map mk_pad_loc (across_edges_x eary x) 
     
let mk_vpads_y (eary:tpEdge array) (y:float) : tpPoint list =
  let mk_pad_loc e = (edge_middle_x e,y) in
    List.map mk_pad_loc (across_edges_y eary y) 

(** extract connectivity matrix C from graph. **)

(* extract Lec08.tpWeightedGraph to prepare for C. *)

(* convert tpEdge to tpWedge = Lec08.tpWeightedEdge *)
let edge2wedge (e:tpEdge) : tpWedge =
  let n1,n2 = e.endpoints in
    ((n1.id,n2.id),e.weight)
    
(* extract edge list from a graph. *)
let graph2edges (g:tpGraph) : tpEdge list = (* = tpWeightedEdge list *)
  let _,eary,_ = g in
    Array.to_list eary

(* return true if an endpoint is a pad (id < 0). *)
let is_pad (n:tpNode) : bool = n.id < 0
let is_node (n:tpNode) : bool = not (is_pad n)

(* return true if an edge is a connection to a pad. *)
let is_pad_edge (e:tpEdge) : bool =
  let n1,n2 = e.endpoints in
    (is_pad n1) || (is_pad n2)

(* return true if an edge is an internal edge. *)
let is_node_edge (e:tpEdge) : bool = 
  not (is_pad_edge e)

(* convert a negated id into positive id in wedge. *)
let negid2pos (((x,y),w):tpWedge) : tpWedge =
  ((abs x, abs y),w)

(* convert all negated ids to positive ids. *)
let negids2pos (wedges:tpWedge list) : tpWedge list =
  List.map negid2pos wedges

(* P matrix is constructed in the same way as C matrix. *)
let mk_cmatrix = A.mk_cmatrix
let pr_fmatrix = A.pr_fmatrix
let matrix_c2a = A.matrix_c2a
type tpWeightedGraph = A.tpWeightedGraph

(* assign weights on P matrix. *)
let assign_pweights 
    (wnet:tpWeightedGraph) (pm:tpAary) (width:int) =
  let rec aw wnet pm =
    match wnet with
      | ((x,y),w)::tl ->
	  if (x>=width || y>=width)
	  then 
	    let msg = sprintf "exceed P matrix width %i\n" width in
	    failwith ("assign_weights: "^msg)
	  else begin
	    pm.(x).(y) <- w;
(*	    pm.(y).(x) <- w;  *)
	    aw tl pm
	  end
      | [] -> pm
  in aw wnet pm

(* max x is the maximal gate id. *)
let wnet_total_nodes (wnet:tpWeightedGraph) : int =
  let max_id m ((x,y),_) = max x m in
    List.fold_left max_id 0 wnet
  
(* max y is the maximal pad id. *)
let wnet_total_pads (wnet:tpWeightedGraph) : int =
  let max_id m ((x,y),_) = max y m in
    List.fold_left max_id 0 wnet

let mk_pmatrix (wnet:tpWeightedGraph) : tpAary =
  let wnet = negids2pos wnet in
  let total_nodes = 1+(wnet_total_nodes wnet) in
  let total_pads  = 1+(wnet_total_pads  wnet) in
  let cm = Array.make_matrix total_nodes total_pads 0. in
    ignore(assign_pweights wnet cm total_nodes);
    cm


let float_list_sum (alist : float list) : float =
  List.fold_left (+.) 0. alist

(* get ith element from pary with error handling. *)
let pary_ith 
    ?(msg="pary_ith error") (i:int) (pary:tpPad array) : tpPad =
  try  pary.(abs i)
  with Invalid_argument _ ->
    let msg = sprintf "mk_bxy error: i=%i, length pary=%i\n"
      i (Array.length pary) in failwith msg

let mk_bxy (pm:tpAary) (pary:tpPad array) =
  let sum_rowi (row : float array) =
    let wi_mul_xyi i w = 
      let xi,yi = (pary_ith i pary).point in
	w *. xi, w *. yi 
    in
    let xv, yv =
     List.split (Array.to_list (Array.mapi wi_mul_xyi row))
    in
      (float_list_sum xv, float_list_sum yv)
  in
  let bx,by = List.split (Array.to_list (Array.map sum_rowi pm)) in
    Array.of_list bx, Array.of_list by

let graph2bxy (g:tpGraph) : tpBvec*tpBvec =
  let _,_,pary = g in
  let edges = graph2edges g in
  let pad_edges  = List.filter is_pad_edge edges in
  let pad_wedges = List.map edge2wedge pad_edges in
  let pm:tpAary  = mk_pmatrix pad_wedges in
  let bx,by      = mk_bxy pm pary in
    bx,by

(*
let bx,by = graph2bxy five_node_graph 

let _ = print_endline (graph2str five_node_graph)
*)

(* construct matrix A by 
   1. extract connectivity matrix C
   2. extract matrix P from a graph g. 
   3. create A from C and P
*)
let graph2matrixA (g:tpGraph) : tpAary*tpBvec*tpBvec =
  let nary,eary,pary = g in
  let edges = graph2edges g in
  let internal_edges = List.filter is_node_edge edges in
  let internal_wedges = List.map edge2wedge internal_edges in
  let pad_edges  = List.filter is_pad_edge edges in
  let pad_wedges = List.map edge2wedge pad_edges in
  let cm:tpAary  = mk_cmatrix internal_wedges in
  let pm:tpAary  = mk_pmatrix pad_wedges in
  let bx,by      = mk_bxy pm pary in
    matrix_c2a cm pm;
    cm,bx,by

let pr_fvec (a:tpBvec) = print_endline (floatary2str a)

(* test generation of A matrix from five_node_graph. *)

let mk_five_node_graph_matrix () =
  let a,bx,by = graph2matrixA five_node_graph in
    print_endline "Sample matrix C and the generated matrix A";
    pr_fmatrix a;
    print_endline "The generated bx and by";
    printf "bx=%s, by=%s\n"
      (floatary2str bx) (floatary2str by);
    a,bx,by

;;

mk_five_node_graph_matrix ();;

(*
Sample matrix C and the generated matrix A
    0.00   -0.00   -0.00   -0.00   -0.00   -0.00
   -0.00   21.00   -1.00  -10.00   -0.00   -0.00
   -0.00   -1.00    4.00   -1.00   -1.00   -1.00
   -0.00  -10.00   -1.00   13.00   -1.00   -0.00
   -0.00   -0.00   -1.00   -1.00    4.00   -1.00
   -0.00   -0.00   -1.00   -0.00   -1.00    3.00
The generated bx and by
bx=0.,0.,0.,1.,1.,0.5, by=0.,10.,0.,0.,1.,0.
- : tpAary * tpBvec * tpBvec =
([|[|0.; -0.; -0.; -0.; -0.; -0.|]; [|-0.; 21.; -1.; -10.; -0.; -0.|];
   [|-0.; -1.; 4.; -1.; -1.; -1.|]; [|-0.; -10.; -1.; 13.; -1.; -0.|];
   [|-0.; -0.; -1.; -1.; 4.; -1.|]; [|-0.; -0.; -1.; -0.; -1.; 3.|]|],
 [|0.; 0.; 0.; 1.; 1.; 0.5|], [|0.; 10.; 0.; 0.; 1.; 0.|])
*)
