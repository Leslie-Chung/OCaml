(*
  Record based data structure for gate and net. 
*)
open Printf

(** supporting functions **)

(* compare two pairs. *)
let pair_compare (i1,j1) (i2,j2) : int =
  let b = Stdlib.compare i1 i2 in
  if b <> 0
  then b
  else Stdlib.compare j1 j2
let directions = [|[|0; 0|]; [|0; 1|]; [| 0; -1|]; [| 1; 0 |]; [| -1; 0 |]|] ;; (*原地 上 下 右 左*)
(* pick two different integers within the bound w. *)
let two_random_ints (w:int) : int*int =
  let i = Random.int w in
  let j = Random.int w in
  let j = 
    if i=j
    then 
      if i=w-1
      then Random.int i
      else i+1
    else j
  in  i,j 



type tpGate = {
  mutable coord  : int * int;
  mutable width  : int;
  mutable direction: int;
  mutable nets   : tpNet list
}
and tpNet = {
   mutable gates  : tpGate list
 }

let empty_gate :tpGate = { 
  coord = (0,0);
  width = 0;
  direction = 0;
  nets  = []
}

let empty_net : tpNet = {
  gates = []
}

(* convert a coordination to a gate without connecting to any net. *)
let pair2gate (((i, j), (w, dir)):(int*int) * (int*int)): tpGate =
  {
    coord = (i,j);
    width = w;
    direction = dir;
    nets  = []
  }

(* convert a list of pairs a net record. *)
let ilist2net (ilist:((int*int) * (int*int)) list) : tpNet =
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

let pair2str ((i, j), (w, dir)): string = 
  let rec pair2str' i j w dir result =
    if w < 0 then result
    else pair2str' (i+directions.(dir).(0)) (j+directions.(dir).(1)) (w-1) dir ((sprintf "(%i,%i) " i j) ^ result)
  in pair2str' i j w dir ""
(* let pair2str (i,j) = sprintf "(%i,%i) " i j *)

(** module of list form tpNets **)
(* 
   the module of list representation of nets. 
   it can be  used to construct a simple nets which can be transformed
   to tpNetAry and tpGateAry
*)
module ListNetsMod = struct

(* a cell is a pair of integers. *)
type tpGate = (int * int) * (int * int) (* x * y = cols & rows *)
(* type tpGate = int * int *)
(* the type of net is a list of cells. *)
type tpNet = tpGate list

type tpNets = tpNet list

(* convert gate to string. *)
let gate2str = pair2str

(* convert net to string. *)
let net2str (net:tpNet) : string =
  List.fold_left (^) "" (List.map gate2str net)

(* convert nets to string. *)
let nets2str (nets:tpNets) : string =
  let net2str net = (net2str net)^"\n" in
    List.fold_left (^) "" (List.map net2str nets )        

(* return  the maximum gate id of a net. *)
let max_gate_id_in_net (net:tpNet) : int =
  let max_id m ((i, j), (w, dir)) = max (max i j) m in
    List.fold_left max_id 0 net

(* let max_gate_id_in_net (net:tpNet) : int =
  let max_id m (i,j) = max (max i j) m in
    List.fold_left max_id 0 net *)

(* return the maximum gate id of a nets. *)
let max_gate_id (nets:tpNets) : int =
  let max_id m net = max m (max_gate_id_in_net net) in
    List.fold_left max_id 0 nets

(* return the list of all gates. *)
let all_gates (nets:tpNets) : tpGate list =
  List.fold_left list_union [] nets


(** calculating hpwl **)

(* split the net into the list of column numbers and row numbers. *)
let net_cols_rows (net:tpNet) : (int list) * (int list) = 
  let net_cols_rows' net = List.split net in
  let (rows_cols, widths_directions) = net_cols_rows' net in
  List.split rows_cols

(* let net_cols_rows (net:tpNet) : (int list) * (int list) =
  List.split net *)

(** HPWL v 3.0 -- eliminated List.map **)

(* type for a pair of gates for bottom-left and top-right coordinates *)
type tpGate2 = tpGate * tpGate

let modify_coord2min_or_max ((x, y): int*int) (w:int) (dir:int) (min_or_max:int) : tpGate = 
  if min_or_max = 0 then (* if get min gate *)
    if dir = 2 || dir = 4 
      (* if direction is left or down, then get the min_gate and change the direction of original gate*)
      then ((x + directions.(dir).(0) * w, y + directions.(dir).(1) * w), (w, dir - 1)) 
    else ((x, y), (w, dir))
  else
    if dir = 1 || dir = 3 (* if get min gate *)
    (* if direction is right or up *)
    then ((x + directions.(dir).(0) * w, y + directions.(dir).(1) * w), (w, dir + 1))
    else ((x, y), (w, dir))


(* return lower left and uppper right coordinates for two nets. *)
(* 对于tpGate2而言 width和direction已经无意义了，因为已经求过min和max了 *)
let add_coord_left (gate2:tpGate2)  (gate:tpGate) : tpGate2 =
  let ((x,y), (w, dir)) = gate in
  let (((min_col, min_row), (min_w, min_dir)), ((max_col,max_row), (max_w, max_dir))) = gate2 in
  let ((min_x, min_y), (min_gate_w, min_gate_dir)) = modify_coord2min_or_max (x,y) w dir 0 in 
  let ((max_x, max_y), (max_gate_w, max_gate_dir)) = modify_coord2min_or_max (x,y) w dir 1 in 

  let min_col = min min_col min_y and max_col = max max_col max_y in
  let min_row = min min_x min_row and max_row = max max_x max_row in
  (((min_col,min_row),(0,0)),((max_col,max_row),(0,0)))

(* a version of hpwl avoiding the generation of intermediate lists. *)
let hpwl (net:tpNet) =
  let init = (((max_int,max_int), (0, 0)), ((0,0), (0,0))) in
  let (((min_col, min_row), (min_w, min_dir)), ((max_col,max_row), (max_w, max_dir))) =
    List.fold_left add_coord_left init net  in
    (max_col - min_col) + (max_row - min_row)


let add_net_hpwl (hpwl_sum:int) (net:tpNet) : int =
  hpwl_sum + (hpwl net)

(* total hpwl redefined without creating intermediate list. *)
let total_hpwl (nets:tpNets) : int =
  List.fold_left add_net_hpwl 0 nets


(* generate a gate_id in [1..gate_bound] inclusive. *)
let random_gate_id (gate_bound:int): int =
   (Random.int (gate_bound-1)) + 1

(* generate a random gate. *)

let random_coord (gate_bound:int) : int*int = 
  (random_gate_id gate_bound, random_gate_id gate_bound)

let gate_legal (gate:tpGate) : bool =
  let ((x,y), (w, dir)) = gate in
  if (dir = 1 || dir = 2) && (x + directions.(dir).(0) * w) < 0 then false 
  else if (dir = 3 || dir = 4) && (y + directions.(dir).(1) * w) < 0 then false
  else true

let rec random_gate (gate_bound:int) : tpGate = 
  let random_width = (Random.int 3) in
  let random_direction = (Random.int 4) in
  let (x, y) = random_coord gate_bound in 
  let gate = ((x, y), (random_width, random_direction)) in 
  let legal = gate_legal gate in
  if legal = true then gate
  else random_gate gate_bound
;;

(* let random_gate (gate_bound:int) : tpGate = 
  (random_gate_id gate_bound, random_gate_id gate_bound)
;; *)

let rec random_ary_ave (ave:int) (length:int) (aveary:int list) : int list =
  if length>0 then
  (
    let tm = Random.float 1. in
      if tm > 0.9 then 
      let nownum = Random.int length in
        random_ary_ave ave (length-1) (nownum::aveary)
      else 
      let nownum = (Random.int (2*ave-2)) +2 in
        random_ary_ave ave (length-1) (nownum::aveary)
  )
  else aveary

let random_net (gate_bound:int) (total_gates:int) : tpNet =
  (* a net has at least 2 gates *)
  let rec mk_net n result =
    if n=0 then result else
      let new_gate = (random_gate gate_bound) in
	if List.mem new_gate result
	then mk_net (n-1) result
        else mk_net (n-1) (new_gate::result)
  in mk_net total_gates []

  
(* return a randomly generated nets, each net has at most gate_bound gates. *)
let rec random_nets (gate_bound:int) (total_nets:int): tpNets =
  let avearyl = random_ary_ave 5 total_nets [] in
  let rec mk_nets aar result =
  match aar with
  | x::ar ->
    mk_nets ar ((random_net gate_bound x) :: result)
  | [] -> result
  in mk_nets avearyl []

end

module M = ListNetsMod;;

(* print_endline "test random_nets: ";;
M.random_nets 20 4;;
print_endline "\n";; *)

(* return a gate array from a nets. *)
let gate_ary_from_nets (nets:M.tpNets) : tpGateAry =
  let gate_list = M.all_gates nets in
  let gate_list  = List.map pair2gate gate_list in
    Array.of_list gate_list

(** create a hash table from gate coordinates to gate indexes. **)

type tpGateHash = ((int * int) * (int * int), int) Hashtbl.t
(* type tpGateHash = (int*int,int) Hashtbl.t *)
(* create a hash table from gate coordinates to indexes. *)
let gate_hash (gate_ary:tpGateAry) : tpGateHash =
  let gate_without_net_list = Array.mapi (fun i x -> ((x.coord, (x.width, x.direction)),i)) gate_ary in
    Hashtbl.of_seq (List.to_seq (Array.to_list gate_without_net_list))

(* convert a net of gate coordinates to a net of gate indexes. *)
let mk_net (net:M.tpNet) (gh:tpGateHash) (gate_ary:tpGateAry) :tpNet =
  let fd_gate_id ((i,j), (w, dir)) : tpGate = 
    gate_ary.(Hashtbl.find gh ((i,j), (w, dir)))
  in
    { gates = (List.map fd_gate_id net) }

let mk_net_ary (nets:M.tpNets) (gh:tpGateHash) (gate_ary:tpGateAry) : tpNetAry =
  let mk_net net = mk_net net gh gate_ary in
    Array.of_list (List.map mk_net nets)

(* update the nets components of the gate_ary by the net. *)
let update_gates_by_net (gate_ary:tpGateAry) (net:tpNet) : unit =
  let update_gate gate = 
    gate.nets <- net::gate.nets
  in
    List.iter update_gate net.gates


(* update the nets components of the gate_ary by iterating through net array. *)
let update_nets_in_all_gates 
    (gate_ary:tpGateAry) (net_ary:tpNetAry) : unit =
  let update_net net = update_gates_by_net gate_ary net in
    Array.iter update_net net_ary

(* convert a list net to array gates and nets. *)
let mk_netlist ?(gate_ary=[||]) (nets : M.tpNets) : tpNetList =
  let gate_ary   = 
    if gate_ary = [||]
    then gate_ary_from_nets nets 
    else gate_ary
  in
  let gh         = gate_hash gate_ary in
  let net_ary    = mk_net_ary nets gh gate_ary in
    update_nets_in_all_gates gate_ary net_ary;
    gate_ary, net_ary

(** net tpNetList structure creation test **)

(* build test net *)

(* name all gates in the nets. *)
let g1 = ((3,3), (3, 1))  (* up *)
let g2 = ((2,2), (1, 2))  (* down *)
let g3 = ((1,4), (4, 3))  (* right *)
let g4 = ((4,5), (2, 4))  (* left  *)

(* swap  g1 and g2 *)

let net_i :M.tpNet = [g3;g1;g4]
let net_j :M.tpNet = [g1;g2]
let net_k :M.tpNet = [g2;g4] 
let three_nets : M.tpNets = [net_i;net_j;net_k] 
;;

three_nets;;
(* [[(1, 4); (3, 3); (4, 5)]; [(3, 3); (2, 1)]; [(2, 1); (4, 5)]] *)

let new_three_nets = mk_netlist three_nets;;

(* test by listing all gate coordinates. *)
let test_gate_listing () = 
  let gate_ary, net_ary = new_three_nets in
  let ng0 = (gate_ary.(0).coord, (gate_ary.(0).width, gate_ary.(0).direction)) in
  let ng1 = (gate_ary.(1).coord, (gate_ary.(1).width, gate_ary.(1).direction)) in
  let ng2 = (gate_ary.(2).coord, (gate_ary.(2).width, gate_ary.(2).direction)) in
  let ng3 = (gate_ary.(3).coord, (gate_ary.(3).width, gate_ary.(3).direction)) in
    [ng0;ng1;ng2;ng3];;
let test' = test_gate_listing ();;

print_endline "test_gate_listing: ";;
List.map print_string (List.map M.gate2str test');;
print_endline "\n";;
(*
          - : (int * int) * (int * int) * (int * int) * (int * int) =
((1, 4), (3, 3), (2, 1), (4, 5))
*)

(* test by listing number of connecting nets w.r.t. gates.. *)
let test_length_of_related_nets () =
  let gate_ary, net_ary = new_three_nets in
  let g0nets = gate_ary.(0).nets in
  let g1nets = gate_ary.(1).nets in
  let g2nets = gate_ary.(2).nets in
  let g3nets = gate_ary.(3).nets in
    [(List.length g0nets);
  (List.length g1nets);
  (List.length g2nets);
  (List.length g3nets)]
;;
print_endline "length of each nets: ";;
let test' = test_length_of_related_nets ();;
List.map (printf "%i ") test';;
print_endline "\n";;
(** recover net as lists of gate coordinates. **)

(* return the coordinate of a gate. *)

let gate_coord (gate:tpGate) = (gate.coord, (gate.width, gate.direction))

(* return the list of gate coordinates of a net. *)
let gate_coords_of_net (net:tpNet) : M.tpNet =
    List.map gate_coord net.gates
;;


(* return all gate coordinates of all nets. *)
let gates_of_net_ary (net_ary:tpNetAry) : M.tpNets =
  Array.to_list (Array.map gate_coords_of_net net_ary)

(* convert gate array to list of gates. *)
let gates_of_gate_ary (gate_ary:tpGateAry) : M.tpGate array =
  (Array.map gate_coord gate_ary)

(* return the gates in the gate_ary, and the gate list in all nets. *)
let recover_netlist (nets:tpNetList) : M.tpGate array * M.tpNets =
  let gate_ary,net_ary = nets in
  let mgate_ary  = gates_of_gate_ary gate_ary in
  let mnets      = gates_of_net_ary net_ary in
    mgate_ary,mnets

let recover_mnets (netlist:tpNetList) : M.tpNets =
  let _, nets = recover_netlist netlist  in nets  
;;

(* test the recover of three_nets from new_three_nets. *)
let recovered_three_nets = recover_mnets new_three_nets;;
(*   [[(1, 4); (3, 3); (4, 5)]; [(3, 3); (2, 1)]; [(2, 1); (4, 5)]] *)
three_nets;;

(* calculate half length wire length hpwl of tpNetList. *)
let total_hpwl (netlist:tpNetList) : int =
  M.total_hpwl (recover_mnets netlist)

let net_hpwl (net:tpNet) : int =
  M.hpwl (gate_coords_of_net net);;

(** convert netlist to string to show the state. **)


let gate2str (g:tpGate) = pair2str (gate_coord g)

let gates2str (gates:tpGate list) : string =
  List.fold_left (^) "" (List.map gate2str gates)

let gate_ary2str (gate_ary:tpGateAry) : string =
  gates2str (Array.to_list gate_ary)

let net2str (net:tpNet) : string = 
  sprintf "%s:%i" (gates2str net.gates) (net_hpwl net)

let net_ary2str (net_ary:tpNetAry) : string =
  let net2str net = (net2str net)^"\n" in
  let nets = Array.to_list net_ary in
  List.fold_left (^) "" (List.map net2str nets )    

(* main netlist state priting function. *)
let netlist2str (netlist:tpNetList) : string =
  let gate_ary, net_ary = netlist in
  let gate_ary_str = gate_ary2str gate_ary in
  let net_ary_str  = net_ary2str  net_ary in
  let netlist_hpwl = total_hpwl netlist in
    sprintf "gates: %s\nnets: (hpwl=%i)\n%s\n" 
      gate_ary_str netlist_hpwl net_ary_str
;;
let pr_new_three_nets ?(msg="") () =
  printf "%s%s" msg (netlist2str new_three_nets);;
pr_new_three_nets () ;;
three_nets;;
(** gate swap **)

let swap_gates (db:tpNetList) (g1:tpGate) (g2:tpGate) : unit = 
  begin
  let coord = g1.coord in
    g1.coord <- g2.coord;
    g2.coord <- coord;
  
  let width = g1.width in
    g1.width <- g2.width;
    g2.width <- width;
  
  let direction = g1.direction in
    g1.direction <- g2.direction;
    g2.direction <- direction;
  end;;

(* swap gates test *)
let swap_gates_test ?(i=0) ?(j=1) (nets:tpNetList) =
  let gate_ary,net_ary = nets in
    pr_new_three_nets ~msg:"nets before swap: " ();
  let gi = gate_ary.(i) and gj = gate_ary.(j) in
  let _ = swap_gates new_three_nets gi gj in
    pr_new_three_nets ~msg:"nets after swap: " ();;

print_endline "swap_gates_test: ";;
swap_gates_test new_three_nets;;
print_endline "\n";;



(* recover net *)
let recover_nets (nets:tpNet list) : M.tpNet list =
  List.map gate_coords_of_net nets

(* convert swap gates, delta_hpwl and db to string. *)
let spr_swap_gates_db ?(msg="") 
    (g1:tpGate) (g2:tpGate) (hpwl:int) db : string =
  let g1s = gate2str g1 and g2s = gate2str g2 in
    sprintf "%s swap gates %s %s, hpwl=%i, nets=\n%s\n"
      msg g1s g2s hpwl (netlist2str db)
  
(* print status before and after gate swap. *)
let pr_gate_swap_status ?(msg="") 
    (g1:tpGate) (g2:tpGate) (hpwl:int) db =
    print_endline 
      (spr_swap_gates_db ~msg g1 g2 hpwl db)

(** HPWL v 4.0 -- direct HPWL calculation on new nets **)

(* return lower left and uppper right coordinates for two nets. *)
let add_coord_left (gate2:M.tpGate2)  (gate:tpGate) : M.tpGate2 =
  M.add_coord_left gate2 (gate_coord gate)

(* hpwl calculation on new net,looks same as the old code, but types are different *)

let hpwl (net:tpNet) =
  let init = (((max_int,max_int), (0, 0)), ((0,0), (0,0))) in
  let (((min_col, min_row), (min_w, min_dir)), ((max_col,max_row), (max_w, max_dir))) =
    List.fold_left add_coord_left init net.gates  in
    (max_col - min_col) + (max_row - min_row)


(* add the hpwl of a net to the accumulated hpwl*)
let add_net_hpwl (hpwl_sum:int) (net:tpNet) : int =
  hpwl_sum + (hpwl net)

(* total hpwl redefined without creating intermediate list. *)
let nets_hpwl (nets:tpNet list) : int =
  List.fold_left add_net_hpwl 0 nets

(* swap gates and return the change of hpwl. *)
let swap_gates_hpwl ?(debug_on=false) 
    (db:tpNetList) (g1:tpGate) (g2:tpGate) : int =
  let n1 = g1.nets and n2 = g2.nets in
  let hpwl_before  = (nets_hpwl n1)+(nets_hpwl n2) in
    if debug_on then begin
      pr_gate_swap_status 
	~msg:"swap_gates_hpwl: before " g1 g2 hpwl_before db;
    end;
    swap_gates db g1 g2;
    let hpwl_after  = (nets_hpwl n1)+(nets_hpwl n2) in
    if debug_on then
      pr_gate_swap_status 
	~msg:"swap_gates_hpwl: after " g1 g2 hpwl_after db;
      hpwl_after - hpwl_before


(* swap gates hpwl test *)
let swap_gates_hpwl_test ?(debug_on=false) 
    ?(i=0) ?(j=1) (nets:tpNetList) =
  let gate_ary,net_ary = nets in
    pr_new_three_nets ~msg:"nets before swap: " ();
  let gi = gate_ary.(i) and gj = gate_ary.(j) in
  let delta_hpwl = swap_gates_hpwl ~debug_on new_three_nets gi gj in
    pr_new_three_nets ~msg:"nets after swap: " ();
    printf "delta hpwl = %i\n" delta_hpwl
;;

swap_gates_hpwl_test new_three_nets;;


(* a swap placer with input from M.tpNets. *)


(* print status during placer internel iterations. *)
let pr_netlist_status ?(msg="") i j delta_hpwl db =
  if msg="" then
    printf "exchange %i %i, delta=%i\nnetlist:\n%s\n" 
      i j delta_hpwl (netlist2str db)
  else 
    printf "%s %i %i, delta=%i\nnetlist:\n%s\n" 
      msg i j delta_hpwl (netlist2str db)

(* iterating for at most n times if no improvement, return 
   the final placement and the initial and final hpwls. 
   n times of initial random swaps 
   no debug code.
*)
let swap_placer_v2
    (nets:M.tpNets) (uplimit:int) : tpNetList * int * int =
  let db : tpNetList = mk_netlist nets in
  let gate_ary,net_ary = db in
  let total_gates = Array.length gate_ary in
  let initial_hpwl   = total_hpwl db in
  let rec rp db delta_hpwl n =
    if n>=uplimit
    then db,initial_hpwl, (initial_hpwl+delta_hpwl) 
    else 
      let i,j = two_random_ints total_gates in
      let g1 = gate_ary.(i) and g2 = gate_ary.(j) in
      let delta = swap_gates_hpwl  db g1 g2 in  
	begin
	  if delta>=0
	  then (* recover previous db *)
	    let _ = swap_gates_hpwl db g2 g1 in	    
	      rp db delta_hpwl (n+1)
	  else rp db (delta_hpwl+delta) (n+1)
	end
  in 
    rp db 0 0
;;


(* output the updated nets so that it can be called in calc_in_time *)


(** timing calculation and testing scripts adapted from lec04.ml **)

(* calculate the computation time of executing a function. *)
let calc_int_time (f : int -> int*'a) (n:int) : int * 'a =
  let start_time = Sys.time () in
  let hpwl_val, nets = f n in
  let end_time = Sys.time () in
  let elapsed = end_time -. start_time in
    printf "Execution of f () = %i takes %6.2f seconds\n" 
      hpwl_val elapsed;
    hpwl_val, nets


(* swap placer test v 2.0  is form command level test. *)
let test_swap_placer_on_nets swap_placer total_hpwl nets swap_times =
  let hpwl_val = total_hpwl nets in
  let swap_placer swap_times = swap_placer nets swap_times in
  let i,new_nets = calc_int_time swap_placer swap_times in
    printf "original hpwl = %i,  hpwl after %i swaps = %i\n" 
      hpwl_val swap_times i;
    printf "improvements = %i. " (hpwl_val-i);
    printf "final hpwl = %i\n" (total_hpwl new_nets);
    print_endline "\nnets after swap:";
    printf "%s" (netlist2str (mk_netlist new_nets));
;;


let placer_test_v2 (nets:M.tpNets)  swap_times =
  let swap_placer nets n =
    let new_nets,_,final_hpwl = swap_placer_v2 nets n in
      final_hpwl, (recover_mnets new_nets)
  in
    test_swap_placer_on_nets swap_placer M.total_hpwl nets swap_times



(** placement initialization **)

(* the input to the placer is a list of nets, each net is
   a list of integers, each integer is the id of a gate. *)
type tpInputNet = int list
type tpInputNetList = tpInputNet list

(* placement with automatically calculated square size. *)


let debug_on = ref false


(** command option processing **)

(* global variables for command options. *)
let cmdopt_total_gates : int ref = ref 10
let cmdopt_total_nets :int ref = ref 5
let cmdopt_swap_times : int ref = ref 200

(* global variable setting functions. *)
let set_total_gates (i:int)   =  cmdopt_total_gates := i
let set_total_nets (i:int)    =  cmdopt_total_nets  := i
let set_swap_times (i:int)    =  cmdopt_swap_times  := i
let set_debug_on ()           =  debug_on := true


let read_options () : string =
  let speclist =
    [ 
      ("-total_gates",   Arg.Int set_total_gates,"Total gates in a net");
      ("-total_nets", Arg.Int set_total_nets, "Total number of nets");
      ("-swap_times", Arg.Int set_swap_times, "Total number of gate swaps");
      ("-debug_on",   Arg.Set debug_on, "Enable debug information output");
    ]
  in
  let usage_msg = "Usage: ./placer [option] where options are:" in
  let _ = Arg.parse speclist (fun s -> ()) usage_msg in
    ""


let main () =
  let _ = read_options () in 
  let gate_bound = !cmdopt_total_gates in
  let total_nets = !cmdopt_total_nets in
  let swap_times = !cmdopt_swap_times in
  let nets = M.random_nets gate_bound total_nets in 
  let new_nets = mk_netlist nets in
	print_endline "\n--- testing swap_placer_v2 ---";
  print_endline "\nnets before swap:";
  printf "%s" (netlist2str new_nets);
	placer_test_v2 nets  swap_times
;;

main ();;
