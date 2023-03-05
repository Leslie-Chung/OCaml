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

(* sort a list of pairs. *)
let pair_list_sort (pl : ('a * 'b) list) : ('a * 'b) list =
  List.sort pair_compare pl

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

let pair2str (i,j) = sprintf "(%i,%i) " i j

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

(** HPWL v 3.0 -- eliminated List.map **)

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
let hpwl (net:tpNet) =
  let init = ((max_int,max_int),(0,0)) in
  let ((min_col,min_row),(max_col,max_row)) =
    List.fold_left add_gate_left init net  in
    (max_col - min_col) + (max_row - min_row)

(* add the hpwl of a net to the accumulated hpwl*)
let add_net_hpwl (hpwl_sum:int) (net:tpNet) : int =
  hpwl_sum + (hpwl net)

(* total hpwl redefined without creating intermediate list. *)
let total_hpwl (nets:tpNets) : int =
  List.fold_left add_net_hpwl 0 nets


(* generate a gate_id in [1..gate_bound] inclusive. *)
let random_gate_id (gate_bound:int): int =
   (Random.int (gate_bound-1)) + 1

(* generate a random gate. *)
let random_gate (gate_bound:int) : tpGate = 
  (random_gate_id gate_bound, random_gate_id gate_bound)
;;

(*  random_gate 5;; *)

(* new random net generator are expected to generat less number of  nets for a gate. *)
let random_net (gate_bound:int) (total_gates:int) : tpNet =
  (* a net has at least 2 gates *)
  let total_nets = 2 + Random.int total_gates in
  let rec mk_net n result =
    if n=0 then result else
      let new_gate = (random_gate gate_bound) in
	if List.mem new_gate result
	then mk_net (n-1) result
        else mk_net (n-1) (new_gate::result)
  in mk_net total_nets []

(* return a randomly generated nets, each net has at most gate_bound gates. *)
let random_nets (gate_bound:int) (total_nets:int) : tpNets =
  let total_gates = total_nets * (gate_bound / 2) in
  let rec mk_nets n result =
    if n=0 then result else
      mk_nets (n-1) ((random_net gate_bound total_gates) :: result)
  in mk_nets total_nets []
;;

(* old random net generator might generate too many nets for a gate. *)
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


(** gate swap **)

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

(* return new nets and delta of wire length after swap. *)
let nets_gate_swap_wl_delta 
    (gi:tpGate) (gj:tpGate) (nets:tpNets) : tpNets * int =
  let new_nets = nets_gate_swap gi gj nets in
  let total_delta = (total_hpwl new_nets) - (total_hpwl nets) in
    new_nets, total_delta


(* randomly pick up two gates. *)
let random_two_gates (gates:tpGate list) (len:int) : tpGate * tpGate =
(*   let i = Random.int len and j = Random.int len in *)
  let i,j = two_random_ints len in
  let gi = List.nth gates i and gj = List.nth gates j in
    gi,gj


(* random swap placer v1.0 is form toplever test. *)

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


end

module M = ListNetsMod;;



(* return a gate array from a nets. *)
let gate_ary_from_nets (nets:M.tpNets) : tpGateAry =
  let gate_list = M.all_gates nets in
  let gate_list  = List.map pair2gate gate_list in
    Array.of_list gate_list

(** create a hash table from gate coordinates to gate indexes. **)

type tpGateHash = (int*int,int) Hashtbl.t

(* create a hash table from gate coordinates to indexes. *)
let gate_hash (gate_ary:tpGateAry) : tpGateHash =
  let coord_list = Array.mapi (fun i x -> (x.coord,i)) gate_ary in
    Hashtbl.of_seq (List.to_seq (Array.to_list coord_list))

(* convert a net of gate coordinates to a net of gate indexes. *)
let mk_net (net:M.tpNet) (gh:tpGateHash) (gate_ary:tpGateAry) :tpNet =
  let fd_gate_id (i,j) : tpGate = 
    gate_ary.(Hashtbl.find gh (i,j))
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
let g1 = (3,3)  (* middle *)
let g2 = (2,1)  (* bottom *)
let g3 = (1,4)  (* up left *)
let g4 = (4,5)  (* up right *)

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
  let ng0 = gate_ary.(0).coord in
  let ng1 = gate_ary.(1).coord in
  let ng2 = gate_ary.(2).coord in
  let ng3 = gate_ary.(3).coord in
    ng0,ng1,ng2,ng3;;
test_gate_listing ();;
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
    (List.length g0nets),
  (List.length g1nets),
  (List.length g2nets),
  (List.length g3nets)
;;
test_length_of_related_nets ();;

(** recover net as lists of gate coordinates. **)

(* return the coordinate of a gate. *)
let gate_coord (gate:tpGate) = gate.coord 

(* return the list of gate coordinates of a net. *)
let gate_coords_of_net (net:tpNet) : (int*int) list =
    List.map gate_coord net.gates
;;

(* test connected net lists of a few gates. *)
let test_connected_nets () =
  let gate_ary, net_ary = new_three_nets in
  let ng0 = gate_ary.(0).coord in
  let ng1 = gate_ary.(1).coord in
  let ng2 = gate_ary.(2).coord in
  let ng3 = gate_ary.(3).coord in
  let g0nets = List.map gate_coords_of_net (gate_ary.(0).nets) in
  let g1nets = List.map gate_coords_of_net (gate_ary.(1).nets) in
  let g2nets = List.map gate_coords_of_net (gate_ary.(2).nets) in
  let g3nets = List.map gate_coords_of_net (gate_ary.(3).nets) in
    (ng0,g0nets),(ng1,g1nets),(ng2,g2nets),(ng3,g3nets);;

test_connected_nets ();;


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
(*
  let mgate_list = pair_list_sort (Array.to_list mgate_ary) in
  let mnets      = M.nets_normalization mnets in
*)
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
(* pr_new_three_nets () ;; *)
three_nets;;
(** gate swap **)

let swap_gates (db:tpNetList) (g1:tpGate) (g2:tpGate) : unit =
  let coord = g1.coord in
    g1.coord <- g2.coord;
    g2.coord <- coord

(* swap gates test *)
let swap_gates_test ?(i=0) ?(j=1) (nets:tpNetList) =
  let gate_ary,net_ary = nets in
    pr_new_three_nets ~msg:"nets before swap: " ();
  let gi = gate_ary.(i) and gj = gate_ary.(j) in
  let _ = swap_gates new_three_nets gi gj in
    pr_new_three_nets ~msg:"nets after swap: " ();;

(*
swap_gates_test new_three_nets;;
*)

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
let add_gate_left (gate2:M.tpGate2)  (gate:tpGate) : M.tpGate2 =
  M.add_gate_left gate2 gate.coord

(* hpwl calculation on new net,looks same as the old code, but types are different *)
let hpwl (net:tpNet) =
  let init = ((max_int,max_int),(0,0)) in
  let ((min_col,min_row),(max_col,max_row)) =
    List.fold_left add_gate_left init net.gates  in
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
(*
swap_gates_hpwl_test new_three_nets;;
*)

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

three_nets;;
let placed_three_nets,init_hpwl, final_hpwl = 
  swap_placer_v2  three_nets 2
in
  init_hpwl, final_hpwl, recover_mnets placed_three_nets;;

(* iterating for at most n times if no improvement, return 
   the final placement and the initial and final hpwls. *)
(* m times of initial random swaps *)
let random_nets_placer ?(debug_on=false) (hw:int*int) 
    (nets:M.tpNets) (n:int) : tpNetList * int * int =
  let db : tpNetList = mk_netlist nets in
  let gate_ary,net_ary = db in
  let max_gate_id = Array.length gate_ary in
  let initial_hpwl   = total_hpwl db in
    if debug_on then
        pr_netlist_status ~msg:"random_placer: initial placement" 0 0 0 db;
  let rec rp db cur_hpwl cnt =
    (* exit when iterating for n times without improvement *)
    if cnt <= 0 
    then db,initial_hpwl, cur_hpwl 
    else 
      let i,j = two_random_ints max_gate_id in
      let g1 = gate_ary.(i) and g2 = gate_ary.(j) in
	if debug_on then pr_netlist_status i j 0 db;
      let delta_hpwl = swap_gates_hpwl ~debug_on db g1 g2 in  
	if debug_on then
	  printf "random_placer: delta_hpwl after swap = %i\n" delta_hpwl;
	begin
	  if delta_hpwl < 0 (* got improvement *)
	  then rp db (cur_hpwl+delta_hpwl) n (* reset cnt *)
	  else (* recover previous db *)
	    let _ = swap_gates_hpwl db g2 g1 in	    
	      rp db cur_hpwl (cnt-1)
	end
  in 
  let final_result = rp db initial_hpwl n in
    if debug_on then
      pr_netlist_status ~msg:"final placement" 0 0 0 db;
    final_result
;;
let placed_three_nets,init_hpwl, final_hpwl = 
  (random_nets_placer (8,8)  three_nets 10) in
  recover_mnets placed_three_nets;;


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
    printf "original hpwl = %i,  hpwl after %i swaps =%i\n" 
      hpwl_val swap_times i;
    printf "improvements = %i. " (hpwl_val-i);
    printf "final hpwl = %i\n" (total_hpwl new_nets)
;;

let placer_test_v1 (nets:M.tpNets) swap_times =
  test_swap_placer_on_nets M.swap_placer_v1 M.total_hpwl nets swap_times ;; 


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

(* increment a pair of integers (i,j) within the region h*w . *)
let pair_inc (i:int) (j:int)  (w:int) (h:int) : int*int =
  if j<h then (i,j+1) else if i<w then (i+1,0)
  else failwith "pair_inc: (i,j) exceeds the region w*h."

(* make initial placement by random arragement. *)
(* given region height h and width w, place 1..n on to it. *)
let init_gate_place_list (n:int)  (w:int) (h:int) : (int*int) list =
  if (h*w<n)
  then 
    let msg = 
      sprintf "init_gate_place_list: the region %ix%i can not place %i gates"
	w h n 
    in failwith msg
  else 
    let rec ip m i j result =
      if m<=0 then result
      else 
	let (i,j) = pair_inc i j (w-1) h in
	  ip (m-1) i j ((i,j)::result)
    in (ip n 0 0 [])

let init_gate_ary (n:int) (w:int) (h:int) : tpGateAry =
  let plist = init_gate_place_list n h w in
  let mk_gate (i,j) : tpGate = { coord = (i,j); nets = [] } in
    Array.of_list (List.map mk_gate plist)
;;
gate_ary2str (init_gate_ary 15 4 4);;

(* return the maximum integer in the list of int list. *)
let max_int_in_lists (inets:tpInputNetList) : int = 
  int_list_max (List.map int_list_max inets)

(* convert a gate id list net to gate coordinates net. *)
let nets_of_inets (gate_ary:tpGateAry) (inets : tpInputNetList): M.tpNets =
  let net_of_inet (inet : tpInputNet): M.tpNet =
    let get_gate (i:int) : M.tpGate =   gate_ary.(i).coord in
      List.map get_gate inet
  in
    try
      List.map net_of_inet inets
    with _ -> failwith "nets_of_inets: gate_ary access error"

(* convert the input list to tpNetList *)
let netlist_of_input_nets (h:int) (w:int) (inets : tpInputNetList) : tpNetList =
  let max_gate_id = max_int_in_lists inets in
  let gate_ary  = init_gate_ary (max_gate_id+1) h w in
  let mnets : M.tpNets = nets_of_inets gate_ary inets in
    mk_netlist ~gate_ary mnets

(** gate id netlist test **)

(* create an input test net. *)
let gate_id_test_nets () : tpInputNetList =
  let inet_i = [2;0;3] in
  let inet_j = [0;2] in
  let inet_k = [1;3] in
  let three_inets  = [inet_i;inet_j;inet_k] in
    three_inets
;;

(* example of input netlist *)
let gate_id_nets_test ()  =
  let inets = gate_id_test_nets () in
  let netlist = netlist_of_input_nets 3 3 inets in
    recover_netlist netlist
;;
gate_id_test_nets ();;
gate_id_nets_test ();;

(* placement with automatically calculated square size. *)

(* return the sqrt on integers. *)
let int_sqrt (n:int) : int =
  1 + int_of_float (sqrt (float_of_int n))

(** simulation aneeling **)

(*
  simulation annealing algorithm:
  t = temperature  frozen = false
  while (! frozen) {
   for (i = 1 to total_gates) {
      swap 2 raondom gi and gj and get delta_hpwl
      if delta_hpwl < 0
      then accept swap
      else {
        if  (Random.in 1.0) < exp (- delta_hpwl/t)
        then accept swap
        else undo swap
     }
*)

(** temperature control supporting variables and functions. **)
let cmdopt_temperature : float ref = ref 0.15
let cmdopt_t_update_period : int ref = ref 1000
let cmdopt_t_update_ratio : float ref = ref 0.9
(* do not accept a gate swap if hpwl exceeds the toleration value. *)
let cmdopt_delta_toleration : int ref = ref 4
let debug_on = ref false
(* run simulation annealing. *)
let cmdopt_v3_only : bool ref = ref false
let cmdopt_v4_only : bool ref = ref false
let cmdopt_v5_only : bool ref = ref false

(* return ture if the delta is acceptable under the temperature t. *)
let accept (t:float) (delta:int) : bool = 
  let a = Random.float 1.0 in 
    if !debug_on && !cmdopt_v3_only then
    printf "delta=%i, t=%6.2f, a = %6.2f, accept = %b \n" 
      delta t a (a < t); 
    (delta < !cmdopt_delta_toleration) && (a < t)

(* update temperature and the updating period count. *)
let update_temp (t:float) (cnt_t:int) : float * int =
  if cnt_t > !cmdopt_t_update_period
  then t *. !cmdopt_t_update_ratio, 0 
  else t, cnt_t+1

type tpGate2 = tpGate * tpGate

(* return two random gates. *)
let get_two_random_gates 
    (gate_ary:tpGateAry) (total_gates:int) : tpGate2 =
  let i,j = two_random_ints total_gates in
  let g1 = gate_ary.(i) and g2 = gate_ary.(j) in
    g1,g2

(* simulation annealing placer, t is the initial temperatue. *)

(* add temperature control which will be updated every t_update_times. *)
let swap_placer_v3 
    (nets:M.tpNets) (uplimit:int) : tpNetList * int * int =
  let db : tpNetList = mk_netlist nets in
  let gate_ary,net_ary = db in
  let total_gates = Array.length gate_ary in
  let initial_hpwl   = total_hpwl db in
  let rec rp db delta_hpwl n t cnt_t =
    if n>=uplimit
    then db,initial_hpwl, (initial_hpwl+delta_hpwl) 
    else 
      let t,cnt_t = update_temp t cnt_t in
      let g1,g2 = get_two_random_gates gate_ary total_gates in
      let delta = swap_gates_hpwl  db g1 g2 in  
	if (delta>=0 && (not (accept t delta)))
	then (* recover previous db *)
	  let _ = swap_gates_hpwl db g2 g1 in	    
	    rp db delta_hpwl (n+1) t cnt_t
	else rp db (delta_hpwl+delta) (n+1) t cnt_t
  in 
    rp db 0 0 !cmdopt_temperature 0
(*
  in cooling rp t db 0 0 total_gates uplimit
*)
;;

let placer_test_v3 (nets:M.tpNets)  swap_times =
  let swap_placer nets n =
    let new_nets,_,final_hpwl = swap_placer_v3 nets n in
      final_hpwl, (recover_mnets new_nets)
  in
    test_swap_placer_on_nets swap_placer M.total_hpwl nets swap_times


(** swap_placerv4: multiple random gate swap **)

let swap_placer_v4 
    (nets:M.tpNets) (uplimit:int) : tpNetList * int * int =
  let db : tpNetList = mk_netlist nets in
  let gate_ary,net_ary = db in
  let total_gates = Array.length gate_ary in
  let initial_hpwl   = total_hpwl db in
  let rec rp db delta_hpwl n t cnt_t =
    if n>=uplimit
    then db,initial_hpwl, (initial_hpwl+delta_hpwl) 
    else 
      let t,cnt_t = update_temp t cnt_t in
      let g1,g2 = get_two_random_gates gate_ary total_gates in
      let g3,g4 = get_two_random_gates gate_ary total_gates in
      let delta1 = swap_gates_hpwl  db g1 g2 in  
      let delta2 = swap_gates_hpwl  db g3 g4 in  
      let delta  = delta1 + delta2 in
	if (delta>=0 && (not (accept t delta)))
	then (* recover previous db *)
	  let _ = swap_gates_hpwl db g4 g3 in	    
	  let _ = swap_gates_hpwl db g2 g1 in	    
	    rp db delta_hpwl (n+2) t cnt_t
	else rp db (delta_hpwl+delta) (n+2) t cnt_t
  in 
    rp db 0 0 !cmdopt_temperature 0

let placer_test_v4 (nets:M.tpNets)  swap_times =
  let swap_placer nets n =
    let new_nets,_,final_hpwl = swap_placer_v4 nets n in
      final_hpwl, (recover_mnets new_nets)
  in
    test_swap_placer_on_nets swap_placer M.total_hpwl nets swap_times

(** swap_placerv5: pick up gates with longest net HPWL **)

(* the sum of an int list. *)
let int_list_sum (ilist : int list) : int =
  List.fold_left (+) 0 ilist

(* gate hpwl is the sum of all its related nets hpwl. *)
let gate_hpwl (g:tpGate) : int =
  int_list_sum (List.map hpwl g.nets)

let rcompare x y = - (compare x y)

(* get a sorted gate array (in decreasing order) by gate_hpwl. *)
let mk_sorted_gate_ary (gate_ary:tpGateAry) : tpGateAry =
  let new_gate_ary = Array.copy gate_ary in
  let rcompare g1 g2 = rcompare (gate_hpwl g1) (gate_hpwl g2) in
    Array.sort rcompare new_gate_ary;
    new_gate_ary

let cmdopt_head_gates = ref 50
let cmdopt_head_inc   = ref 50
let set_head_gates (a:int) = cmdopt_head_gates := a
let set_head_inc (a:int)   = cmdopt_head_inc   := a
(* display total number of records during debug. *)
let cmdopt_debug_cnt = ref 20
let set_debug_cnt (a:int) = cmdopt_debug_cnt := a

let total_nets (g:tpGate) = List.length g.nets

(* swap gates with longest net hpwl. *)
let swap_placer_v5
    (nets:M.tpNets) (uplimit:int) : tpNetList * int * int =
  let db : tpNetList = mk_netlist nets in
  let gate_ary,net_ary = db in
  let sorted_gate_ary = mk_sorted_gate_ary gate_ary in
  let total_gates = Array.length gate_ary in
  let initial_hpwl   = total_hpwl db in
  let rec rp db delta_hpwl n head_gates =
    if n>=uplimit
    then db,initial_hpwl, (initial_hpwl+delta_hpwl) 
    else 
      let i,j = two_random_ints head_gates in
      let g1 = sorted_gate_ary.(i) and g2 = sorted_gate_ary.(j) in
      let delta = swap_gates_hpwl  db g1 g2 in  
      let head_gates = min total_gates (head_gates + !cmdopt_head_inc) in
	begin
	  if !debug_on && !cmdopt_v5_only && n < !cmdopt_debug_cnt then
	    printf "select gates %i %i with hpwl = %i,%i total nets = %i,%i, delta = %i\n"
	      i j (gate_hpwl g1) (gate_hpwl g2) (total_nets g1) (total_nets g2)  delta;
	  if delta>=0
	  then (* recover previous db *)
	    let _ = swap_gates_hpwl db g2 g1 in	    
	      rp db delta_hpwl (n+1) head_gates
	  else rp db (delta_hpwl+delta) (n+1) head_gates
	end
  in 
  let head_gates = !cmdopt_head_gates in (* initial 10 first gate indexes *)
    rp db 0 0 head_gates
;;

let placer_test_v5 (nets:M.tpNets)  swap_times =
  let swap_placer nets n =
    let new_nets,_,final_hpwl = swap_placer_v5 nets n in
      final_hpwl, (recover_mnets new_nets)
  in
    test_swap_placer_on_nets swap_placer M.total_hpwl nets swap_times


(** command option processing **)

(* global variables for command options. *)
let cmdopt_total_gates : int ref = ref 10
let cmdopt_total_nets :int ref = ref 20
let cmdopt_swap_times : int ref = ref 20
let cmdopt_v2_only : bool ref = ref false

(* global variable setting functions. *)
let set_total_gates (i:int)   =  cmdopt_total_gates := i
let set_total_nets (i:int)    =  cmdopt_total_nets  := i
let set_swap_times (i:int)    =  cmdopt_swap_times  := i
let set_temperature (a:float) =  cmdopt_temperature := a
let set_debug_on ()           =  debug_on := true

let set_t_update_period (a:int)   = cmdopt_t_update_period := a
let set_t_update_ratio  (a:float) = cmdopt_t_update_ratio := a
let set_delta_toleration (a:int)  = cmdopt_delta_toleration := a 

let read_options () : string =
  let speclist =
    [ 
      ("-total_gates",   Arg.Int set_total_gates,"Total gates in a net");
      ("-total_nets", Arg.Int set_total_nets, "Total number of nets");
      ("-swap_times", Arg.Int set_swap_times, "Total number of gate swaps");
      ("-v2_only",    Arg.Set cmdopt_v2_only, "Run placer_v2 only");
      ("-debug_on",   Arg.Set debug_on, "Enable debug information output");
      ("-v3_only",    Arg.Set cmdopt_v3_only, "Run simulation annealing only");
      ("-temperature", Arg.Float set_temperature, "Set temperature (0.15)");
      ("-t_update_period",  Arg.Int set_t_update_period, "Set temperature update period(1000)");
      ("-t_update_ratio",   Arg.Float set_t_update_ratio, "Set temperature update ratio(0.9)"); 
      ("-delta_toleration", Arg.Int set_delta_toleration, "Set bad swap hpwl toleration number(4)");
      ("-v4_only",    Arg.Set cmdopt_v4_only, "Swap 4 gates in one time");
      ("-v5_only",    Arg.Set cmdopt_v5_only, "Swap gates with longer nets first");
      ("-head_gates", Arg.Int set_head_gates, "Pick up the first n gates for swap(50)");
      ("-head_inc",   Arg.Int set_head_inc,   "Set the value to increment the head gates(50)");
      ("-debug_cnt",  Arg.Int set_debug_cnt,  "Set total records to display in debug(20)");
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
    if !cmdopt_v5_only then begin
    print_endline "--- testing swap_placer_v5 (swap gates with longer nets)---";
    placer_test_v5 nets  swap_times;
    end 
    else
    if !cmdopt_v4_only then begin
    print_endline "--- testing swap_placer_v4 (swap 4 gates one time)---";
    placer_test_v4 nets  swap_times;
    end 
    else
    if !cmdopt_v3_only then begin
    print_endline "--- testing swap_placer_v3 (simulation annealing)---";
    placer_test_v3 nets  swap_times;
    end 
    else
      begin
	if not !cmdopt_v2_only then begin
	  print_endline "--- testing swap_placer_v1 ---";
	  placer_test_v1 nets  swap_times;
	end;
	print_endline "\n--- testing swap_placer_v2 ---";
	placer_test_v2 nets  swap_times
      end
;;

main ();;
