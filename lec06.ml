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
let mk_netlist (nets : M.tpNets) : tpNetList =
  let gate_ary   = gate_ary_from_nets nets in
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

(* swap gates and return the change of hpwl. *)
let swap_gates_hpwl ?(debug_on=false) 
    (db:tpNetList) (g1:tpGate) (g2:tpGate) : int =
  let n1 = g1.nets and n2 = g2.nets in
  let related_nets = recover_nets (list_union n1 n2) in
  let hpwl_before  = M.total_hpwl related_nets in
    if debug_on then begin
      pr_gate_swap_status 
	~msg:"swap_gates_hpwl: before " g1 g2 hpwl_before db;
    end;
    swap_gates db g1 g2;
    let changed_nets = recover_nets (list_union n1 n2) in
    let hpwl_after = M.total_hpwl changed_nets in
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

(* placement with automatically calculated square size. *)

(* return the sqrt on integers. *)
let int_sqrt (n:int) : int =
  1 + int_of_float (sqrt (float_of_int n))


(* output the updated nets so that it can be called in calc_in_time *)


(** timing calculation and testing scripts adapted from lec04.ml **)

(* calculate the computation time of executing a function. *)
let calc_int_time (f : int -> int*'a) (n:int) : int * 'a =
  let start_time = Sys.time () in
  let hpwl_val, nets = f n in
  let end_time = Sys.time () in
  let elapsed = end_time -. start_time in
    printf "Execution of f () = %i takes %6.2g seconds\n" 
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

let placer_test_v1 (nets:M.tpNets) gate_bound total_nets swap_times =
  test_swap_placer_on_nets M.swap_placer_v1 M.total_hpwl nets swap_times ;; 


let placer_test_v2 (nets:M.tpNets) gate_bound total_nets swap_times =
  let swap_placer nets n =
    let new_nets,_,final_hpwl = swap_placer_v2 nets n in
      final_hpwl, (recover_mnets new_nets)
  in
    test_swap_placer_on_nets swap_placer M.total_hpwl nets swap_times

(** command option processing **)

(* global variables for command options. *)
let cmdopt_total_gates : int ref = ref 10
let cmdopt_total_nets :int ref = ref 20
let cmdopt_swap_times : int ref = ref 20

(* global variable setting functions. *)
let set_total_gates (i:int) =
  cmdopt_total_gates := i

let set_total_nets (i:int) =
  cmdopt_total_nets := i

let set_swap_times (i:int) = 
  cmdopt_swap_times := i

let read_options () : string =
  let speclist =
    [ 
      ("-total_gates",   Arg.Int set_total_gates,"Total gates in a net");
      ("-total_nets", Arg.Int set_total_nets, "Total number of nets");
      ("-swap_times", Arg.Int set_swap_times, "Total number of gate swaps");
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
    print_endline "--- testing swap_placer_v1 ---";
    placer_test_v1 nets gate_bound total_nets swap_times;
    print_endline "\n--- testing swap_placer_v2 ---";
    placer_test_v2 nets gate_bound total_nets swap_times
;;

main ();;
