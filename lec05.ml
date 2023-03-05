(*
  Ionia version 2.0
  Ionia is an experimental layout software by OCaml
  Goal: design nets and gates
  Each gate has a unique id and is associated with a pair of coordinate,
  and a list of input nets and output nets;
  Eacy net has an unique id and is associated with a list
  of gate id's

  net list reference: slide 18, slide 24.
*)


(** array based gates and nets. **)

(* a gate has an id, a location and a list of connected nets. *)
type tpGate = {
  coord  : int * int;
  mutable nets   : tpNet list
}
and tpNet = {
   gates  : tpGate list
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


(* compare two pairs. *)
let pair_compare (i1,j1) (i2,j2) : int =
  let b = Stdlib.compare i1 i2 in
  if b <> 0
  then b
  else Stdlib.compare j1 j2

(* sort a list of pairs. *)
let pair_list_sort (pl : ('a * 'b) list) : ('a * 'b) list =
  List.sort pair_compare pl


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

end

module M = ListNetsMod
