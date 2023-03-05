let in_list = [1;2;3;4];;

let rec get_length a = 
  match a with
  hd::rest -> 1 + (get_length rest)
  | [] -> 0 
;;

let weight = 1.0 /. float_of_int ((get_length in_list) - 1);;


let rec print_list a =
  match a with
    hd::rest -> print_int hd; print_list rest
  | [] -> Printf.printf "\n" 
;;

let rec print_tuples a =
  match a with
    ((b, c), w)::rest -> Printf.printf "((%i, %i), %f);" b c w; print_tuples rest
  | [] -> Printf.printf "\n" 
;;


let rec add_tuples' first seconds result = 
  match seconds with
    hd::rest -> add_tuples' first rest (((first, hd), weight)::result)
  | [] -> result
;;

let rec add_tuples a result =   
  match a with
    hd::rest -> add_tuples rest (add_tuples' hd rest result)
  | [] -> result
;;

let res = List.rev (add_tuples in_list []);;

print_tuples(res)
