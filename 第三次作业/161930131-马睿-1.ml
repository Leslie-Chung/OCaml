let in_list = [[2;0;3];[0;2];[1;4]];;

let rec print_list a =
  match a with
    hd::rest -> print_int hd; print_list rest
  | [] -> Printf.printf "\n" 
;;

let rec print_tuples a =
  match a with
    (b, c)::rest -> Printf.printf "(%i, %i);" b c; print_tuples rest
  | [] -> Printf.printf "\n" 
;;


let rec add_tuples' first seconds result = 
  match seconds with
    hd::rest -> add_tuples' first rest ((first, hd)::result)
  | [] -> result
;;

let rec add_tuples a result =   
  match a with
    hd::rest -> add_tuples rest (add_tuples' hd rest result)
  | [] -> result
;;

let rec main a result =   
  match a with
    hd::rest -> add_tuples hd (main rest result)
  | [] -> result
;;

let res = List.rev (main in_list []);;

print_tuples(res)
