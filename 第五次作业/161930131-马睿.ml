(* 字符串列表转字符串
（练习,要求用List.fold_left）
字符串数组转字符串
（练习,要求用Array.fold_left） *)
open Printf
let strlist2str ?(sep=',') (ilist:string list) : string =
  let add_sep s i = 
    if s="" then sprintf "%s%s" s i else sprintf "%s%c%s" s sep i 
    in
    List.fold_left add_sep "" ilist

let res = strlist2str ~sep:' ' ["It"; "is"; "a"; "test"; "of"; "string"; "list."];;
print_endline res;;

let strary2str ?(sep=',') (ilist:string array) : string =
  let add_sep s i = 
    if s="" then sprintf "%s%s" s i else sprintf "%s%c%s" s sep i 
    in
    Array.fold_left add_sep "" ilist

let res = strary2str ~sep:' ' [|"It"; "is"; "a"; "test"; "of"; "string"; "array."|];;
print_endline res;;