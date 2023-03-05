open Printf

type matrix = int array array

let pr_fvec (v :int array) =
  let prf (f:int) = printf "%i " f in
    Array.iter prf v

(* print a matrix of ints. *)
let pr_fmatrix (m :matrix) =
  let pr_row v = pr_fvec v; print_newline () in
    Array.iter pr_row m

let matrix_copy (m :matrix) :matrix =
  Array.map Array.copy m

let c = [|
  [| 1;2;3|];
  [| 4;5;6|];
  [| 7;8;9|];
|]

let a = matrix_copy c;;

print_endline "matrix C: ";
pr_fmatrix c;
print_endline "matrix A: ";
pr_fmatrix a;
print_endline "\n";;
a.(0).(0) <- 0;;
print_endline "After changing the first element of Matrix A: ";;
print_endline "matrix C: ";
pr_fmatrix c;
print_endline "matrix A: ";
pr_fmatrix a;