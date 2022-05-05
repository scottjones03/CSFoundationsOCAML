type vehicle =
  Car of int
  | Bike of bool
  | Plane
;;


exception Change;;
let rec giveChangeExceptions till cost = match till, cost with
  | _, 0 -> []
  | [], _ -> raise Change
  | coin::till, cost ->
    if cost<0 then raise Change
    else try
      coin::giveChangeExceptions till (cost-coin)
    with
      | Change -> giveChangeExceptions till cost
;;

let cst = 220;;
let tl = [150;65;20;20;20;10];;
let change = giveChangeExceptions tl cst;;
print_newline();;
let () = List.iter (fun el -> print_int el; print_string " ") change;;

type 'a tree =
  Leaf
  | Branch of 'a * 'a tree * 'a tree
;;

let rec count = function
  | Leaf -> 0
  | Branch (h, tr1, tr2) -> 1 + count tr1 + count tr2
;;

let rec depth = function
  | Leaf -> 1
  | Branch (h, tr1, tr2) -> 1 + max (depth tr1) (depth tr2)
;;

let rec leaves = function
  | Leaf -> 1
  | Branch (h, tr1, tr2) -> leaves tr1 + leaves tr2
;;

let tr0 = Branch (1, Branch (2, Branch (3, Leaf, Leaf), Branch (4, Leaf, Leaf)), Branch (5, Leaf, Leaf))