let rec greedyGiveChange till cost = match till, cost with
  | _, 0 -> []
  | [], _ -> raise (Failure "no more coins")
  | coin::till, cost -> 
    if cost>=coin then coin::(greedyGiveChange till (cost-coin))
    else greedyGiveChange till cost
;;
let tl = [50; 20; 15; 10];;
let cst = 80;;
let change = greedyGiveChange tl cst;;
let () = List.iter (fun el -> print_int el; print_string(" ")) change;;

let rec giveChangeErr till cost change current_till current_cost = match till, current_till, current_cost with
  | _, _, 0 -> change
  | coin::till, [], _ -> giveChangeErr till cost [] till cost
  | _, coin::current_till, current_cost -> 
    if current_cost>=coin then giveChangeErr till cost (coin::change) current_till (current_cost-coin)
    else giveChangeErr till cost change current_till current_cost
  | [], [], _ -> raise (Failure "no more coins")
;;
let rec giveChange till cost = match till, cost with
  | _, 0 -> [[]]
  | [], _ -> []
  | coin::till, cost ->
    if cost<coin then giveChange till cost 
    else let rec allCombinations = function
      (* This is the case where we run out of coins (returns empty change) *)
      | [] -> []
      (* Otherwise add the coin to the changes combinations found *)
      | combination::tail -> (coin::combination)::allCombinations tail
      in allCombinations (giveChange till (cost-coin)) @ giveChange till cost 
;;

let tl = [100; 70; 55; 40; 20; 15; 10];;
(* allCombinations (giveChange [70; 55; 40; 20; 15; 10] 120) @ giveChange ...*)
(* allCombinations (allCombinations (giveChange [55; 40; 20; 15; 10] 50 @ giveChange ...) @ giveChange ...*)
(* allCombinations (allCombinations (giveChange [40; 20; 15; 10] 50 @ giveChange ...) @ giveChange ...*)
(* allCombinations (allCombinations (allCombinations (giveChange [20; 15; 10] 10 @ ...) @ giveChange ...) @ giveChange ...*)
(* allCombinations (allCombinations (allCombinations (giveChange [10] 10 @ ...) @ giveChange ...) @ giveChange ...*)
(* allCombinations (allCombinations (allCombinations (allCombinations ([[]] @ ...) @ ...) @ giveChange ...) @ giveChange ...*)
(* allCombinations (allCombinations (allCombinations (allCombinations ([[], ...]) @ ...) @ giveChange ...) @ giveChange ...*)
(* allCombinations (allCombinations (allCombinations ([[10], ...]) @ giveChange ...) @ giveChange ...*)
(* [[100, 70, 40, 10], ...]*)


let cst = 220;;
let change = giveChange tl cst;;
print_newline();;
let () = List.iter (fun el -> (List.iter (fun el2 -> print_int el2; print_string " ") el); print_newline()) change;;

let rec giveChangeFaster till cost change changes = match till, cost with
  | _, 0 -> change::changes
  | [], _ -> changes
  | coin::till, cost ->
    if cost<coin then giveChangeFaster till cost change changes
    else giveChangeFaster till (cost-coin) (coin::change) (giveChangeFaster till cost change changes)
;;

(* 
[55; 12; 10; 5], 70
giveChangeFaster [55; 12; 10; 5] 70 [] []
giveChangeFaster [12; 10; 5] 15 [55] (giveChangeFaster [12; 10; 5] 70 [] [])
giveChangeFaster [12; 10; 5] 15 [55] (giveChangeFaster [10; 5] 58 [12] (giveChangeFaster [10; 5] 70 [] []))
giveChangeFaster [12; 10; 5] 15 [55] (giveChangeFaster [10; 5] 58 [12] (giveChangeFaster [5] 60 [10] [giveChangeFaster [5] 70 [] []]))
giveChangeFaster [12; 10; 5] 15 [55] (giveChangeFaster [10; 5] 58 [12] (giveChangeFaster [5] 60 [10] [giveChangeFaster [] 65 [5] [giveChangeFaster [] 70 [] []]]))
giveChangeFaster [12; 10; 5] 15 [55] (giveChangeFaster [10; 5] 58 [12] (giveChangeFaster [5] 60 [10] (giveChangeFaster [] 65 [5] [])))
giveChangeFaster [12; 10; 5] 15 [55] (giveChangeFaster [10; 5] 58 [12] (giveChangeFaster [5] 60 [10] []))
*)

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