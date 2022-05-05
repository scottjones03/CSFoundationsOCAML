let rec insert x = function
  | [] -> [x]
  | y::ys -> 
    if x<=y then x::y::ys
    else y::insert x ys
;;

let rec insertSort = function
  | [] -> []
  | [x] -> [x]
  | x::xs -> insert x (insertSort xs)
;;

let rec partition left right pivot = function
  | [] -> left, right
  | x::xs ->
    if x<=pivot then partition (x::left) right pivot xs
    else partition left (x::right) pivot xs
;;
let rec quickSort = function
  | [] -> []
  | [x] -> [x]
  | pivot::xs -> 
    let left, right = partition [] [] pivot xs in
      quickSort left @ (pivot::quickSort right)
;;
let rec quickSortFaster = function
  | ([], sorted) -> sorted
  | ([x], sorted) -> x::sorted
  | pivot::xs, sorted -> let left, right = partition [] [] pivot xs in
    quickSortFaster (left, (pivot::(quickSortFaster (right, sorted)))) 
;;

let rec take i = function
  | [] -> []
  | x::xs -> 
    if i>0 then x::(take (i-1) xs)
    else []
;;
let rec drop i = function
  | [] -> []
  | x::xs -> 
    if i>0 then drop (i-1) xs
    else x::xs
;;
let rec merge = function
  | xs, [] -> xs
  | [], ys -> ys
  | x::xs, y::ys ->
    if x<=y then x::merge (xs, (y::ys))
    else y::merge ((x::xs), ys)
;;

let rec mergeSort = function
  | [] -> []
  | [x] -> [x]
  | lst -> 
    let k = (List.length lst) / 2 in
    merge ((mergeSort (take k lst)), (mergeSort (drop k lst)))
;;

let unsorted_lst = [5; 3; 8; 1; 10; 15; 4; 2];;

let rec print_list = function
  | [] -> print_newline()
  | x::xs -> 
    print_int x;
    print_string " ";
    print_list xs
;;

print_list (insertSort unsorted_lst);;
print_list (quickSort unsorted_lst);;
print_list (quickSortFaster (unsorted_lst, []));;
print_list (mergeSort unsorted_lst);;