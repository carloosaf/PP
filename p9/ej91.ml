(*
let rec to0from n = 
  if n < 0 then []
  else n :: to0from (n - 1);;
*)


let rec to0from n = 
  let rec aux acc i = if i >= 0 then aux (i::acc) (i-1)
                      else List.rev acc
  in aux [] n;;

(*let rec fromto m n = 
  if m > n then []
  else m :: fromto (m+1) n;;*)

let rec fromto m n = 
  let rec aux acc i =
    if i >= m then aux(i::acc) (i-1)
    else acc
  in aux [] n;;

(*
let rec from1to n =
  if n < 1 then []
  else from1to (n-1) @ [n];;
*)

let rec from1to n = 
  let rec aux acc i = if i >= 1 then aux (i::acc) (i-1)
                      else acc
  in aux [] n;;

(*
let map = 
  List.map;
*)

let map f l = 
  let rec aux acc l = match l with
    [] -> List.rev acc |
    h::t -> aux ((f h)::acc) t
  in aux [] l

(*
let power x y =
  let rec innerpower x y =
    if y = 0 then 1
    else x * innerpower x (y-1)
  in
    if y >= 0 then innerpower x y
    else invalid_arg "power";;
*)

let power x y =
  let rec aux x y acc = 
    if y = 1 then acc * x
    else aux x (y-1) (acc*x)
  in if y >= 0 then aux x y 1
  else invalid_arg "power";;


(*
let incseg l =
  List.fold_right (fun x t -> x::List.map ((+) x) t) l [];;
*)

let incseg l = 
  let rec total acc l = match l with
    [] -> acc |
    h::t -> total (acc + h) t
  in let rec aux total l acc =  match l with
    [] -> acc |
    h::t -> aux (total-h) t (total-h::acc) 
  in aux (total 0 l) (List.rev l) [];;



(*

3 [2, 1, 0] [3]
1 [1, 0] [1, 3]


*)
