let rec descending = function
	h1::h2::t -> h1 >= h2 && descending (h2::t) |
	_ -> true;;

let rec ascending = function
	h1::h2::t -> h1 <= h2 && ascending (h2::t) |
	_ -> true;;

let rec qsort ord = function 
    [] -> []
    | h::t -> let after, before = List.partition (ord h) t 
              in qsort ord before @ h :: qsort ord after;;


let next_number l =  
  (* Given a list l with head h, returns a list l' with the next element in topological order
   to h in the first position of that list, and the remaining elementes in the tail with
   no particular order*)   
   let rec aux1 n acc1 l1 = 
        let rec aux2 r acc2 l2 = match l2 with
            [] -> r::acc2 |
            h::t -> if h > n  && h < r then aux2 h (r::acc2) t
            else aux2 r (h::acc2) t
        in match l1 with
        [] -> raise (Invalid_argument "next_number") |
        h::t -> if h > n then aux2 h (n::acc1) t 
            else aux1 n (h::acc1) t
    in aux1 (List.hd l) [] (List.tl l)

let prev_number l = 
  (* Equivalent to next_number, but places the previos element in the head of l'*)
    let rec aux1 n acc1 l1 = 
        let rec aux2 r acc2 l2 = match l2 with
            [] -> r::acc2 |
            h::t -> if h < n  && h > r then aux2 h (r::acc2) t
            else aux2 r (h::acc2) t
        in match l1 with
        [] -> raise (Invalid_argument "prev_number") |
        h::t -> if h < n then aux2 h (n::acc1) t 
            else aux1 n (h::acc1) t
    in aux1 (List.hd l) [] (List.tl l)

let reorder_next l =  let c = next_number l in 
  (*Returns the next permutation of a list l with a tail that has no more possible permutations*)
    (List.hd c):: qsort (<=) (List.tl c)
    
let reorder_prev l =  let c = prev_number l in 
  (*Returns the previous permutation of a list l with a tail that has no more possible permutations*)
    (List.hd c):: qsort (>=) (List.tl c)


let rec next l =
  if descending l then raise Not_found
  else match l with
    h::[] -> [h] |
    h1::h2::[] -> h2::[h1] |
    h1::t -> (try h1::next(t) with 
      Not_found -> reorder_next l)|
    [] -> [];;

let rec prev l = 
  if ascending l then raise Not_found
  else match l with
    h::[] -> [h] |
    h1::h2::[] -> h2::[h1] |
    h1::t -> (try h1::prev(t) with 
      Not_found -> reorder_prev l)|
    [] -> [];;

let allperms l = 
  let rec aux_next acc=
    try  aux_next (next (List.hd acc)::acc) with 
      Not_found -> List.rev acc in
  let rec aux_prev acc = 
    try  aux_prev (prev (List.hd acc)::acc) with 
      Not_found -> acc
  in aux_prev [l] @ try let next_element = next l in aux_next [next_element] with 
(* In case l is the last element, append the result of aux_prev to an empy list *)
                    Not_found -> [];; 
