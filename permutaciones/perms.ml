let rec descending = function
	h1::h2::t -> h1 >= h2 && descending (h2::t) |
	_ -> true;;

let rec ascending = function
	h1::h2::t -> h1 <= h2 && ascending (h2::t) |
	_ -> true;;

let rec divide = function
  h1::h2::t -> let t1, t2 = divide t 
                 in h1::t1, h2::t2 |

  l -> l, [];;
let rec merge f l1 l2 = match l1, l2 with 
  [], l | l, [] -> l |
  h1::t1, h2::t2 -> if (f h1 h2) then h1::merge f t1 l2
                    else h2::l1;;

let rec m_sort f  = function (*No tail recursive*)
  [] -> [] |
  h::[] -> h::[] |
  l -> let l1, l2 = divide l
  in merge f (m_sort f l1) (m_sort f l2);;

let rec max_in_list l = List.fold_left max (List.hd l) l;;
let rec min_in_list l = List.fold_left min (List.hd l) l;;

let next_greater n l =
  let rec aux l r = match l with 
    [] -> r |
    h::t -> if h > n && h < r then aux t h
    else aux t r 
  in aux l (max_in_list l);;

let next_lesser n l =
  let rec aux l r = match l with 
    [] -> r |
    h::t -> if h < n && h > r then aux t h
    else aux t r 
  in aux l (min_in_list l);;

let rec delete_n n l = (*No tail recursive*)
  let rec aux acc l = match l with 
    [] -> acc |
    h::t -> if h != n then aux (h::acc) t
    else acc@t
  in aux [] l;;

let rec reorder_next l = let next =  next_greater (List.hd l) l  in 
next::(m_sort (<=) (delete_n next l));;

let rec reorder_prev l = let next =  next_lesser (List.hd l) l  in 
next::(m_sort (>=) (delete_n next l));;

let rec next l =
  if descending l then raise Not_found
  else match l with
    h1::h2::[] -> h2::[h1] |
    h1::t -> (try h1::next(t) with 
      Not_found -> reorder_next l)|
    [] -> [];;

let rec prev l = 
  if ascending l then raise Not_found
  else match l with
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
  in aux_prev [l] @ aux_next [next l];;

(*

[2, 1, 3, 4]


h>n && n h<r

n=2 r=l [] [1;3;4]
n=2 r=1 [1] [3;4]
n=2 r=3 [1,2,4] [;]

n=2 r=l [] [1;3;4]
n=2 r=1 [1] [3;4]
n=2 r=3 [1,2,4] [;]

----------------------------------------

 [2,1,3,4]

 foo1
 2 [] [1,3,4]
 2 [1] [3,4]
 2 [1] [3,4]

 foo2 3 [1;2] [4]
 3 [1;2;4]


PROBAR A PONER R EN LA PRIMERA POSICION
 *)


let combi l = 
    let rec foo1 n acc1 l1 = 
        let rec foo2 r acc2 l2 = match l2 with
            [] -> r::acc2 |
            h::t -> if h > n  && h < r then foo2 h (r::acc2) t
            else foo2 r (h::acc2) t
        in match l1 with
        [] -> raise Not_found |
        h::t -> if h > n then foo2 h (n::acc1) t 
            else foo1 n (h::acc1) t
    in foo1 (List.hd l) [] (List.tl l)
        




let rec combi n l =
    let rec aux acc l r = match l with
        [] -> (acc, r) |
        h::t -> if h > n && h < r then aux (r::acc) t h 
        else aux (h::acc) t r
    in match l with
    h::t-> if h>n then (aux [] t h)
    else let c = (combi n l) in ((h::fst c), snd c );;
