
let hd = function
  [] -> raise (Failure "hd") |
  h::_ -> h;;

let tl = function
  [] -> raise (Failure "tl") |
  _::t -> t;;

let length l = 
  let rec suma_length i = function
  	[] -> i |
  	_::t -> suma_length (i+1) t 
  in suma_length 0 l;;

let rec compare_lengths l1 l2 = match (l1, l2) with
  [], [] -> 0 |
  [], _ -> -1 |
  _, [] -> 1  |
  _::t1, _::t2 -> compare_lengths t1 t2;;

let rec nth l n = 
  if n < 0 then raise (Invalid_argument "nth") 
  else match (l, n) with
    ([], _) -> raise (Failure "nth") |
    (h::_, 0) -> h |
    (_::t, _) -> nth t (n-1);;

let rec append l1 l2 = match l1 with
 [] -> l2|
 h::t -> h::(append t l2);;

let rec find f l= match l with  
  h::t -> ((function true -> h|
                  false -> find f t )(f h)) |
  [] -> raise Not_found;;

let rec for_all f l = match l with
  h::t -> f h &&  for_all f t |
  [] -> true;;

let rec exists f l = match l with
h::t -> (f h || exists f t) |
[] -> false;;

let rec mem x l = match l with
  h::t -> ((x=h) || mem x t) |
  [] -> false;;

let rev l = 
  let rec aux v = function
    [] -> v |
    h::t -> aux (h::v) t
  in aux [] l;;

let filter f l = 
  let rec aux acc f l = match l with 
    h::t -> if (f h) then aux (h::acc) f t  else aux acc f t |
    [] -> rev acc
  in aux [] f l;;

let find_all f l = 
  let rec aux acc f l = match l with 
    h::t -> if (f h) then aux (h::acc) f t  else aux acc f t |
    [] -> rev acc
  in aux [] f l;;

let partition f l =
  let rec aux tacc facc f l = match l with 
    h::t -> if (f h) then aux (h::tacc) facc f t  else aux tacc (h::facc) f t |
    [] -> (rev tacc, rev facc)
  in aux [] [] f l;;

let split l=
  let rec aux acc1 acc2 l = match l with 
    h::t -> aux (fst h::acc1) (snd h::acc2) t |
    [] -> (rev acc1,rev acc2)
  in aux [] [] l;;

let rec combine l1 l2 = match l1, l2 with
    ([], []) -> [] |
    h1::t1, h2::t2 -> (h1, h2):: combine t1 t2 |
    _,_ -> raise (invalid_arg "combine");;

let init n f =
  if n < 0 then raise (Invalid_argument "init") 
  else
    let rec aux acc i n f =
      if i < n then aux ((f i)::acc) (i+1) n f 
      else (rev acc) in
    aux [] 0 n f;; 

let rev l = 
  let rec aux v = function
    [] -> v |
    h::t -> aux (h::v) t
  in aux [] l;;

let rec rev_append l1 l2 = match l1 with 
	[]->l2 |
	h::t -> rev_append t (h::l2);;

let concat l=
  let rec aux acc l = match l with 
    h::t -> aux (rev_append h acc) t |
    [] -> rev acc 
  in aux [] l;;

let flatten l= concat l;;

let rec map f l = match l with 
  [] -> [] |
  h::t -> (f h)::map f t;;

let rec rev_map f l = 
  let rec aux v = function
    [] -> v |
    h::t -> aux (f h::v) t
  in aux [] l;;

let rec map2 f l1 l2 = match l1, l2 with 
    ([], []) -> [] | 
    (h1::t1, h2::t2) -> (f h1 h2)::map2 f t1 t2 |
    _ , _ -> raise (Invalid_argument "map2");; 

let rec fold_left f n l = match l with
  [] -> n |
  h::t -> fold_left f (f n h) t;;

let rec fold_right op l e = match l with
  [] -> e |
  h::t -> op h (fold_right op t e)
