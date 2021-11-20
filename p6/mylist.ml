
let hd = function
  h::_ -> h;;

let tl = function
  _::t -> t;;

let rec length = function
  [] -> 0 |
  _::tl -> 1 + length tl;; 

let rec compare_lengths l1 l2 = match (l1, l2) with
  [], [] -> 0 |
  [], _ -> -1 |
  _, [] -> 1  |
  _::t1, _::t2 -> compare_lengths t1 t2;;

let rec nth l n = match (l, n) with
 (h::_, 0) -> h |
 (_::t, _) -> nth t (n-1);;

let rec append l1 l2 = match l1 with
 h::[] -> h::l2 |
 h::t -> h::(append t l2);;

let rec rev_append l1 l2 = match l1 with 
	[]->l2 |
	h::t -> rev_append t (h::l2);;
(*-------------------------------------------------------------------*)

 (* FIND PERO DEVUELVE EL INDICE
 let rec find f l= match l with  
  h::t -> ((function true -> 0|
                    false -> 1 + find f t )(f h)) |
  [] -> 1;;*)
  
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

let rec filter f l = match l with
  h::t -> ((function true -> h::(filter f t) |
                     false -> filter f t)(f h)) |
  [] -> [];;

let rec partition f l = match l with
  h::t -> (function true -> (h::fst (partition f t), snd (partition f t)) |
                    false ->(fst (partition f t), h::snd (partition f t)))(f h) |
  [] -> ([], []);;

let rec split = function
  h::t -> (fst h::(fst (split t)), snd h::(snd (split t))) |
  [] -> ([],[]);;

let init n f =
  let rec init_rec i n f =
    if i >= n then [] 
    else f i::init_rec (i+1) n f in
  init_rec 0 n f;; 

(*[1; 2; 3]

aux 1::[] [2;3]
aux 2::[1] [3]
aux 3::[2 ; 1] [] *)        
let rev l = 
  let rec aux v = function
    [] -> v |
    h::t -> aux (h::v) t
  in aux [] l;;

let rec concat r = function
  h::t -> let rec aux lr l1 = match lr with
    [] -> l1 |
    h::[] -> h::l1 |
    h::t -> h::(aux t l1) 
  in concat (aux r h) t |
  [] -> r;;

let rec map f l = match l with 
h::[] -> [f h] |
h::t -> (f h)::map f t;;

let rec rev_map f l = 
  let rec aux v = function
    [] -> v |
    h::t -> aux (f h::v) t
  in aux [] l;;

let rec map2 f l1 l2 = match l1, l2 with 
    (h1::[], h2::[]) -> [f h1 h2] |
    (h1::t1, h2::t2) -> (f h1 h2)::map2 f t1 t2;;

let rec fold_left f n l = match l with
  h::[] -> f n h |
  h::t -> fold_left f (f n h) t

let rec fold_right op l e = match l with
  [] -> e |
  h::t -> op h (fold_right op t e)
