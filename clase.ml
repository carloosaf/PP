	let rec div x y = 
	if x < y then 0, x
	else 
	let q, r = div (x-y) y in
    1 + q, r;;


(*fibonacci eficiente*)

let rec fib2 = function
	1 -> 1,0 |
	n -> let f1, f2 = fib2(n-1) in 
		f1 + f2, f1;;

let fib n =
	let rec fib2 = function
		0-> 0,1 |
		n -> let f1, f2 = fib2(n-1) in 
			f1 + f2, f1
	in fst(fib2 n);;

let rec length l = 
	if l = [] then 0
	else 1 + length (List.tl l);;

(*let rec last l =
  if List.tl = h::[] then List.hd l
	else last (List.tl l)*)


let rec append = function
	[] -> (function l -> l)  |
	h::t -> (function l -> h :: append t l);;

(*Con match*)

let rec append l1 l2 = match l1 with
	[] -> l2  |
	h::t -> h :: append t l2

(*Compare lengths*)

let rec compare_lengths l1 = function
	[] -> (function [] -> 0 |
			_ -> -1) |
	_::t1 -> (function [] -> 1 |
  _::t2 -> compare_lengths t1 t2);;

let rec compare_lengths l1 l2 = match(l1, l2) with
	[], [] -> 0  |
	[], _  -> -1 |
	_, []  -> 1  |
	_::t1, _::t2  -> compare_lengths t1 t2;;

(*Cosa recursividad*)

let rec length = function 
	[] -> 0 |
	_::t -> 1 + length t;;

let rec suma_length i = function
	[] -> i |
	_::t -> suma_length (i+1) t;; (*Recursividad terminal*)

let rec length l = suma_length 0 l;;

(*25/10*)

let rec lmax = function
	h::[] ->h |
	h1::h2::t -> lmax (max h1 h2 :: t);;

let rec rev_append l1 l2 = match l1 with (*RE*)
	[]->l2 |
	h::t -> rev_append t (h::l2);;

(*let rec tail_append l1 l2 =
	rev_append(rev l1 l2);;*)

let rec fold_left op e l = function
	[] -> e |
	h::t -> fold_left op (op e h) t;;

let rec fold_right op l e = match l with
	[] -> e |
	h::t -> op h (fold_right op t e);;

let lmax (h::t) = fold_left max h t;;

(*27/10*)


let for_all f l= List.fold_left (fun b x -> b && f x) true l;;

let rec sorted = function
	[] -> true   |
	h::[]-> true |
	h1::h2::t -> h1 <= h2 && sorted t;;

let rec sorted = function
	h1::h2::t -> h1 <= h2 && sorted t |
	_ -> true;;

let rec insert x = function
	[] -> [x] |
	h::t -> if x <= h then x::h::t
										else h :: insert x t;;

let rec i_sort = function
	[] -> [] | 
	h::t -> insert h (i_sort t);;

(*let crono f x = 
  let t Sys.time () in 
  f x ; Sys.time ()-. t;;*)


(*3/11*)

let append l1 l2 = 
  try List.append l1 l2 with
    Stack_overflow -> List.rev_append (List.rev l1) l2;;

let insert' x l =
  let rec aux front = function
    [] -> List.rev (x::front) |
    h::t -> if x <= h then List.rev_append front (x::h::t)
            else aux (h::front) t
  in aux [] l;;

let isort' l = 
  let rec aux acc = function
    [] -> acc |
    h::t -> aux (insert' h acc) t
  in aux [] l;;

let rec insert_ord ord x = function 
  [] -> [x] |
  h::t -> if ord x h then x::h::t
  else  h::insert ord ;;

let rec isort_ord ord = function
  [] -> [] |
  h::t -> insert_ord ord h (isort_ord ord t);;

(*8/11*)

let rec divide = function
  h1::h2::t -> let t1, t2 = divide t 
                 in h1::t1, h2::t2 |
  l -> l, [];;

let rec merge l1 l2 = match l1, l2 with 
  [], l | l, [] -> l |
  h1::t1, h2::t2 -> if h1 <= h2 then h1::merge t1 l2
                    else h2::l1;;

let rec m_sort l = function
  [] -> [] |
  h::[] -> h::[] |
  l -> let l1, l2 = divide l
  in merge (m_sort l1) (m_sort l2);;

let rlist n = List.init n (fun _ -> Random.int 100_000);;

(*10/11*)

let come(i1, j1) (i2, j2) =
  i1=i2 || j1 = j2 ||  abs(j2 - j1) = abs(j2 - j1);;

let rec compatible p = function
  [] -> true |
  h::t -> not (come p h) && compatible p t;;

let compatible p l= not (List.exists (come p) l);;

let reinas n =
  let rec completar path (i,j) = 
    if i > n then path
    else if j > n then (raise Not_found)
    else if compatible (i, j) path
      then try completar ((i,j)::path) (i+1, j) with 
        Not_found -> completar path (i, j+1)  
    else completar path (i, j+1)
  in completar [] (1,1);;

(*15/11*)

let div x y = 
  if y = 0 then None
  else Some(x/y);;

let hd' l = 
  try Some (List.hd l) with Failure _ -> None;;

(*17/11*)

type int_o_no =
    UnInt of int |
    NoInt ;;

let div m n = match m,n with
    UnInt x, UnInt 0 -> NoInt |
    UnInt x, UnInt y -> UnInt (x/y) |
    _ -> NoInt;;

type booleano = V | F;;

let conj b1 b2 = match b1,b2 with
    V, V -> V |
    _, F -> F;;


