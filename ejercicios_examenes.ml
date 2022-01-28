(*ej2 jun 2015*)

(* let rec first_match f1 f2 = function
  [] -> raise (Failure "first_match none")
  | h::t -> if f1 h = f2 h then h else first_match f1 f2 t;; *)

let rec first_match f1 f2 l = let compare x = (f1 x = f2 x) in 
    try List.find compare l with (Not_found) -> raise (Failure "first_match none");;


(*ej4 jun 2015*)

(* let rec dist = function
  h1::h2::t -> let t1,t2 = dist t in h1::t1, h2::t2
  | l -> l, [];; *)

let dist l = 
    let rec aux (acc1,acc2) = function
        h1::h2::t -> aux (h1::acc1, h2::acc2) t
     |  l -> (List.rev_append acc1 l, List.rev acc2)
    in aux ([], []) l;;

(*ej2 feb 2008*)
 let sumpro n = 
    let rec aux (s, p) n = 
         if n = 0 then (s, p)
         else aux (s+n, p*n) (n-1)
    in aux (0, 1) n;;

(*ej4 feb 2008*)

 type 'a arbolgen = Nodo of 'a * 'a arbolgen list;;
  
 let ag1 = Nodo(0, [Nodo (1, []);
                    Nodo (2, []);
                    Nodo (3, []);
                    Nodo (4, [Nodo(5, [])]);
                    Nodo (6, [])]);;

 let rec leaf = function
     Nodo(n, []) -> [n]
   | Nodo(n, l) -> List.flatten (List.map leaf l);;

(*ej3 dic 2009*)

let apariciones x l = 
    let rec aux l n = match l with
        [] -> n
      | h::t ->  aux t (if h=x then n+1 else n)
    in aux l 0;;

(*ej3 ene 2016*)

let rec last_element = function 
    [] -> raise (Invalid_argument "last_element")
  | h::[] ->h
  | h::t -> last_element t;;

(*ej4 ene 2016*)

let output_multiples x a b = 
    for i = a to b do 
        if (i mod x) = 0 then 
            Format.print_int i
    done;;

(*ej5 ene 2016*)

type 'a clist = Empty | Single of 'a | App of 'a clist * 'a clist

let rec is_empty = function
    Empty  -> true 
  | Single _ -> false 
  | App (a, b) -> (is_empty a) && (is_empty b);;

type 'a arb = R of 'a
| U of 'a * 'a arb 
|B of 'a * 'a arb * 'a arb


let anchura al =
    let rec aux acc queue = 
        if List.length queue = 0 then acc
        else 
            let x = List.hd queue in
            match x with 
                 R(x) -> aux (x::acc) queue
               | U(x, a) -> aux (x::acc) (a::queue)
               | B(x, a1, a2) -> aux (x::acc) (a1::a2::queue)
    in aux [] [al];;

let anchura arbol = 
    let rec aux = function 
        [] -> []
               | R(x)::t -> x::aux t
               | U(x, i)::t -> x::aux (t @ [i])
               | B(x, i, d)::t -> x::aux (t@[i;d])
    in aux [arbol];;
