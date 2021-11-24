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
  (* Dada una lista l con cabeza h, devuelve una lista l' con cabeza h' igual al siguiente
     elemento a h en orden topologico de los presentes en l. El resto de elementos se situan
     en la cola sin ningun orden concreto *)   
   let rec aux1 n acc1 l1 = (*Buscar el primer número mayor que h*)
        let rec aux2 r acc2 l2 = match l2 with (*Busca el siguiente en orden topologico*)
            [] -> r::acc2 |
            h::t -> if h > n  && h < r then aux2 h (r::acc2) t
            else aux2 r (h::acc2) t
        in match l1 with
        [] -> raise (Invalid_argument "next_number") |
        h::t -> if h > n then aux2 h (n::acc1) t 
            else aux1 n (h::acc1) t
    in aux1 (List.hd l) [] (List.tl l)

let prev_number l = 
  (* Equivalente a next_number pero situa como h' al elemente previo en orden topologico*)
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
  (*Devuelve la siguiente permutacion de una lista l con una cola sin más permutaciones siguientes*)
    (List.hd c):: qsort (<=) (List.tl c)
    
let reorder_prev l =  let c = prev_number l in 
  (*Devuelve la anterior permutacion de una lista l con una cola sin más permutaciones previas*)
    (List.hd c):: qsort (>=) (List.tl c)


let rec next l =
  if descending l then raise Not_found (*Si l esta en orden decentente no tiene más permutaciones siguientes*)
  else match l with
    h::[] -> [h] |
    h1::h2::[] -> h2::[h1] |
    h1::t -> (try h1::next(t) with 
      Not_found -> reorder_next l)|
    [] -> [];;

let rec prev l = 
  if ascending l then raise Not_found  (*Si l esta en orden ascendente no tiene más permutaciones previas*)
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
(* En caso de que l sea el ultimo elemento, concatenar el resultado de aux_prev con la lista vacia**)
                    Not_found -> [];; 
