type 'a g_tree = Gt of 'a * 'a g_tree list;;

let rec size = function 
    Gt (_,[]) -> 1
  | Gt (r,h::t) -> size h + size (Gt (r,t));;

let rec height = function
    Gt (_,[]) -> 1 |
    Gt (r,l) -> 1 + List.fold_left max 0 (List.map (height) l);;

let leaves tree =
    let rec aux acc = function
        Gt (r, []) -> r::acc |
        Gt (_, l) -> List.concat (List.map (aux acc) l)
    in aux [] tree;;

let rec mirror = function
    Gt (r, l) -> Gt (r, List.rev (List.map mirror l));;

let rec preorder = function
        Gt (r, []) -> [r] | 
        Gt (r, l) -> r::List.concat (List.map preorder l);;

let rec postorder = function
        Gt (r, []) -> [r] | 
        Gt (r, l) -> List.concat (List.map postorder l) @ [r];;
