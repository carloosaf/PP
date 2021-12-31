let possible m n path (x, y)=
    not (List.mem (x, y) path) && x > 0 && x <= m && y > 0 && y <= n ;;

let next_jump m n path (x, y) =
    let jumps = [(x+1, y-2);(x+1, y+2);(x-1, y-2);(x-1, y+2);
                 (x+2, y-1);(x+2, y+1);(x-2, y-1);(x-2, y+1)]
    in List.filter (possible m n path) jumps

let tour m n (xi,yi) (xf,yf) = 
    let rec aux1 path (xs,ys) = 
        if (xs, ys) = (xf, yf) then List.rev ((xs, ys)::path)
        else let next_jumps = next_jump m n path (xs, ys) in 
            let rec aux2 = function
                [] -> raise (Not_found)
              | h::t -> try aux1 ((xs, ys)::path) h 
                        with Not_found -> aux2 t
            in aux2 next_jumps
    in aux1 [] (xi, yi);;


(* let tour m n (xi,yi) (xf,yf) = 
    let rec aux path (xs,ys) = 
        let possible =
            not (List.mem (xs, ys) path) && xs > 0 && xs < m && ys > 0 && ys < n 
        in if (xs,ys) = (xf, yf) then List.rev ((xs, ys)::path)
            else if possible  then try aux ((xs, ys)::path) (xs+1, ys-2) with
                      Not_found -> try aux ((xs, ys)::path) (xs+1, ys+2) with
                      Not_found -> try aux ((xs, ys)::path) (xs-1, ys-2) with
                      Not_found -> try aux ((xs, ys)::path) (xs-1, ys+2) with
                      Not_found -> try aux ((xs, ys)::path) (xs+2, ys-1) with
                      Not_found -> try aux ((xs, ys)::path) (xs+2, ys+1) with
                      Not_found -> try aux ((xs, ys)::path) (xs-2, ys-1) with
                      Not_found -> aux ((xs, ys)::path) (xs-2, ys+1)
                else raise (Not_found)
    in aux [] (xi, yi);; *)
