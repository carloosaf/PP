let tour m n (xi,yi) (xf,yf) = 
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
    in aux [] (xi, yi);;
