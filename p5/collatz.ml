let rec orbit x = 
  if x = 1 then print_endline "1"
  else
    (let y = if x mod 2 = 0 
    then x/2
    else 3* x +1
  in let _ = print_string ((string_of_int x) ^ ", ") in orbit y);;

let rec length x =
  let y = if x mod 2 = 0 
    then x/2
    else 3* x +1
  in (if x = 1 then 0
  else 1 + length y);;


let rec top x =
  if x = 1 then 0 
  else
    (let y = if x mod 2 = 0 
    then x/2
    else 3* x +1
  in (max x (top y)));;


  let rec length_n_top x =
    let y = if x mod 2 = 0 
      then x/2
      else 3* x +1
    in (if x = 1 then (0, 0)
        else let r = length_n_top y in ((1 + fst r), (max x (snd r))));;