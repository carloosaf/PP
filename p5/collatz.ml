let f n = if n mod 2 = 0 then n / 2 else 3 * n + 1

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


  let rec length'n'top x =
    let y = if x mod 2 = 0 
      then x/2
      else 3* x +1
    in (if x = 1 then (0, 0)
        else let r = length'n'top y in ((1 + fst r), (max x (snd r))));;
