(*let <x> = <eL> in <eG>*)
(*(function <x> -> <eG>) <eL>*)

let e1 =
  let pi = 2. *. asin 1. in pi *. (pi +. 1.);;
let e1 = (function pi -> pi *. (pi +. 1.)) (2. *. asin 1.);;

let e2 =
  let lg2 = log 2. in
  let log2 = function x -> log x /. lg2
  in log2 (float (1024 * 1024));;
let e2 = (function x -> log x /. log 2.)(float (1024 * 1024));;

let e3 =
  let pi_2 = 4. *. asin 1. in
  function r -> pi_2 *. r;;
let e3 = (function r -> 4. *. asin 1. *.r);;

let e4 =
  let sqr = function x -> x *. x in
  let pi = 2. *. asin 1. in
  function r -> pi *. sqr r;;
let e4 = (function e4 ->  2. *. asin 1. *. ((function x -> x *. x) e4));;


let abs n = if n >= 0 then n else - n;;
let abs n = ((function  true -> n | false -> -n)(n>=0));;


let par n = if n mod 2 = 0 then true else false;;
let par n = (n mod 2 = 0);;

let saluda s = if s = "Hola" then print_endline "Hola!" else ();;
let saluda s = ((function "Hola" -> print_endline "Hola!" | _ -> ())s);;

let f n = if n mod 2 = 0 then "es par" else "es impar";;
let f n = ((function true -> "es par" | false -> "es impar")(n mod 2 = 0));;

let f n = if n mod 2 = 0 then "m ́ultiplo de 2"
          else if n mod 3 = 0 then "m ́ultiplo de 3"
          else "impar";;
let f n = ((function true -> "múltiplo de 2" | false -> ((function true -> "múltiplo de 3"| false -> "impar")(n mod 3 = 0)))(n mod 2 = 0))

