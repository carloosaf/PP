();;
(*- : unit = ()*)
2 + 5 * 3;;
(*- : int = 17*)
1.0;;
(*- : float = 1.*)
(*1.0 * 2;;*)
(*Error: This expression has type float but an expression was expected of type
         int : *)
(*Da este error porque se intenta multiplicar un float usando el operador de ints * *)
(*Correccion: 1 * 2;;*)
(*2 - 2.0;;*)
(*Error: This expression has type float but an expression was expected of type
         int*)
(*Da error de tipo porque se esta restando un float a un int con el operador resta de ints *)
(*Correccion: 2 - 2;;*)
(*3.0 + 2.0;;*)
(*Error: This expression has type float but an expression was expected of type
         int*)
(*Da error de tipo porque se estan sumando dos float con la suma de ints*)
(*Correccion: 3.0 +. 2.0;;*)
5 / 3;;
(*- : int = 1*)
5 mod 3;;
(*- : int = 2*)
3.0 *. 2.0 ** 3.0;;
(*- : float = 24*)
3.0 = float_of_int 3;;
(*- : bool = true*)
(*sqrt 4;;*)
(*Error: This expression has type int but an expression was expected of type
         float*)
(*Da error porque se le pasa un valor int a una funcion float -> float*)
int_of_float 2.1 + int_of_float (-2.9);;
(*- : int = 0*)
truncate 2.1 + truncate (-2.9);;
(*- : int = 0*)
floor 2.1 +. floor (-2.9);;
(*- : float= -1*)
(*ceil 2.1 +. ceil -2.9;;*)
(*- : Error: This expression has type float -> float
       but an expression was expected of type float*)
(*Da error porque se crea una ambiguedad entre el - operador de resta *)
(*  y el - que indica que un número es negativo *)
(*Correccion: ceil 2.1 +. ceil (-2.9);;*)
2.0 ** 3.0 ** 2.0;;
(*- : float = 521.*)
'B';;
(*- : char = 'B'*)
int_of_char 'A';;
(*- : int = 65*)
char_of_int 66;;
(*- : char = 'B'*)
Char.code 'B';;
(*- : int = 66*)
Char.chr 67;;
(*- : char = 'C*)
'\067';;
(*- : char = 'C*)
Char.chr (Char.code 'a' - Char.code 'A' + Char.code 'M');;
(*- : char = 'm'*)
Char.uppercase 'm';;
(*- : char = 'M*)
Char.lowercase 'O';;
(*- : char = 'O'*)
"this is a string";;
(*- : string = "this is a string"*)
String.length "longitud";;
(*- : int = 8*)
(*"1999" + "1";;*)
(*Error: This expression has type string but an expression was expected of type
         int*)
(*Da error de tipo porque se estan intando sumar dos string con el operador de suma de ints*)
"1999" ^ "1";;
(*- : string = "19991"*)
int_of_string "1999" + 1;;
(*- : int = "2000"*)
"\064\065";;
(*- : string = "@a"*)
string_of_int 010;;
(*- : string = "10"*)
not true;;
(*- : bool = false*)
true && false;;
(*- : bool = false*)
true || false;;
(*- : bool = true*)
(1 < 2) = false;;
(*- : bool = false*)
"1" < "2";;
(*- : bool = false*)
2<12;;
(*- : bool = true*)
"2"<"12";;
(*- : bool = false*)
"uno" < "dos";;
(*- : bool = false*)
if 3 = 4 then 0 else 4;;
(*- : int = 4*)
if 3 = 4 then "0" else "4";;
(*- : string = "4"*)
(*if 3 = 4 then 0 else "4";;*)
(*Error: This expression has type string but an expression was expected of type
         int*)
(*Da error de tipo poruqe se esperaba un int en el else*)
(if 3 < 5 then 8 else 10) + 4;; 
(*- : int = 12*)
2.0 *. asin 1.0;;
(*- : float = 3.14159265358979312*)
sin (2.0 *. asin 1.0 /. 2.);;
(*- : float = 1.*)
function x -> 2 * x;;
(*- : int -> int = func*)
(function x -> 2 * x)(2 + 1);;
(*- : int = 6*)
let x = 1;;
(*val x : int = 1*)
let y = 2;;
(*val y : int = 2*)
x-y;;
(*- : int = -1*)
let x = y in x - y;;
(*- : int = 0*)
x - y;;
(*- : int = -1*)
(*z;;*)
(*Error: Unbound value z*)
(*Se da el error porque z no esta definida con ningun valor*)
let z = x + y;;
(*val z : int = 3*)
z;;
(*- : int = 3*)
let x = 5;;
(*val x : int = 5*)
z;;
(*- : int = 3*)
let y = 5 in x * y;;
(*- :int = 25*)
x + y;;
(*- :int = 7*)
let x = x + y in let y = x * y in x + y + z;;
(*- :int = 24*)
x + y + z;;
(*- :int = 10*)
int_of_float;;
(*- : float -> int = <func>*)
float_of_int;;
(*- : int -> float = <func>*)
int_of_char;;
(*- : char -> int = <func>*)
char_of_int;;
(*- : int -> char = <func>*)
abs;;
(*- : int -> int = <func>*)
sqrt;;
(*- : float -> float = <func>*)
truncate;;
(*- : float -> int = <func>*)
ceil;;
(*- : float -> float = <func>*)
floor;;
(*- : float -> float = <func>*)
Char.code;;
(*- : char -> int = <func>*)
Char.chr;;
(*- : int -> char = <func>*)
Char.uppercase;;
(*- : char -> char = <func>*)
Char.lowercase;;
(*- : char -> char = <func>*)
int_of_string;;
(*- : string -> int = <fun>*)
string_of_int;;
(*- : int -> string = <func>*)
String.length;;
(*- : string -> int*)
let f = function x -> 2 * x;;
(*- : int -> int = <func>*)
f (2+1);;
(*- : int = 6*)
f 2 + 1;;
(*- : int = 5*)
let n = 1;;
(*val n : int = 1*)
let g x = x + n;;
(*- : int -> int = <func>*)
g 3;;
(*- : int = 4*)
let n = 5;;
(*val n : int = 5*)
g 3;;
(*- : int = 4*)