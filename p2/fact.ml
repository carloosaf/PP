let rec fact = function
0 -> 1
| n -> n * fact (n - 1);;

if Array.length Sys.argv = 2 then
  print_int (fact (int_of_string Sys.argv.(1)))
else
  print_endline "fact: número de argumendos invalido";;