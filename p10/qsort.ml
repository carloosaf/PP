let rec qsort1 ord = function
    [] -> []
    | h::t -> let after, before = List.partition (ord h) t 
              in qsort1 ord before @ h :: qsort1 ord after;;

(* 

 - ¿En qué casos no será bueno el rendimiento de esta implementación? 

  En los casos en los que la lista a ordenar sea demasiago grande y produzca
Stack_overflow.

 *)


let rec qsort2 ord =
    let append' l1 l2 = List.rev_append (List.rev l1) l2 in
    function
    [] -> []
  | h::t -> let after, before = List.partition (ord h) t 
            in append' (qsort2 ord before) (h :: qsort2 ord after);;

(* 
 - ¿Tiene qsort2 alguna ventaja sobre qsort1? ¿Permite qsort2 ordenar listas que no podrían
    ordenarse con qsort1?

  Si, qsort2 tiene la ventaja de ser completamente recursiva terminal, por lo que no 
produce Stack_overflow. qsort1 no es recursiva terminal por lo que en algunos casos no puede 
ordenar vectores que qsort2 si.
*)

let l1 = List.init 1_000_000 ((+) 0);;

(*
- ¿Tiene qsort2 alguna desventaja sobre qsort1?

  Para conseguir que qsort2 sea recursiva terminal debemos usar la funcion append’, mucho
menos eficiente que @, por lo tanto es mucho más lenta.

 *)

let l = List.init 1_000 ((+) 0);;

let crono f x = 
  let t = Sys.time() in 
  f x ; Sys.time()-. t;;

let delta = crono (qsort1 (>=)) l -. crono (qsort2 (>=)) l;;

(*

#   val delta : float = -0.0794479999999993
#   val delta : float = -0.0955559999999877618
#   val delta : float = -0.0769390000000100827
#   val delta : float = -0.0879799999999875126

Como podemos ver en estas ejecucines de delta qsort2 es constantemente más lenta que qsort1.
Concretamente tarda alrededor de un 50% más en ordenar un vector de 1000 elementos. 

 *)

let rec divide l = match l with
    h1::h2::t -> let t1, t2 = divide t in (h1::t1, h2::t2)
  | _ -> l, [];;

let rec merge f = function
    [], l | l, [] -> l
  | h1::t1, h2::t2 -> if (f h1) h2 then h1 :: merge f (t1, h2::t2)
                      else h2 :: merge f (h1::t1, t2);;

let rec msort1 f l = match l with
    [] | _::[] -> l
  | _ -> let l1, l2 = divide l 
         in merge f (msort1 f l1, msort1 f l2);;

(*

 - ¿Puede provocar alg ́un problema la no terminalidad de divide o merge?

 Si, pueden dar lugar a un Stack_overflow

 *)


let l2 = List.init 1_000 ((+) 0);;

let divide' l =
    let rec aux acc1 acc2 l1 = match l1 with
        [] -> List.rev acc1, List.rev acc2 |
        h1::[] -> aux (h1::acc1) (acc2) [] |
        h1::h2::t -> aux (h1::acc1) (h2::acc2) t
    in aux [] [] l;;

let merge' f (l1, l2) = 
    let rec aux acc = function 
        [],[] -> List.rev acc |
        [], h::t | h::t, [] -> aux (h::acc) ([], t) |
        h1::t1, h2::t2 -> if f h1 h2 then aux (h1::acc) (t1,(h2::t2))
        else aux (h2::acc) (h1::t1, t2)
    in aux [] (l1,l2);;


let rec msort2 f l = match l with
    [] | _::[] -> l
  | _ -> let l1, l2 = divide' l 
         in merge'  (f) (msort2 f l1, msort2 f l2);;


(*

  - Compare el rendimiento en tiempo de ejecución de msort2 con el de msort1 y con el de qsort2.

  Con una lista de 1000 ints generado aleatoriamente

  qsort2
- : float = 0.00116299999999824877
  
  msort2
- : float = 0.00135000000000218279

  msort1
- : float = 0.000982000000000482487

Observamos como msort1 es el más rapido, debido a que no se sacrifica su rendimiento a costa de hacerlo recursivo
terminal. Por otro lado qsort2 sigue siendo más rapido que msort2, ya que es más eficiente en este caso y ambos 
sacrificaron rendimiento para ser recursivos terminales.

  Con una lista de 10.000 ints generada aleatoriamente

  qsort2  
- : float = 3.79592599999999791

  msort2 
- : float = 4.41033700000000373

  msort1
Stack_overflow

En este caso qsort2 continua siendo más eficiente que msort2. Msort1 ya no puede completar su ejecucion en este caso.

 *)

