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

  Si, qsort2 tiene la ventaja de ser recursiva terminal, por lo que no 
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
