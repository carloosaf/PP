
(* IMPLIMENTACION 1 *)

type log_exp =
    Const of bool
  | Var of string
  | Neg of log_exp
  | Disj of log_exp * log_exp
  | Conj of log_exp * log_exp
  | Cond of log_exp * log_exp
  | BiCond of log_exp * log_exp;;

let rec eval ctx = function
    Const b -> b
  | Var s -> List.assoc s ctx
  | Neg e -> not (eval ctx e)
  | Disj (e1, e2) -> (eval ctx e1) || (eval ctx e2)
  | Conj (e1, e2) -> (eval ctx e1) && (eval ctx e2)
  | Cond (e1, e2) -> (not (eval ctx e1)) || (eval ctx e2)
  | BiCond (e1, e2) -> (eval ctx e1) = (eval ctx e2);;

(* IMPLIMENTACION 2*)

type oper = Not;;

type biOper = Or | And | If | Iff;;

type prop =
    C of bool
  | V of string
  | Op of oper * prop
  | BiOp of biOper * prop * prop;;

(* FUNCTIONS *)

let rec prop_of_log_exp = function
    Const c -> C c
  | Var v -> V v
  | Neg e -> Op (Not, (prop_of_log_exp e))
  | Disj (e1, e2) -> BiOp (Or, prop_of_log_exp e1, prop_of_log_exp e2)
  | Conj (e1, e2) -> BiOp (And, prop_of_log_exp e1, prop_of_log_exp e2)
  | Cond (e1, e2) -> BiOp (If, prop_of_log_exp e1, prop_of_log_exp e2)
  | BiCond (e1, e2) -> BiOp (Iff, prop_of_log_exp e1, prop_of_log_exp e2);;


let rec log_exp_of_prop = function
    C c -> Const c
  | V v -> Var v
  | Op(Not, prop) -> Neg (log_exp_of_prop prop)
  | BiOp (Or, e1, e2) -> Disj ((log_exp_of_prop e1), (log_exp_of_prop e2))
  | BiOp (And, e1, e2) -> Conj ((log_exp_of_prop e1), (log_exp_of_prop e2))
  | BiOp (If, e1, e2) -> Cond ((log_exp_of_prop e1), (log_exp_of_prop e2))
  | BiOp (Iff, e1, e2) -> BiCond ((log_exp_of_prop e1), (log_exp_of_prop e2));;


let opval = function
    Not -> (not);; 

let biopval = function
    Or -> (||)
  | And -> (&&)
  | If -> (fun x y -> not x && x)
  | Iff -> (=);;

let rec peval ctx = function
    C b -> b
  | V v -> List.assoc v ctx
  | Op (p, e) -> (opval p) (peval ctx e) 
  | BiOp (p, e1, e2) -> (biopval p) (peval ctx e1) (peval ctx e2);;
