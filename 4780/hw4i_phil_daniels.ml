(* James Caldwell                                                    
   University of Wyoming, Department of Computer Science, Laramie WY 
   COSC 4780 -- Principles of Programming Languages -- Spring 2016
*)

(* base code for hw4 *)


let remove_all x m =  List.filter (fun y -> y <> x) m;;

let fresh x m = 
  let rec fresh' x k =
    let x_k = x ^ string_of_int k in
      if List.mem x_k m then
	fresh' x (k+1) 
      else
	x_k
  in
    if List.mem x m then
      fresh' x 0 
    else 
      x
;;
(* the type of lambda terms *)
type term =  Var of string | Abs of string * term | Ap of term * term ;;

(* free variables  of a term *)
let rec fv m =
  match m with
    Var x -> [x]
  | Abs (x, m) -> remove_all x (fv m)
  | Ap (m, n) -> (fv m) @ (fv n)
;;

(* implements capture avoiding substitution m[y:=t] *)
let rec subst (y, t) m =
  match m with
    Var x -> if (x = y) then t else  m
  | Abs (x, n) -> if x = y then m else 
                      if not (List.mem x (fv t)) then 
                        let n' = subst (y,t) n in 
                          Abs (x, n')
                      else 
			let vars = y :: x :: (fv t @ fv n)  in
                         let z = fresh "z" vars in 
                         let s' = (x, Var z) in
                         let n' = subst (y, t) (subst s' n) in
 			   Abs (z, n')
  | Ap (n1, n2) -> Ap (subst (y,t) n1, subst (y,t) n2)
			 ;;


(* you need to implement this function - if t is a redex - reduce it - if not just return t unchanged.*)
let beta t = 
  match t with
    Ap (Abs (y,m), n) -> subst(y,n) m
  | _ -> t 
;;


(* if f(x) = x, then x is a fixedpoint of the function f *)
(* The following function computes the fixedpoint of f starting from x -- if there is one. *)

let rec fix f x = 
  let y = f x in
    if x = y then
      x
    else
      fix f y
;;


(* and this one should find a redex in a term - if there is one - and reduce it. If there is no redex - simply return the term unchanged.*)          
let rec one_step t = 
  match t with 
      Var x -> Var x 
    | Abs (x,m) -> Abs (x, one_step (beta m))
    | Ap (m,n) -> if m = n then one_step m
                  else Ap (one_step (beta m), one_step (beta n))
                    
;;


(* reduce can be computed by applying one-step until it reaces a fixedpoint *)
let reduce = fix one_step;;


