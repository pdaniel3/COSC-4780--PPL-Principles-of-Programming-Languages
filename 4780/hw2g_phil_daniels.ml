(* Phil Daniels
 * homework 2 *)


(* Some coe to based HW 2 on *)

type lambda = Var of string | Ap of lambda * lambda | Lambda of string * lambda | Left of lambda | Right of lambda | Decide of (lambda * string * lambda * string * lambda);;

let rec pretty t = 
  match t with 
      Var x -> x
    | Ap (m,n) -> pretty m ^ " " ^ pretty n
    | Lambda (x,m) -> "(lambda " ^ x ^ ". " ^ pretty m ^ ")"
    | Left m -> "Left " ^ pretty m ^ ""
    | Right n -> "Right " ^ pretty n ^ ""
    | Decide (m,x,n1,y,n2) -> "decide(" ^ pretty m ^ ", " ^ x ^ ". " ^ pretty n1 ^ "; " ^ y ^ ". " ^ pretty n2 ^ ")"
;; 




let remove_all x xs = List.filter (fun y -> not (y = x)) xs;;

let rec fv t = 
  match t with 
      Var x -> [x]
    | Ap (m,n) -> fv m @ (fv n)
    | Lambda (x,m) -> remove_all x (fv m)
    | Left m -> fv m
    | Right n -> fv n
    | Decide (m,x,n1,y,n2) -> fv m @ (remove_all x (fv n1)) @ (remove_all y (fv n2))
;; 


(* fresh - returns a fresh variable - w.r.t. the list m *)
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

  
(* here's the subst code presented in class -- extend it to work for Left, Right and Decide *)
 
let your_code_goes_here ()= failwith "Your code goes here!" ;;

let rec subst (x,m) n = 
  match n with
      Var y -> if y = x then m else n
    | Ap (n1,n2) -> Ap (subst (x,m) n1, subst (x,m) n2)
    | Lambda (y,n1) ->
	if y = x then 
	  n
	else 
	  let free = fv m in
            if List.mem y free then
	      let z = fresh "z" (free @ (fv n1) @ [x;y]) in
		Lambda (z, subst (x,m) (subst (y, Var z) n1))
	    else
	      Lambda (y, subst (x,m) n1)
    | Left m1 -> Left (subst (x,m) m1)
    | Right n1 -> Right (subst (x,m) n1)
    | Decide (m1,y,n1,z,n2) -> if x = y then
                                 if x = z then
                                   Decide(subst (x,m) m1,y,n1,z,n2)  
                                 else
                                   let free = fv m in
                                     if List.mem z free then
	                               let z' = fresh "z'" (free @ (fv n1) @ [x;y]) in
                                         Decide(subst (x,m) m1,y,n1,z',subst (x,m) (subst (z,Var z') n2))
                                     else
                                       Decide(subst (x,m) m1,y,n1,z,subst (x,m) n2)
                               else         
                                 let free = fv m in
                                     if List.mem y free then
	                               let y' = fresh "y'" (free @ (fv n1) @ [x;y]) in
                                         if x = z then
                                           Decide(subst (x,m) m1,y',subst (x,m) (subst (y,Var y') n1),z,n2) 
                                         else
                                           let free = fv m in
                                             if List.mem z free then
	                                       let z' = fresh "z'" (free @ (fv n1) @ [x;y]) in
                                               Decide(subst (x,m) m1,y',subst (x,m) (subst (y,Var y') n1),z',subst (x,m) (subst (z,Var z') n2))
                                             else
                                               Decide(subst (x,m) m1,y',subst (x,m) (subst (y,Var y') n1),z,subst (x,m) n2)
                                     else
                                       if x = z then
                                           Decide(subst (x,m) m1,y,subst (x,m) n1,z,n2) 
                                         else
                                           let free = fv m in
                                             if List.mem z free then
	                                       let z' = fresh "z'" (free @ (fv n1) @ [x;y]) in
                                                 Decide(subst (x,m) m1,y,subst (x,m) n1,z',subst (x,m) (subst (z,Var z') n2))
                                             else
                                               Decide(subst (x,m) m1,y,subst (x,m) n1,z,subst (x,m) n2)
;;




(* some tests *)

pretty (subst ("x", Var "y") (Lambda ("x", Var "x")));;
pretty (subst ("x", Var "y") (Lambda ("y", Var "x")));;
pretty (subst ("x", Var "y") (Left  (Var "x")));;
pretty (subst ("x", Var "y") (Left  (Var "w")));;
pretty (subst ("x", Var "y") (Right (Var "x")));;
pretty (subst ("x", Var "y") (Left (Right (Var "x"))));;
pretty (subst ("x", Var "y") (Right (Var "w")));;
pretty (subst ("x",Var "y") (Decide (Left (Var "x"), "x", Var "x", "y", Var "x")));;
pretty (subst ("x",Var "y") (Decide (Left (Var "x"), "x", Var "x", "y", Var "x")));;
pretty (subst ("x", Lambda ("x",Left (Var "x"))) (Decide (Ap (Var "x", Var "y"), "z", Var "x", "w", Var "w")));; 
pretty (subst ("x", Lambda ("x",Left (Var "x"))) (Decide (Ap (Var "x", Var "y"), "z", Var "x", "y", Var "y")));; 
pretty (subst ("x", Ap (Lambda ("x",Left (Var "x")), Var "x")) (Decide (Ap (Var "x", Var "y"), "z", Var "x", "y", Var "y")));; 

