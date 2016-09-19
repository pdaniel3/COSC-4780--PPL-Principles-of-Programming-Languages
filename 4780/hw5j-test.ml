(* James Caldwell                                                    
   University of Wyoming, Department of Computer Science, Laramie WY 
   COSC 4780 -- Principles of Programming Languages -- Spring 2016
*)


(*-------------------------------------------------------------------------------- *)
(* test code for hw5j                                                              *)
(* --------------------------------------------------------------------------------*)

let context_of_term  t = 
  let rec mk_entries vars used_so_far = 
    match vars with
	[] -> []
      | t::ts ->
	  let c = fresh "c" used_so_far in
	    (t, TVar c):: mk_entries ts (c::used_so_far)
  in
    Context (mk_entries (fv_term t) [])
;;

let test trm = 
  let context = context_of_term trm in
  let vars = tvarsContext context in
  let a = fresh "a" vars in
  let (c,vars) = wand context trm (TVar a) (a::vars) in
    try 
      (let s = unify c   in
	 print_string ("const: " ^ (constraintSet2str c) ^ "\n");
	 print_string ("subst: " ^ (subst2str s) ^ "\n\n");
	 print_string ((context2str (substContext s context)) ^ " |- " ^ (term2str trm) ^ " : " ^ (types2str (substType s (TVar a))) ^ "\n\n"))
    with
      Failure msg -> print_string (msg ^ "\n")
;;


(* this test gives back more information - might be helpfull in debugging. *)
let test1 trm = 
  let context = context_of_term trm in
  let vars = tvarsContext context in
  let a = fresh "a" vars in
  let (c,vars) = wand context trm (TVar a) (a::vars) in
  let s = try subst2str (unify c) with Failure msg -> "solve failed saying: " ^ msg ^ "." in
    print_string ("C: " ^ (constraintSet2str c));
    print_string "\n";
    print_string ("TVars: " ^ (list2str (fun x -> x)("[","]") ";" vars));
    print_string "\n";
    print_string s;
    print_string "\n"
;;  

test (Abs("x", Var "x"));;

test (Ap (Abs ("x", Abs ("x", Var "x")), Var "y"));;

(* (\* Y combinator  - not typable - SHOULD FAIL *\) *)

test (Abs ("h", (Ap ((Abs ("x", (Ap (Var "h", (Ap (Var "x", Var "x")))))),(Abs ("x", (Ap (Var "h", (Ap (Var "x", Var "x"))))))))));; 

(* (\* compose operator*\) *)
test (Abs ("f", (Abs ("g" , (Abs ("x", Ap (Var "f" , Ap (Var "g", Var "x"))))))));; 


(* (\* double application *\) *)
test (Abs ("f", (Abs ("x" ,  Ap (Var "f" , Ap (Var "f", Var "x"))))));; 

(* (\* Example 4 - Additional Example Taken from Reynolds *\)  *)

test (Ap ((Abs ("x", Var "x")) ,(Abs ("f", Abs("x", Ap (Var "f", Ap (Var "f", Var "x")))))));; 

(* the K combinator *)
test (Abs ("x", Abs ("y", Var "x")));;

(* the S combinator *)
test1 (Abs ("x", Abs ("y", Abs ("z", Ap (Ap (Var "x", Var "z"), Ap (Var "y", Var "z"))))));;

