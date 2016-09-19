(* James Caldwell                                                    
   University of Wyoming, Department of Computer Science, Laramie WY 
   COSC 4780 -- Principles of Programming Languages -- Spring 2016
*)

(* test code for hw4 *)

(* you can just evaluate the lines here after evalauting your code, or ...
   to get the use-directive to work you'll have fix the path so it points to where your source code is located. *) 

#use "C:/Users/pdani_000/OneDrive/4780 code/hw4i.ml";;


let rec string_of_term t = 
  match t with
      Var x -> x
    | Abs (x,t) -> "(\\" ^ x ^ ". " ^ (string_of_term t) ^ ")"
    | Ap(t1,t2) -> "(" ^ (string_of_term t1) ^ " " ^ (string_of_term t2) ^ ")"
;;

let print_term t = 
  print_string (string_of_term t ^ "\n")
;;

let test t = 
 print_string (string_of_term t ^ "  -->  " ^ string_of_term (reduce t) ^ "\n");;



let x = Var "x";;
let y = Var "y";;
let z = Var "z";;
let w = Var "w";;

let t1 = Abs ("x",x);;
let t2 = Abs ("x",y);;
let t3 = Abs ("x", t1);;

let t4 = Ap (t1,t1);;
t4;;
beta t4;;
test t4;;

let t5 = Ap (z, (Ap (t1, y)));;
beta t5;;
test t5;;

let t6 = Ap(Ap(Abs("x",Ap(x,y)),y),z);;
beta t6;;
test t6;;

let t7 = Ap(Ap(z,Ap(Abs ("x", Abs ("y",Ap (x,y))), y)), w) ;;
beta t7;;
test  t7;;

let t8 = Ap (Abs ("x",Ap (Ap (Var "x", Var "x"), Var "x")), Ap (t1,t1));;
test t8;;

let t9 = let t = Abs ("x", (Ap (Var "y", Ap (Var "x", Var "y")))) in Ap(Ap(t1,t),t1);;
test t9;;
