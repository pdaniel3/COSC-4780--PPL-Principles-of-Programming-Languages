(* Phil Daniels Homework 6 *)

(* PROOFS -------------------------------------------------------
PROOFS
6.2a First we know that both sides of the equality have the 
type of command, so we can apply extensionality. Take some
arbitrary store s

[[C1;(C2;C3):comm]](s) = [[(C1;C2);C3:comm]](s)

Starting from the left...

[[C1;(C2;C3):comm]](s) = [[C2;C3:comm]]([[C1:comm]](s)) 
                       = [[C3:comm]]([[C2:comm]]([[C1:comm]](s)))
                       = [[C3:comm]]([[C1;C2:comm]](s)) 
                       = [[(C1;C2);C3:comm]](s)

6.2c. So I ran out of time, so this is an informal proof...
Because both left and right have the same type (comm), we 
can apply extensionality. These two statements being equal 
makes sense intuitively. E : boolExp so it can be either 
true or false. The while loop terminates when it is equal 
to false, if it does indeed terminate. So we have two 
possible cases to prove this equality:


Case 1: The while loop never terminates.

We know that both pieces of code are syntactically identical
up until the end of the while loop. If the while loop does 
not terminate, it loops forever and continually calls C1, in 
both cases. If the while loop never terminates, nothing will 
be executed afterwards. So, both programs continually call C1 
forever in this case, making them semantically equivalent as 
well.

Case 2: The while loop terminates:
The while loop terminates if and only if E is false. So we 
assume that because both E’s and both C1’s in the left and 
right side of the equality are the same, that the while loop 
on both sides will execute C1 and terminate in the same amount 
of steps. So, again, semantically and syntactically speaking, 
the programs are identical up until the end of the while loop. 
The two programs differ only in the if then else statement 
following the while loop on the right side of the expression. 
Assuming the while loop terminates, it terminates when E is 
false, in both cases. So, the following if then else statement 
will only ever execute the else portion, because, when the while 
loop terminates, E will be false, so the "if" portion of the 
statement on the right side won’t execute. Because the "if" 
portion of the if then else statement will never execute, C2 
will always be called after the while loop in both the left and 
right expression. 
*)















(* James Caldwell                                                    
   University of Wyoming, Department of Computer Science, Laramie WY 
   COSC 4780 -- Principles of Programming Languages -- Spring 2016
*)

(* base code for hw6 *)

(* ====================================================================== *)
(* UTILITIES                                                              *)
(* ====================================================================== *)
 

(* compute the intersection of two lists *)

let list_intersection m n = 
  List.fold_right (fun x y -> if List.mem x n then x:: y else y) m [];;

(* ====================================================================== *)
(* ABSTRACT SYNTAX                                                        *)
(* ====================================================================== *)

(* ---------------------------------------------------------------------- *)
(* locations                                                              *)
(* ---------------------------------------------------------------------- *)

type loc =  Loc of int ;;

let string_of_loc l = 
  match l  with
   Loc i -> "Loc " ^ (string_of_int i)
;;

(* ---------------------------------------------------------------------- *)
(* expressions                                                            *)
(* ---------------------------------------------------------------------- *)

type expression =
    N of int
  | Deref of loc
  | Add of expression * expression
  | Neg of expression
  | Eq of expression * expression
;;

let rec string_of_expression e =
  match e with 
    N (i) -> "N " ^ (string_of_int i)
  | Deref (al) -> "Deref " ^ (string_of_loc al)
  | Add (e1,e2) -> "Add (" ^ (string_of_expression e1) ^ ", " ^ (string_of_expression e1) ^ ")"
  | Neg (e1) -> "Neg " ^ (string_of_expression e1)
  | Eq (e1,e2) -> "Eq  (" ^ (string_of_expression e1) ^ ", " ^ (string_of_expression e1) ^ ")"
;;


(* ---------------------------------------------------------------------- *)
(* commands                                                               *)
(* ---------------------------------------------------------------------- *)

type command = 
    Assign of loc * expression
  | Seq of command * command
  | If of expression * command * command
  | While of expression * command
  | Skip
;;

let rec string_of_command c = 
  match c with 
    Assign (al,e) -> "Assign (" ^ (string_of_loc al) ^ ", " ^ (string_of_expression e) ^ ")"
  | Seq (c1,c2) -> "Seq (" ^ (string_of_command c1) ^ ", " ^ (string_of_command c2) ^ ")"
  | If (e,c1,c2) -> "If (" ^ (string_of_expression e) ^ ", " ^ (string_of_command c1) ^ ", " ^ (string_of_command c2) ^ ")"
  | While (e,c1) -> "While (" ^ (string_of_expression e) ^ ", " ^ (string_of_command c1) ^ ")"
  | Skip  -> "Skip "
;;


(* ====================================================================== *)
(* ANALYSIS                                                               *)
(* ====================================================================== *)

(* ---------------------------------------------------------------------- *)
(* reports                                                                *)
(* ---------------------------------------------------------------------- *)

type report = Good | Possible of (command list);;

let union_reports r1 r2 = 
  match r1 with
      Good -> r2
    | Possible c1 -> (match r2 with
			  Good -> r1
			| Possible c2 -> Possible (c1 @ c2))
		      ;;

(* ---------------------------------------------------------------------- *)
(* the analysis                                                           *)
(* ---------------------------------------------------------------------- *)

(* implement the following functions *)
let rec exp_refs e = 
  match e with
    N (i) -> []
  | Deref (al) -> al::[]
  | Add (e1,e2) -> (exp_refs e1)@(exp_refs e2)
  | Neg (e1) -> exp_refs e1
  | Eq (e1,e2) -> (exp_refs e1)@(exp_refs e2)
;;

let rec active_locs c =  
  match c with
    Assign (al,e) -> al::[]
  | Seq (c1,c2) -> (active_locs c1)@(active_locs c2)
  | If (e,c1,c2) -> (active_locs c1)@(active_locs c2)
  | While (e,c1) -> (active_locs c1)
  | Skip  -> []
;;
        
let rec loops_analysis c =
  match c with
    Assign (al,e) -> Good
  | Seq (c1,c2) -> union_reports(loops_analysis c1) (loops_analysis c2)
  | If (e,c1,c2) ->  union_reports(loops_analysis c1) (loops_analysis c2)
  | While (e,c1) -> let intersection = 
                      list_intersection (active_locs c1) (exp_refs e) in
                    if intersection = [] then 
                      union_reports (Possible [c]) (loops_analysis c1)
                    else
                      union_reports (Good) (loops_analysis c1)  
                       
  | Skip  -> Good
;;  
