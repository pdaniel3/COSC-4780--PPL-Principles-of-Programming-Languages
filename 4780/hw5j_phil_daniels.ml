(*
 * Phil Daniels
 * Hadi, we spoke Friday around noon about
 * me turning this in a second time, and
 * you wanted me to leave a comment. Thank you.
 *)


(* Code developed by Sunil Kothari & James Caldwell.
 * Implements an  extension of Wand's algorithm as described in the paper
 * titled "On Extending Wand's Type Reconstruction Algorithm to Handle Polymorphic Let"
 * Copyright (C) 2007  Sunil Kothari, University of Wyoming.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 *
 *)


(* -------------------------------------------------------------------------------- *)
(*  Some utility functions                                                          *)
(*--------------------------------------------------------------------------------  *)

let remove_all x t = List.filter (fun h -> not (x = h)) t ;;
let unique l = List.fold_right (fun x y -> x :: remove_all x y) l [];;

(* returns a "fresh" variable wrt the variables in the string list xs.  Note, the 
   argument x is the suggested name of the new variable. *)

let fresh x xs  =
  let count = ref (0) in
  let rec getit y =
    if not(List.mem y xs) then
      y
    else
      (count := !count + 1;
       getit (x ^ (string_of_int !count)))
  in
    getit x
;;

let gwrap (l,r) x = l ^ x ^ r ;;
let wrap = gwrap ("(",")")  ;;

let list2str a2str wrapper sep l = 
  let m = List.fold_left (fun x y -> x ^ sep ^ (a2str y))  "" l in
  let k = String.length sep in
  let m' = if m = "" then m else (String.sub m k (String.length m - k)) in
    gwrap wrapper m' 
;;
   
let list_minus l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1;;


(*--------------------------------------------------------------------------------*)
(* term : lambda terms                                                            *)
(*--------------------------------------------------------------------------------*)

type term =
    Var of string
  | Abs of string * term
  | Ap of term * term
 ;;

let rec fv_term  t = 
  match t with    
      Var z -> [z]
    | Abs (x,t1) -> remove_all x (fv_term t1)
    | Ap (x,y) -> (fv_term x) @ (fv_term y)
;;


let rec term2str t = 
match t with    
    Var z -> z
  | Abs (x,t1) -> wrap ("\\" ^ x ^ "." ^ (term2str t1))
  | Ap (x,y) -> wrap ( (term2str x) ^" "^ (term2str y))
;;


(*--------------------------------------------------------------------------------*)
(* Simple types                                                                   *)
(*--------------------------------------------------------------------------------*)

type  types =
  | TVar of string
  | Arrow of  types *  types
;;

let rec tvars t = 
  match t with 
      TVar x -> [x]
    | Arrow (t1,t2) -> tvars t1 @ tvars t2
;;

let rec types2str t =
  match t with 
    | TVar a -> a
    | Arrow(a,b) -> wrap ((types2str a ) ^ "->" ^ (types2str b))
;;

(*--------------------------------------------------------------------------------*)
(* environments: keep track of types of term variables                            *)
(*--------------------------------------------------------------------------------*)


type context = Context of (string * types )  list ;;

let lookup x (Context e) = List.assoc x e;;

let addType x t (Context e) = Context ((x,t) :: e);;

let context2str (Context e) = 
  list2str 
    (fun (x,y) -> x ^ ":" ^ (types2str y)) ("{","}") "," e ;; 

let tvarsContext (Context e) = 
  List.concat (List.map (fun (_,t) -> tvars t) e) ;;

(*--------------------------------------------------------------------------------*)
(* constraints: these are pairs of types (asserted to be equal)                   *)
(*--------------------------------------------------------------------------------*)

type eqn = Eq of (types * types) ;; 

let eqn2str (Eq (t1,t2)) = types2str t1 ^ " = " ^ types2str t2 ;;
 
type constraintSet = ConstraintSet of eqn list;;

let constraintSet2str (ConstraintSet e) = 
  list2str eqn2str  ("{","}") "," e ;; 




(*-------------------------------------------------------------------------------- *)
(* substitutions: type values substitutions                                       *)
(*--------------------------------------------------------------------------------*)

(* We're representing a substitution as a list of pairs *)
type subst = Subst of (string * types) list;;


let subst2str (Subst s) = 
  list2str (fun (x,y) -> x ^ ":= " ^ types2str y) ("{", "}") ";"  s;; 

let dom (Subst s) = List.map fst s;;
let rng (Subst s) = List.map  snd s;;

let rec substType (Subst s) t =
  match t with
    | TVar a -> (try List.assoc a s with Not_found -> t)
    | Arrow(t1,t2)  -> Arrow (substType (Subst s) t1, substType (Subst s) t2)
;;

(* apply a type substitution to a context *)
let rec substContext s (Context c) = Context (List.map (fun (x,t) -> (x, substType s t)) c) ;;

let rec substEqn s (Eq(t1,t2)) = Eq (substType s t1, substType s t2) ;;

let pair_Eq f (Eq(t1,t2)) = Eq(f t1, f t2);;
       
let substConstraintSet s (ConstraintSet e) = ConstraintSet (List.map (substEqn s) e);;

let substSubst s (Subst s') = Subst (List.map (fun (x,ty) -> (x, substType s ty)) s')

(* note: composition may be backwards from what you expect
     -- (compose s1 s2) t = substType s2 (substType s1 t)  *)

let compose_subst (Subst s1) (Subst s2) = 
   let comp1 = List.map (fun (x,y) -> (x,substType (Subst s2) y)) s1 in 
   let dom = List.map fst comp1 in
   let comp3 = List.filter (fun (x,_) -> not (List.mem x dom)) s2 in
     Subst(comp1 @ comp3);;


(*-------------------------------------------------------------------------------- *)
(* Unification for types                                                           *)
(* --------------------------------------------------------------------------------*)

let rec unify (ConstraintSet c) =  
  match c   with
      []  -> (Subst [])
    | h::t -> 
	match h with
          | Eq(TVar a, TVar b)  -> 
	      if a = b then 
		unify (ConstraintSet t )
              else 
		let s = Subst [(a, TVar b)] in
		  compose_subst s (unify (ConstraintSet (List.map (pair_Eq (substType s)) t)))
	  | Eq(TVar a, ty) ->   
	      if (List.mem a (tvars ty)) then
		raise (Failure "unify: occurs check failure.")
	      else  
		let s = Subst [(a, ty)] in
		  compose_subst s (unify (ConstraintSet (List.map (pair_Eq (substType s)) t)))
	  | Eq(ty, TVar a) -> unify (ConstraintSet (Eq (TVar a,ty)::t)) 
	  | Eq(Arrow(t1,t2),Arrow(t3,t4)) -> unify (ConstraintSet ((Eq(t1, t3)):: (Eq(t2,t4))::t))
;;

(*-------------------------------------------------------------------------------- *)
(* Put your version of wand here                                                    *)
(* --------------------------------------------------------------------------------*)

(* wand :: context -> term -> types -> string list -> constraintSet *
string list *) (* Note - the string list argument - and return value
are the list of variables used so far.  If the term is closed - has no
free variables - the list is empty, if the term has free variables,
add them to the list.  As you create fresh variables - add them to the
list for the recursive calls and *)


let get_string a =
  match a with
    TVar a -> a
;;

let rec wand context t ty vars =
  match t with
    Var x -> let t' = lookup x context in
             (ConstraintSet([Eq(ty, t')]), vars)
  | Ap (m,n) -> let alpha = TVar (fresh "alpha" vars) in
                let (ConstraintSet(c1), vars1) = wand context m (Arrow(alpha, ty)) ((get_string alpha)::vars) in
                let (ConstraintSet(c2), vars2) = wand context n alpha vars1 in
                  (ConstraintSet(c1@c2), vars2)
  | Abs (x,m) -> let alpha = TVar(fresh "alpha" vars) in
                 let beta = TVar (fresh "beta" vars) in
                 let (ConstraintSet(c), vars') = wand (addType x alpha context) m beta (get_string(alpha)::(get_string(beta)::vars)) in
                 (ConstraintSet([Eq(ty,Arrow(alpha,beta))]@c),vars')    
                  
;;
(*-------------------------------------------------------------------------------- *)
(* testing                                                                         *)
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


(* this test gives back more information *)
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

