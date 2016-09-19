(* haskell: data Tree a = Node (Tree a) (Tree a) | Leaf *)

(* ocaml: *)
type 'a tree = Node of 'a * 'a tree * 'a tree | Leaf;;

(* declaring a tree 
 *
 * let node x y z = Node (x,y,z);; *)

(* haskell
 *
 * case foo of 
 *   [] -> 0
 *   (x:xs) -> 1
 *)
 
(* ocaml *)

match foo with
  | [] -> 0
  | (x::xs) -> 1;;

(* haskell
 * foo [] = o
 * foo (x:xs) = 0
*)

(* ocaml *)

let foo x = 
  match x with 
    [] -> 0
  | (x::xs) -> 1;;

 
