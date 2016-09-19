type formula = 
  Var of string
  | Not of formula
  | And formula * formula
  | Or of formula * formula
  | Implies of formula * formula
;;
