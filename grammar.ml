open Focus


type production = Epsilon | Var of string | String of string 
type rule = string * (production list)
type grammar = rule list 

(*
ou alors
type rule = string * ((production list) list) ?
type grammar = string * rule list si on souhaite préciser l'axiome 
*)

let (g1:grammar) = [("Z", [String("{"); Var("X"); Var("U")]);
          ("X", [String("ID:"); Var("Y")]);("X", [String("ID:"); Var("Z")]);
          ("Y", [String("NUM")]);("Y", [String("STR")]);
          ("U", [Var("X"); Var("U")]);("U", [String("}")])]  

let (g2:grammar) = [("X", [String("("); Var("X");String(")")]);
          ("X", [String("["); Var("X"); String("]")]);
          ("X", [Var("X"); Var("X")]);("X", [Epsilon])] (* par contre ambigüe *)