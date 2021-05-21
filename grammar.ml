(*
type production = 
    | Epsilon
    | String of string 
    | Vars of string list 
type rule = (string * production)
type grammar = string * rule list 

(*
ou alors
type rule = string * ((production list) list) ?
type grammar = string * (rule list) si on souhaite préciser l'axiome 
*)


let (g1:grammar) = "Z" , [("Z", Vars(["A";"X"; "U"]));
          ("X", Vars(["B";"Y"]));("X", Vars(["B"; "Z"]));
          ("Y", String("NUM"));("Y", String("STR"));
          ("U", Vars(["C"; "X"; "U"]));("U", String("}"));
          ("A", String("{")); ("B", String("ID:")); ("C", String(", "))]  

let (g2:grammar) = "X", [("X", Vars(["A";"X";"B"]));
          ("X", Vars(["C"; "X"; "D"]));
          ("X", Vars(["X";"X"]));("X", Epsilon)] (* par contre ambigüe *)

*)

type syntagm = string (** non terminaux *)
type token = string (** terminaux *)

(**
    a grammarTree represents a grammar under the form of a tree. 
    The initial state is represented by Nill -> an empty tree
    We should always have (unless the grammar is empty) the root be a
    Var. Prod and Item should only appear in subtrees of a tree starting with a Var
    We have Var("Z", lg, rg) represents Z -> lg | rg 
            Var("Z", Prod(lg, rg), rg') represents Z -> lg.rg | rg' 
            Var("Z", Item(s), rg) represents Z -> s
    In cases were we have rules such as Z -> Xb and then X-> sZ
    we should have 
    Var("Z", 
        Prod(Var("X", Item(s), Var("Z", Nill, Nill)), Item(b)), 
        Nill
    )
    because Z is in the context of X so we allready know the rules for Z.
    this should also happen when we have Z -> XY and Y -> aX
    X is in the context of Y so we already know the rules for X, 
    there is no need to duplicate them here. 
 *)
type grammarTree = 
| Nill
| Var of syntagm * grammarTree * grammarTree
| Prod of grammarTree * grammarTree 
| Item of token 

let (g1': grammarTree) = 
Var("Z", 
    Prod(Item("{"), 
        Prod(
            Var("X", 
                Prod(Item("ID:"), Var("Y", Item("NUM"), Item("STR"))), 
                Prod(Item("ID:"), Var("Z", Nill, Nill))
            ),
            Var("U", Prod(Item(", "), Var("X", Nill, Nill)), Item("}") )
        )
    ),
Nill)
