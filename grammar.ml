type syntagm = string (** non terminaux *) [@@ deriving yojson]
type token = string (** terminaux *) [@@ deriving yojson]


type symbol = 
    | Item of token
    | Var of syntagm 
    [@@ deriving yojson]
type production = Production of symbol list [@@ deriving yojson]
type rules = Rules of syntagm * production list [@@ deriving yojson]
type grammar = Grammar of syntagm * rules list [@@ deriving yojson]
(*
let grammar_to_yojson : grammar -> Yojson.Safe.t = function 
 | _ -> failwith "TODO"

let grammar_of_yojson : Yojson.Safe.t -> (grammar,string) Result.result =
  let open Result in function
  | _ -> Error "Invalid serialization of item"
*)

let initial_grammar = Grammar("S", [Rules("S", [Production[]])])

(*
let g4 = Grammar("S", [Rules("S", [Production([Var("E")])]); 
    Rules("E", [Production([Var("E"); Item("+"); Var("N")]); Production([Var("E"); Item("-"); Var("N")]); Production([Var("N")])]);
    Rules("N", [Production([Var("N"); Item("*"); Var("F")]); Production([Var("N"); Item("/"); Var("F")]); Production([Var("F")])]);
    Rules("F", [Production([Item("a")]); Production([Item("-"); Var("F")]); Production([Item("+"); Var("F")]); Production([Item("("); Var("E"); Item(")")])])])

let g3 = Grammar("Z", [
  Rules("Z", [Production([Var("U"); Var("V")])]);
  Rules("U", [Production([Item("a")]);Production([Item("b")])]);
  Rules("V", [])
  ]) 

let (g1:grammar) = Grammar("Z" , [
 Rules("Z", [Production([Item("{"); Var("X"); Var("U")])]);
 Rules("X", [Production([Item("ID:");Var("Y")]); Production([Item("ID:"); Var("Z")])]);
 Rules("Y", [Production([Item("NUM")]); Production([Item("STR")])]);
 Rules("U", [Production([Item(", "); Var("X")]); Production([Item("}")])]);
])

let (g2:grammar) = Grammar("X", [
    Rules("X", [
        Production([Item("("); Var("X"); Item(")")]); 
        Production([Item("["); Var("X"); Item("]")]);
        Production([Var("X"); Var("X")]);
        Production([])
    ])
]) 
*)





(** 
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
*)