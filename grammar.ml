type syntagm = string (** non terminaux *)
type token = string (** terminaux *)


type production = 
    | Epsilon
    | Item of token
    | Var of syntagm 
type rule = Rule of syntagm * production * production 
type grammar = Grammar of syntagm * rule list 


let (g1:grammar) = Grammar("Z" , [
    Rule("Z", Item("{"), Var("W"));
    Rule("W", Var("X"), Var(U)); 
    Rule("X", Item("ID:"),Var("Y"));
    Rule("X", Item("ID:"), Var("Z"));
    Rule("Y", Item("NUM"), Epsilon);
    Rule("Y", Item("STR"), Epsilon);
    Rule("U", Item(", "), Var("X"));
    Rule("U", Item("}"), Epsilon);
]) 

let (g2:grammar) = Grammar("X", [
    Rule("X", Item("("), Var("W"));
    Rule("W", Var("X"), Item(")"));
    Rule("X", Item("["), Var("Y"));
    Rule("Y", Var("X"), Item("]"));
    Rule("X", Var("X"), Var("X"));
    Rule("X", Epsilon, Epsilon)
]) (* par contre ambigüe *)

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
