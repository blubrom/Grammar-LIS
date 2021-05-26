open Focus
open Grammar

type syntagm_ctx = 
  | Rules1 of rules_ctx * production list 
  | Grammar1 of grammar_ctx * rules list 

and symbol_ctx = 
  | ProductionX of symbol list_ctx * production_ctx 

and production_ctx =
  | Rules2X of syntagm * production list_ctx * rules_ctx 

and rules_ctx = 
  | Grammar2X of syntagm * rules list_ctx * grammar_ctx

and grammar_ctx = 
  | Root


type focus = 
  | GrammarFocus of grammar * grammar_ctx
  | RulesFocus of rules * rules_ctx
  | ProductionFocus of production * production_ctx 
  | SyntagmFocus of syntagm * syntagm_ctx 
  | SymbolFocus of symbol * symbol_ctx 

let rec grammar_of_focus : focus -> grammar = function
  | GrammarFocus(g, Root) -> g
  | RulesFocus(r, Grammar2X(s, r_ctx, ctx)) -> grammar_of_focus (GrammarFocus(Grammar(s, Focus.list_of_ctx r r_ctx),ctx))
  | ProductionFocus(p, Rules2X(s, p_ctx, ctx)) -> grammar_of_focus (RulesFocus(Rules(s, Focus.list_of_ctx p p_ctx),ctx))
  | SyntagmFocus(s, Rules1(ctx, p)) -> grammar_of_focus (RulesFocus(Rules(s,p),ctx))
  | SyntagmFocus(s, Grammar1(ctx, r)) -> grammar_of_focus (GrammarFocus(Grammar(s,r),ctx) )
  | SymbolFocus(s, ProductionX(s_ctx, ctx)) -> grammar_of_focus (ProductionFocus(Production(Focus.list_of_ctx s s_ctx),ctx)) 

let focus_up : focus -> (focus*path) option = function
  | GrammarFocus(g, Root) -> None
  | RulesFocus(r, Grammar2X(s, r_ctx, ctx)) -> Some(GrammarFocus(Grammar(s, Focus.list_of_ctx r r_ctx),ctx), down_rights (List.length (fst r_ctx)) )
  | ProductionFocus(p, Rules2X(s, p_ctx, ctx)) -> Some(RulesFocus(Rules(s, Focus.list_of_ctx p p_ctx),ctx), down_rights (List.length (fst p_ctx)))
  | SyntagmFocus(s, Rules1(ctx, p)) -> Some(RulesFocus(Rules(s,p),ctx), down_rights 0)
  | SyntagmFocus(s, Grammar1(ctx, r)) -> Some(GrammarFocus(Grammar(s,r),ctx), down_rights 0) 
  | SymbolFocus(s, ProductionX(s_ctx, ctx)) -> Some(ProductionFocus(Production(Focus.list_of_ctx s s_ctx),ctx), down_rights (List.length (fst s_ctx)))



let focus_down (f:focus) : focus option = 

let focus_left (f:focus) : focus option = 

let focus_right (f:focus) : focus option = 























type binary_tree =
Nil
| Cons of binary_tree * binary_tree

type binary_path =
Top
| Left of binary_path * binary_tree
| Right of binary_tree * binary_path
type binary_location = Loc of binary_tree * binary_path
let change (Loc(_,p)) t = Loc(t,p)

let go_left (Loc(t,p)) = match p with
Top -> failwith "left of top"
| Left(father,right) -> failwith "left of Left"
| Right(left,father) -> Loc(left,Left(father,t))
let go_right (Loc(t,p)) = match p with
Top -> failwith "right of top"
| Left(father,right) -> Loc(right,Right(t,father))
| Right(left,father) -> failwith "right of Right"
let go_up (Loc(t,p)) = match p with
Top -> failwith "up of top"
| Left(father,right) -> Loc(Cons(t,right),father)
| Right(left,father) -> Loc(Cons(left,t),father)
let go_first (Loc(t,p)) = match t with
Nil -> failwith "first of Nil"
| Cons(left,right) -> Loc(left,Left(p,right))
let go_second (Loc(t,p)) = match t with
Nil -> failwith "second of Nil"
| Cons(left,right) -> Loc(right,Right(left,p))