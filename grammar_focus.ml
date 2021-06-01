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
  | SyntagmFocus of syntagm * syntagm_ctx (* apparaît toujours à gauche *)
  | SymbolFocus of symbol * symbol_ctx (* apparait toujours à droite *)

let rec grammar_of_focus : focus -> grammar = function
  | GrammarFocus(g, Root) -> g
  | RulesFocus(r, Grammar2X(s, r_ctx, ctx)) -> grammar_of_focus (GrammarFocus(Grammar(s, Focus.list_of_ctx r r_ctx),ctx))
  | ProductionFocus(p, Rules2X(s, p_ctx, ctx)) -> grammar_of_focus (RulesFocus(Rules(s, Focus.list_of_ctx p p_ctx),ctx))
  | SyntagmFocus(s, Rules1(ctx, p)) -> grammar_of_focus (RulesFocus(Rules(s,p),ctx))
  | SyntagmFocus(s, Grammar1(ctx, r)) -> grammar_of_focus (GrammarFocus(Grammar(s,r),ctx) )
  | SymbolFocus(s, ProductionX(s_ctx, ctx)) -> grammar_of_focus (ProductionFocus(Production(Focus.list_of_ctx s s_ctx),ctx)) 

let focus_up : focus -> (focus*path) option = function
  | GrammarFocus(g, Root) -> None
  | RulesFocus(r, Grammar2X(s, r_ctx, ctx)) -> Some(GrammarFocus(Grammar(s, Focus.list_of_ctx r r_ctx),ctx), down_rights (List.length (fst r_ctx) + 1) )
  | ProductionFocus(p, Rules2X(s, p_ctx, ctx)) -> Some(RulesFocus(Rules(s, Focus.list_of_ctx p p_ctx),ctx), down_rights (List.length (fst p_ctx) + 1))
  | SyntagmFocus(s, Rules1(ctx, p)) -> Some(RulesFocus(Rules(s,p),ctx), down_rights 0)
  | SyntagmFocus(s, Grammar1(ctx, r)) -> Some(GrammarFocus(Grammar(s,r),ctx), down_rights 0) 
  | SymbolFocus(s, ProductionX(s_ctx, ctx)) -> Some(ProductionFocus(Production(Focus.list_of_ctx s s_ctx),ctx), down_rights (List.length (fst s_ctx)))

let grammar_path_of_focus (foc : focus) : grammar * path =
  let rec aux foc path =
    match focus_up foc, foc with
    | None, GrammarFocus (g,Root) -> g, path
    | None, _ -> assert false
    | Some (foc',path'), _ -> aux foc' (path'@path) in
  aux foc []

let rec focus_of_path_focus : path * focus -> focus (* raises Invalid_path *) = function
 | [], f -> f
 | DOWN::RIGHT::p, GrammarFocus(Grammar(s, h::t),ctx)-> focus_of_path_focus (p, RulesFocus(h, Grammar2X(s,([],t),ctx))) 
 | DOWN::p, GrammarFocus(Grammar(s, r),ctx) -> focus_of_path_focus (p, SyntagmFocus(s, Grammar1(ctx,r))) 
 | RIGHT::p, RulesFocus(r, Grammar2X(s, (ll, h::rr), ctx)) -> focus_of_path_focus (p, RulesFocus(h, Grammar2X(s, (r::ll, rr), ctx))) 
 | DOWN::RIGHT::p, RulesFocus(Rules(s, h::prods), ctx) -> focus_of_path_focus (p, ProductionFocus(h, Rules2X(s,([], prods),ctx)))
 | DOWN::p, RulesFocus(Rules(s, prods), ctx) -> focus_of_path_focus (p, SyntagmFocus(s, Rules1(ctx, prods)))
 | RIGHT::p, ProductionFocus(prod, Rules2X(s, (ll, h::rr), ctx)) -> focus_of_path_focus (p, ProductionFocus(h, Rules2X(s, (prod::ll, rr), ctx)))
 | DOWN::p, ProductionFocus(Production(h::t), ctx) -> focus_of_path_focus (p, SymbolFocus(h, ProductionX(([], t), ctx)))
 | RIGHT::p, SymbolFocus(s, ProductionX((ll, h::rr), ctx)) -> focus_of_path_focus (p, SymbolFocus(h, ProductionX((s::ll, rr), ctx)))
 | _ -> raise Invalid_path


let focus_down (foc : focus) : focus option =
  try Some (focus_of_path_focus ([DOWN], foc))
  with Invalid_path -> None
    
let focus_right (foc : focus) : focus option =
  match focus_up foc with
  | None -> None
  | Some (foc',path') ->
     try Some (focus_of_path_focus ((path'@[RIGHT]), foc'))
     with Invalid_path -> None

let focus_left (foc : focus) : focus option =
  match focus_up foc with
  | None -> None
  | Some (foc',path') ->
     match List.rev path' with
     | [] -> None
     | DOWN::_ -> None
     | RIGHT::path'' ->
        try Some (focus_of_path_focus (path'', foc'))
        with Invalid_path -> None


let initial_focus = let g = Grammar("Z", [
  Rules("Z", [Production([Var("U"); Var("V")])]);
  Rules("U", [Production([Item("a")]);Production([Item("b")])]);
  Rules("V", [])
  ]) in GrammarFocus(g , Root)

let focus_to_yojson (foc : focus) : Yojson.Safe.t =
  let g, path = grammar_path_of_focus foc in
  `Assoc [ "grammar", Grammar.grammar_to_yojson g;
	   "path", Focus.path_to_yojson path ]

let focus_of_yojson (x : Yojson.Safe.t) : (focus,string) Result.result =
  match x with
  | _ -> Result.Error "Invalid serialization of a focus"

let rec focus_succ (foc : focus) : focus option =
  match focus_down foc with
  | Some foc' -> Some foc'
  | None -> focus_succ_aux foc
and focus_succ_aux foc =
  match focus_right foc with
  | Some foc' -> Some foc'
  | None ->
     match focus_up foc with
     | Some (foc',_) -> focus_succ_aux foc'
     | None -> None

let rec focus_pred (foc : focus) : focus option =
  match focus_left foc with
  | Some foc' -> focus_pred_down_rightmost foc'
  | None ->
     match focus_up foc with
     | Some (foc',_) -> Some foc'
     | None -> None
and focus_pred_down_rightmost foc =
  match focus_down foc with
  | None -> Some foc
  | Some foc' -> focus_pred_rightmost foc'
and focus_pred_rightmost foc =
  match focus_right foc with
  | Some foc' -> focus_pred_rightmost foc'
  | None -> focus_pred_down_rightmost foc

let rec delete (foc : focus) : focus option = match foc with
  | GrammarFocus (_, ctx) -> begin match ctx with 
    | Root -> None
  end
  | RulesFocus (_, ctx) -> begin match ctx with 
    | Grammar2X (s, r_ctx, ctx') -> Some (GrammarFocus(Grammar(s, Focus.list_of_ctx_none r_ctx),ctx'))
  end
  | ProductionFocus (_, ctx) -> begin match ctx with 
    | Rules2X (s, p_ctx, ctx') -> Some (RulesFocus(Rules(s, Focus.list_of_ctx_none p_ctx), ctx'))
  end 
  | SymbolFocus (_, ctx) -> begin match ctx with 
    | ProductionX (sl, ctx') -> Some (ProductionFocus(Production(Focus.list_of_ctx_none sl), ctx'))
  end 
  | SyntagmFocus (_, ctx) -> begin match ctx with 
    (** quand on supprime la variable de règles, on supprime toutes les productions associées à cette variable  *)
    | Rules1(ctx', pl) -> begin match ctx' with 
      | Grammar2X (s, (ll,rr), ctx'') -> begin match ll, rr with 
        | [], [] -> Some (GrammarFocus(Grammar(s, []), ctx''))
        | [], Rules(s', p) :: t ->  Some (RulesFocus(Rules(s', p), Grammar2X(s,([], t), ctx'')))
        | Rules(s', p) :: t, rr ->  Some (RulesFocus(Rules(s', p), Grammar2X(s, (t, rr), ctx'')))
      end
    end
    (** quand on supprime l'axiome de la grammaire, on choisit par défaut la variable qui apparait en premier pour le remplacer *)
    | Grammar1 (ctx', rl) -> begin match Focus.focus_list_of_list rl with 
      | [] -> None 
      | (Rules(s, pl), _) :: t -> Some (GrammarFocus(Grammar(s, rl), ctx'))
    end
  end 
