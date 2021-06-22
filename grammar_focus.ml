open Focus
open Grammar

type word_list = (Grammar.token array) list [@@ deriving yojson]

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


type grammar_focus = 
  | GrammarFocus of grammar * grammar_ctx
  | RulesFocus of rules * rules_ctx
  | ProductionFocus of production * production_ctx 
  | SyntagmFocus of syntagm * syntagm_ctx (* apparaît toujours à gauche *)
  | SymbolFocus of symbol * symbol_ctx (* apparait toujours à droite *)

type focus = {grammar_focus:grammar_focus; data: word_list}

let rec grammar_of_focus : focus -> grammar = function
  | {grammar_focus = GrammarFocus(g, Root)} -> g
  | {grammar_focus = RulesFocus(r, Grammar2X(s, r_ctx, ctx))} -> grammar_of_focus ({grammar_focus = GrammarFocus(Grammar(s, Focus.list_of_ctx r r_ctx),ctx); data = []})
  | {grammar_focus = ProductionFocus(p, Rules2X(s, p_ctx, ctx))} -> grammar_of_focus ({grammar_focus = RulesFocus(Rules(s, Focus.list_of_ctx p p_ctx),ctx); data = []})
  | {grammar_focus = SyntagmFocus(s, Rules1(ctx, p))} -> grammar_of_focus ({grammar_focus = RulesFocus(Rules(s,p),ctx); data = []})
  | {grammar_focus = SyntagmFocus(s, Grammar1(ctx, r))} -> grammar_of_focus ({grammar_focus = GrammarFocus(Grammar(s,r),ctx); data = []} )
  | {grammar_focus = SymbolFocus(s, ProductionX(s_ctx, ctx))} -> grammar_of_focus ({grammar_focus = ProductionFocus(Production(Focus.list_of_ctx s s_ctx),ctx); data = []}) 

let focus_up : focus -> (focus*path) option = function
  | {grammar_focus; data} -> 
    begin match grammar_focus with 
      | GrammarFocus(g, Root) -> None
      | RulesFocus(r, Grammar2X(s, r_ctx, ctx)) -> Some({grammar_focus=GrammarFocus(Grammar(s, Focus.list_of_ctx r r_ctx),ctx); data}, down_rights (List.length (fst r_ctx) + 1) )
      | ProductionFocus(p, Rules2X(s, p_ctx, ctx)) -> Some({grammar_focus=RulesFocus(Rules(s, Focus.list_of_ctx p p_ctx),ctx);data}, down_rights (List.length (fst p_ctx) + 1))
      | SyntagmFocus(s, Rules1(ctx, p)) -> Some({grammar_focus=RulesFocus(Rules(s,p),ctx);data}, down_rights 0)
      | SyntagmFocus(s, Grammar1(ctx, r)) -> Some({grammar_focus=GrammarFocus(Grammar(s,r),ctx);data}, down_rights 0) 
      | SymbolFocus(s, ProductionX(s_ctx, ctx)) -> Some({grammar_focus=ProductionFocus(Production(Focus.list_of_ctx s s_ctx),ctx);data}, down_rights (List.length (fst s_ctx)))
    end

let grammar_data_path_of_focus (foc : focus) : grammar * word_list * path =
  let rec aux foc path =
    match focus_up foc, foc with
    | None, {grammar_focus=GrammarFocus (g,Root);data} -> g, data, path
    | None, _ -> assert false
    | Some (foc',path'), _ -> aux foc' (path'@path) in
  aux foc []

let rec focus_of_path_focus : path * focus -> focus (* raises Invalid_path *) = function
 | [], f -> f
 | DOWN::RIGHT::p, {grammar_focus=GrammarFocus(Grammar(s, h::t),ctx); data}-> focus_of_path_focus (p, {grammar_focus=RulesFocus(h, Grammar2X(s,([],t),ctx));data}) 
 | DOWN::p, {grammar_focus=GrammarFocus(Grammar(s, r),ctx);data} -> focus_of_path_focus (p, {grammar_focus=SyntagmFocus(s, Grammar1(ctx,r));data}) 
 | RIGHT::p, {grammar_focus=RulesFocus(r, Grammar2X(s, (ll, h::rr), ctx));data} -> focus_of_path_focus (p, {grammar_focus=RulesFocus(h, Grammar2X(s, (r::ll, rr), ctx));data}) 
 | DOWN::RIGHT::p, {grammar_focus=RulesFocus(Rules(s, h::prods), ctx);data} -> focus_of_path_focus (p, {grammar_focus=ProductionFocus(h, Rules2X(s,([], prods),ctx));data})
 | DOWN::p, {grammar_focus=RulesFocus(Rules(s, prods), ctx);data} -> focus_of_path_focus (p, {grammar_focus=SyntagmFocus(s, Rules1(ctx, prods));data})
 | RIGHT::p, {grammar_focus=ProductionFocus(prod, Rules2X(s, (ll, h::rr), ctx));data} -> focus_of_path_focus (p, {grammar_focus=ProductionFocus(h, Rules2X(s, (prod::ll, rr), ctx));data})
 | DOWN::p, {grammar_focus=ProductionFocus(Production(h::t), ctx);data} -> focus_of_path_focus (p, {grammar_focus=SymbolFocus(h, ProductionX(([], t), ctx));data})
 | RIGHT::p, {grammar_focus=SymbolFocus(s, ProductionX((ll, h::rr), ctx));data} -> focus_of_path_focus (p, {grammar_focus=SymbolFocus(h, ProductionX((s::ll, rr), ctx));data})
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
        try Some (focus_of_path_focus (List.rev path'', foc'))
        with Invalid_path -> None

let focus_of_grammar_data_path (g,d,p) = focus_of_path_focus (p,({grammar_focus = GrammarFocus(g, Root); data = d}))

let initial_words =         [
        [| "b" ; "a" ; "+" ; "(" ; "a" ; "*" ; "a"; ")"; "w"|];
        [| "a" ; "+" ; "a"|];
        [| "b"; "-" ; "(" ; "a" ; "/" ; "a" ; "+" ; "a" ; ")" |]
        ]

let initial_focus = {grammar_focus = GrammarFocus(Grammar.initial_grammar, Root); 
                     data = initial_words}

let focus_to_yojson (foc : focus) : Yojson.Safe.t =
  let g, data, path = grammar_data_path_of_focus foc in
  `Assoc [ "grammar", Grammar.grammar_to_yojson g;
     "data", word_list_to_yojson data;
	   "path", Focus.path_to_yojson path ]

let focus_of_yojson (x : Yojson.Safe.t) : (focus,string) Result.result =
  match x with
    | `Assoc ["grammar", x_g; "data", x_d;"path", x_path] ->
     Result.bind (Grammar.grammar_of_yojson x_g)
      (fun g ->  
        Result.bind (word_list_of_yojson x_d)
        (fun d -> 
          Result.bind  (Focus.path_of_yojson x_path)
              (fun path -> let foc = focus_of_grammar_data_path (g,d,path) in Result.Ok foc))
        )
  | _ -> Result.Error "Invalid serialization of a focus"

let rec focus_succ (foc : focus) : focus option =
  match focus_right foc with
  | Some foc' -> Some foc'
  | None -> focus_succ_aux foc

and focus_succ_aux foc =
  match focus_down foc with
  | Some foc' -> Some foc'
  | None ->
     match focus_up foc with
     | Some (foc',_) -> focus_succ foc'
     | None -> None

let rec focus_pred (foc : focus) : focus option =
  match focus_left foc with
  | Some foc' -> Some foc'
  | None ->
     match focus_up foc with
     | Some (foc',_) -> Some foc'
     | None -> None
(*
and focus_pred_down_rightmost foc =
  match focus_down foc with
  | None -> Some foc
  | Some foc' -> focus_pred_rightmost foc'

and focus_pred_rightmost foc =
  match focus_right foc with
  | Some foc' -> focus_pred_rightmost foc'
  | None -> focus_pred_down_rightmost foc
*)
let rec delete (foc : focus) : focus option = match foc with
  | {grammar_focus;data} -> 
    begin match grammar_focus with 
      | GrammarFocus (_, ctx) -> begin match ctx with 
        | Root -> None
      end
      | RulesFocus (_, ctx) -> begin match ctx with 
        | Grammar2X (s, r_ctx, ctx') -> Some ({grammar_focus=GrammarFocus(Grammar(s, Focus.list_of_ctx_none r_ctx),ctx');data})
      end
      | ProductionFocus (_, ctx) -> begin match ctx with 
        | Rules2X (s, p_ctx, ctx') -> Some ({grammar_focus=RulesFocus(Rules(s, Focus.list_of_ctx_none p_ctx), ctx');data})
      end 
      | SymbolFocus (_, ctx) -> begin match ctx with 
        | ProductionX (sl, ctx') -> Some ({grammar_focus=ProductionFocus(Production(Focus.list_of_ctx_none sl), ctx');data})
      end 
      | SyntagmFocus (_, ctx) -> begin match ctx with 
        (** quand on supprime la variable de règles, on supprime toutes les productions associées à cette variable  *)
        | Rules1(ctx', pl) -> begin match ctx' with 
          | Grammar2X (s, (ll,rr), ctx'') -> begin match ll, rr with 
            | [], [] -> Some ({grammar_focus=GrammarFocus(Grammar(s, []), ctx'');data})
            | [], Rules(s', p) :: t ->  Some ({grammar_focus=RulesFocus(Rules(s', p), Grammar2X(s,([], t), ctx''));data})
            | Rules(s', p) :: t, rr ->  Some ({grammar_focus=RulesFocus(Rules(s', p), Grammar2X(s, (t, rr), ctx''));data})
          end
        end
        (** quand on supprime l'axiome de la grammaire, on choisit par défaut la variable qui apparait en premier pour le remplacer *)
        | Grammar1 (ctx', rl) -> begin match Focus.focus_list_of_list rl with 
          | [] -> None 
          | (Rules(s, pl), _) :: t -> Some ({grammar_focus=GrammarFocus(Grammar(s, rl), ctx');data})
        end
      end
    end 

type transf =
  | FocusUp
  | FocusRight
  | Delete
  | InputFileString of (string * string) input
  | InsertRule of syntagm input
  | InsertProduction
  | InsertSymbol

let rec apply_transf (transf : transf) (foc : focus) : focus option =
  match transf with
  | FocusUp -> Option.map fst (focus_up foc)
  | FocusRight -> focus_right foc
  | Delete -> delete foc
  | InputFileString(input) -> 
     (*let filename, contents = input#get in ;*) Some(foc)
  | InsertRule(input) -> Some(foc)
  | InsertProduction -> Some(foc)
  | InsertSymbol -> Some(foc)
