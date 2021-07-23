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

type focus = {grammar_focus:grammar_focus option; data: word_list}

exception No_grammar 

let rec grammar_of_focus : focus -> grammar = function
  | {grammar_focus = Some(GrammarFocus(g, Root))} -> g
  | {grammar_focus = Some(RulesFocus(r, Grammar2X(s, r_ctx, ctx)))} -> grammar_of_focus ({grammar_focus = Some(GrammarFocus(Grammar(s, Focus.list_of_ctx r r_ctx),ctx)); data = []})
  | {grammar_focus = Some(ProductionFocus(p, Rules2X(s, p_ctx, ctx)))} -> grammar_of_focus ({grammar_focus = Some(RulesFocus(Rules(s, Focus.list_of_ctx p p_ctx),ctx)); data = []})
  | {grammar_focus = Some(SyntagmFocus(s, Rules1(ctx, p)))} -> grammar_of_focus ({grammar_focus = Some(RulesFocus(Rules(s,p),ctx)); data = []})
  | {grammar_focus = Some(SyntagmFocus(s, Grammar1(ctx, r)))} -> grammar_of_focus ({grammar_focus = Some(GrammarFocus(Grammar(s,r),ctx)); data = []} )
  | {grammar_focus = Some(SymbolFocus(s, ProductionX(s_ctx, ctx)))} -> grammar_of_focus ({grammar_focus = Some(ProductionFocus(Production(Focus.list_of_ctx s s_ctx),ctx)); data = []}) 
  | _ -> raise No_grammar

let focus_up : focus -> (focus*path) option = function
  | {grammar_focus = Some(grammar_focus); data} -> 
    begin match grammar_focus with 
      | GrammarFocus(g, Root) -> None
      | RulesFocus(r, Grammar2X(s, r_ctx, ctx)) -> Some({grammar_focus=Some(GrammarFocus(Grammar(s, Focus.list_of_ctx r r_ctx),ctx)); data}, down_rights (List.length (fst r_ctx) + 1) )
      | ProductionFocus(p, Rules2X(s, p_ctx, ctx)) -> Some({grammar_focus=Some(RulesFocus(Rules(s, Focus.list_of_ctx p p_ctx),ctx));data}, down_rights (List.length (fst p_ctx) + 1))
      | SyntagmFocus(s, Rules1(ctx, p)) -> Some({grammar_focus=Some(RulesFocus(Rules(s,p),ctx));data}, down_rights 0)
      | SyntagmFocus(s, Grammar1(ctx, r)) -> Some({grammar_focus=Some(GrammarFocus(Grammar(s,r),ctx));data}, down_rights 0) 
      | SymbolFocus(s, ProductionX(s_ctx, ctx)) -> Some({grammar_focus=Some(ProductionFocus(Production(Focus.list_of_ctx s s_ctx),ctx));data}, down_rights (List.length (fst s_ctx)))
    end
  | _ -> None 

let grammar_data_path_of_focus (foc : focus) : grammar * word_list * path =
  let rec aux foc path =
    match focus_up foc, foc with
    | None, {grammar_focus=Some(GrammarFocus(g,Root));data} -> g, data, path
    | None, _ -> assert false
    | Some (foc',path'), _ -> aux foc' (path'@path) in
  aux foc []

let rec focus_of_path_focus : path * focus -> focus (* raises Invalid_path *) = function
 | [], f -> f
 | DOWN::RIGHT::p, {grammar_focus=Some(GrammarFocus(Grammar(s, h::t),ctx)); data}-> focus_of_path_focus (p, {grammar_focus=Some(RulesFocus(h, Grammar2X(s,([],t),ctx)));data}) 
 | DOWN::p, {grammar_focus=Some(GrammarFocus(Grammar(s, r),ctx));data} -> focus_of_path_focus (p, {grammar_focus=Some(SyntagmFocus(s, Grammar1(ctx,r)));data}) 
 | RIGHT::p, {grammar_focus=Some(RulesFocus(r, Grammar2X(s, (ll, h::rr), ctx)));data} -> focus_of_path_focus (p, {grammar_focus=Some(RulesFocus(h, Grammar2X(s, (r::ll, rr), ctx)));data}) 
 | DOWN::RIGHT::p, {grammar_focus=Some(RulesFocus(Rules(s, h::prods), ctx));data} -> focus_of_path_focus (p, {grammar_focus=Some(ProductionFocus(h, Rules2X(s,([], prods),ctx)));data})
 | DOWN::p, {grammar_focus=Some(RulesFocus(Rules(s, prods), ctx));data} -> focus_of_path_focus (p, {grammar_focus=Some(SyntagmFocus(s, Rules1(ctx, prods)));data})
 | RIGHT::p, {grammar_focus=Some(ProductionFocus(prod, Rules2X(s, (ll, h::rr), ctx)));data} -> focus_of_path_focus (p, {grammar_focus=Some(ProductionFocus(h, Rules2X(s, (prod::ll, rr), ctx)));data})
 | DOWN::p, {grammar_focus=Some(ProductionFocus(Production(h::t), ctx));data} -> focus_of_path_focus (p, {grammar_focus=Some(SymbolFocus(h, ProductionX(([], t), ctx)));data})
 | RIGHT::p, {grammar_focus=Some(SymbolFocus(s, ProductionX((ll, h::rr), ctx)));data} -> focus_of_path_focus (p, {grammar_focus=Some(SymbolFocus(h, ProductionX((s::ll, rr), ctx)));data})
 | _ , {grammar_focus = None} -> raise No_grammar
 | path, _ -> raise (Invalid_path path) 


let focus_down (foc : focus) : focus option =
  try Some (focus_of_path_focus ([DOWN], foc))
  with Invalid_path (_) -> None
    
let focus_right (foc : focus) : focus option =
  match focus_up foc with
  | None -> None
  | Some (foc',path') ->
     try Some (focus_of_path_focus ((path'@[RIGHT]), foc'))
     with Invalid_path (_) -> None

let focus_left (foc : focus) : focus option =
  match focus_up foc with
  | None -> None
  | Some (foc',path') ->
     match List.rev path' with
     | [] -> None
     | DOWN::_ -> None
     | RIGHT::path'' ->
        try Some (focus_of_path_focus (List.rev path'', foc'))
        with Invalid_path (_) -> None

let focus_of_grammar_data_path (g,d,p) = focus_of_path_focus (p,({grammar_focus = Some(GrammarFocus(g, Root)); data = d}))

let initial_words = []      

(*
let w1 = [
        [| "b" ; "a" ; "+" ; "(" ; "a" ; "*" ; "a"; ")"; "w"|];
        [| "a" ; "+" ; "a"|];
        [| "b"; "-" ; "(" ; "a" ; "/" ; "a" ; "+" ; "a" ; ")" |]
        ]
*)

let initial_focus = {grammar_focus = None; 
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
      | Some gf -> begin match gf with 
        | GrammarFocus (_, ctx) -> begin match ctx with 
          | Root -> None
        end
        | RulesFocus (_, ctx) -> begin match ctx with 
          | Grammar2X (s, r_ctx, ctx') -> Some ({grammar_focus=Some(GrammarFocus(Grammar(s, Focus.list_of_ctx_none r_ctx),ctx'));data})
        end
        | ProductionFocus (_, ctx) -> begin match ctx with 
          | Rules2X (s, p_ctx, ctx') -> begin match Focus.list_of_ctx_none p_ctx with 
            | [] -> Some ({grammar_focus=Some(RulesFocus(Rules(s, [Production([])]), ctx'));data})
            | pl -> Some ({grammar_focus=Some(RulesFocus(Rules(s, pl), ctx'));data}) end
        end 
        | SymbolFocus (_, ctx) -> begin match ctx with 
          | ProductionX (sl, ctx') -> Some ({grammar_focus=Some(ProductionFocus(Production(Focus.list_of_ctx_none sl), ctx'));data})
        end 
        | SyntagmFocus (_, ctx) -> begin match ctx with 
          (** quand on supprime la variable de règles, on supprime toutes les productions associées à cette variable  *)
          | Rules1(ctx', pl) -> begin match ctx' with 
            | Grammar2X (s, (ll,rr), ctx'') -> begin match ll, rr with 
              | [], [] -> Some ({grammar_focus=Some(GrammarFocus(Grammar(s, []), ctx''));data})
              | [], Rules(s', p) :: t ->  Some ({grammar_focus=Some(RulesFocus(Rules(s', p), Grammar2X(s,([], t), ctx'')));data})
              | Rules(s', p) :: t, rr ->  Some ({grammar_focus=Some(RulesFocus(Rules(s', p), Grammar2X(s, (t, rr), ctx'')));data})
            end
          end
          (** quand on supprime l'axiome de la grammaire, on choisit par défaut la variable qui apparait en premier pour le remplacer *)
          | Grammar1 (ctx', rl) -> begin match Focus.focus_list_of_list rl with 
            | [] -> None 
            | (Rules(s, pl), _) :: t -> Some ({grammar_focus=Some(GrammarFocus(Grammar(s, rl), ctx'));data})
          end
        end
      end
      | None -> None
    end 

type transf =
  | FocusUp
  | FocusRight
  | Delete
  | ClearGrammar
  | SetWords of ((string * string) input * (string input))
  | AddWords of ((string * string) input * (string input))
  | InputWordSeparator of (string input * (string input))
  | ClearWords
  | InsertRule of syntagm input
  | ChangeSyntagm of (syntagm list *(syntagm input))
  | InsertProduction of (string list * (string input))
  | InsertSymbolBefore of (string list * (string input))
  | InsertSymbolAfter of (string list * (string input))
  | PutInVariable of (syntagm list *(syntagm input)) * (syntagm input )
  | Copy 
  | SetSymbol of (syntagm list *(syntagm input)) 
  | NameAxiom of syntagm input
  | ExpandBefore of (string list * (string input))
  | ExpandAfter of (string list * (string input))
  | PutSuffixInVariable of (syntagm list *(syntagm input)) * (syntagm input )

(*
class symbol_frequency_table = 
object 
    val table : (symbol, Focus.frequency) Hashtbl.t = Hashtbl.create 20

    method getFrequency (s:symbol) : frequency = Hashtbl.find table s
    method setFrequency (s:symbol) (f:frequency) : unit = Hashtbl.replace table s f 
end
*)

exception No_selection 

let word_of_string (s:string) (sep:string) : Grammar.token array = 
  Array.of_list (Str.split (Str.regexp sep) s)

let words_of_file (contents:string) (sep:string) : word_list = 
  let sl = Str.split (Str.regexp_string "\n") contents in 
  List.map (fun s -> word_of_string s sep) sl 

let initial_select = "Choose between"

let in_data (s:string) (d : word_list) = List.exists (fun w -> Array.exists (fun t -> t = s) w ) d 

let regexp_token = Str.regexp_string "token "
let regexp_syntagm = Str.regexp_string "syntagm "

let rec change_syntagm (foc:grammar_focus) (s:syntagm) : grammar_focus option = match foc with
  | GrammarFocus(Grammar(_,rl), ctx) -> Some(GrammarFocus(Grammar(s,rl),ctx))
  | RulesFocus(Rules(_,pl), Grammar2X(s',ll_rr, ctx')) -> 
    begin match List.fold_left 
      (fun accu r' -> 
        match accu,r' with 
          | (ll,None,rr), Rules(s'', pl') when s'' = s -> (ll,Some(Rules(s,pl@pl')),rr)
          | (ll,None, rr), _ -> (r'::ll, None, rr)
          | (ll, Some(r''), rr), _ -> (ll, Some(r''), r'::rr)) 
      ([],None,[]) (Focus.list_of_ctx_none ll_rr) with 
      | (ll, Some(r'), rr) -> Some(RulesFocus(r', Grammar2X(s', (ll,rr), ctx')))
      | _ -> Some(foc)
    end 
  | ProductionFocus(p, ctx) -> 
    let newctx =  match ctx with 
      | Rules2X(s', (ll_rr), Grammar2X(axiom, ll_rr', ctx'')) when s' <> s -> 
            let Rules(_, pl), newctx' =  match 
            List.fold_left 
            (fun accu r -> match accu, r with 
              | (ll, None, rr) , Rules(s'', pl) when s'' = s -> (ll, Some(Rules(s'', pl)), rr)
              | (ll, None, rr), _ -> (r::ll, None, rr)
              | (ll, Some(r'), rr), _ -> (ll, Some(r'), r::rr) ) 
            ([],None, []) (Focus.list_of_ctx (Rules(s', Focus.list_of_ctx_none ll_rr)) ll_rr') with 
            | (ll, Some(r), rr) -> r, Grammar2X(axiom, (ll,rr), ctx'') 
            | _ -> assert false 
            in Rules2X(s, ([],pl), newctx')
      | _ -> ctx
    in Some(ProductionFocus(p,newctx))
  | SyntagmFocus(s', ctx) ->
    begin match ctx with 
      | Grammar1(ctx',rl) -> Some(SyntagmFocus(s,ctx)) 
      | Rules1(ctx', pl) -> 
        begin match change_syntagm (RulesFocus(Rules(s', pl), ctx')) s with 
          | Some(RulesFocus(Rules(_, pl'), ctx'')) -> Some(SyntagmFocus(s, Rules1(ctx'', pl')))
          | _ -> assert false  
        end
    end
  | _ -> assert false 

and put_in_variable foc syn syn' = match foc with 
    | RulesFocus(Rules(s,pl), ctx) when s = "" -> 
      begin try 
        let s', nctx = 
        if syn <> initial_select then 
            syn, ctx
        else if syn' <> "" then 
          match ctx with | Grammar2X(axiom, (ll,rr), ctx') ->
            match List.exists (fun r -> match r with | Rules(s'',_) when s'' = syn' -> true | _  -> false) (Focus.list_of_ctx_none (ll,rr)) with 
              | true -> syn', ctx
              | false -> syn', Grammar2X(axiom, (ll,rr@[Rules(syn', [])]), ctx') (* le syntagme n'existait pas donc on l'ajoute *)
        else raise No_selection in  
        let axiom, ll_rr, ctx' = match nctx with Grammar2X(axiom,ll_rr,ctx') -> axiom, ll_rr, ctx' in 
        begin match List.fold_left 
          (fun accu r' -> 
            match accu,r' with 
              | (ll,None,rr), Rules(s'', pl') when s'' = s' -> (ll,Some(Rules(s',pl'@pl)),rr)
              | (ll,None, rr), _ -> (r'::ll, None, rr)
              | (ll, Some(r''), rr), _ -> (ll, Some(r''), r'::rr)) 
          ([],None,[]) (Focus.list_of_ctx (Rules(s, [Production([Var s'])])) (ll_rr)) with 
          | (ll, Some(r'), rr) -> Some(RulesFocus(r', Grammar2X(axiom, (ll,rr), ctx')))
          | _ -> Some(foc)
        end  
      with | No_selection -> Jsutils.firebug "no selection made"; Some foc end
    
    | ProductionFocus(p, Rules2X(s', (ll,rr), ctx')) ->  
      begin try 
        let s, nctx' = 
        if syn <> initial_select then syn, ctx' 
        else if syn' <> "" then 
          if syn' = s' then s', ctx'
          else match ctx' with | Grammar2X(axiom, (ll',rr'), ctx'') ->
            match List.exists (fun r -> match r with | Rules(s'',_) when s'' = syn' -> true | _  -> false) (Focus.list_of_ctx_none (ll',rr')) with 
              | true -> syn', ctx' 
              | false -> syn', Grammar2X(axiom, (ll',Rules(syn', [])::rr'), ctx'') (* le syntagme n'existait pas donc on l'ajoute *)
        else raise No_selection in  
        let ll' = List.filter (fun p -> p <> Production([Var s])) ll in 
        let rr' = List.filter (fun p -> p <> Production([Var s])) rr in 
        change_syntagm (ProductionFocus(p,(Rules2X(s', (Production([Var(s)])::ll', rr'), nctx')))) s
      with | No_selection -> Jsutils.firebug "no selection made"; Some foc end
    | SymbolFocus(s, ctx) -> 
      begin try 
        let syn'', nctx = 
        if syn <> initial_select then 
          syn, ctx
        else if syn' <> "" then 
          match ctx with | ProductionX((ll,rr), ctx') -> 
            begin match ctx' with 
              | Rules2X(s', ll_rr', ctx'') when s' <> syn' -> 
                begin match ctx'' with 
                  | Grammar2X(axiom, (ll'',rr''), ctx''') -> 
                    match List.exists (fun r -> match r with | Rules(s'',_) when s'' = syn' -> true | _  -> false) (Focus.list_of_ctx_none (ll'',rr'')) with 
                     | true -> syn', ctx 
                     | false -> syn', ProductionX((ll,rr), Rules2X(s', ll_rr', Grammar2X(axiom, (ll'',Rules(syn', [])::rr''), ctx'''))) (* le syntagme n'existait pas donc on l'ajoute *)
                end
              | _ -> syn', ctx 
            end
        else 
          raise No_selection
        in 
        match nctx with | ProductionX((ll,rr), ctx') -> 
          begin match ctx' with 
            | Rules2X(s', (ll',rr'), ctx'') -> 
              let new_foc = change_syntagm (ProductionFocus(Production([s]), Rules2X(s', (Production(Focus.list_of_ctx (Var(syn'')) (ll,rr))::ll', rr'), ctx''))) syn''
              in match focus_down {grammar_focus = new_foc; data = []} with | Some({grammar_focus}) -> grammar_focus | _ -> assert false
          end
      with | No_selection -> Jsutils.firebug "no selection made"; Some foc end
    | _ -> assert false

and symbol_transf (f : Grammar.symbol -> grammar_focus option) sym data curr_foc : focus option  = 
  try              
    if sym <> initial_select then begin 
      let s = 
      if Str.string_match regexp_syntagm sym 0 then Var (Str.global_replace regexp_syntagm "" sym) 
      else Item (Str.global_replace regexp_token "" sym) in
      Some {grammar_focus=(f s);data} 
      end
    
    else raise No_selection
  with | No_selection -> Jsutils.firebug "no selection"; Some {grammar_focus=curr_foc;data}

and apply_transf (transf : transf) (foc : focus) : focus option =
  match foc with {grammar_focus;data} ->
    begin match transf, grammar_focus with
      | FocusUp, _ -> Option.map fst (focus_up foc)
      | FocusRight, _-> focus_right foc
      | Delete, _ -> delete foc
      | ClearGrammar, _ -> Some({grammar_focus = None; data})
      | SetWords (in_file, in_sep), _ -> 
        let _, contents = (in_file#get) in 
        if contents <> "" then 
          let words = words_of_file contents (in_sep#get) in Some({grammar_focus; data = words})
        else 
          Some(foc)

      | AddWords (in_file, in_sep), _ -> 
        let _, contents = (in_file#get) in 
        if contents <> "" then 
          let words = words_of_file contents (in_sep#get) in Some({grammar_focus; data = data@words})
        else 
          Some(foc)

      | InputWordSeparator (in_word, in_sep), _-> 
        let w = (in_word#get) in 
        if w <> "" then 
          let word = word_of_string w (in_sep#get) in Some({grammar_focus; data = word::data})
        else 
          Some(foc)

      | ClearWords, _ -> Some({grammar_focus; data=initial_words})

      | InsertRule in_syn, Some(gf) ->  
        let syn = (in_syn#get) in 
        if syn <> "" then 
          let Grammar(axiom, rl) = grammar_of_focus foc in 
          begin match List.exists (fun r -> match r with | Rules(s, _) when s = syn -> true | _ -> false ) rl with 
            | true -> Some(foc) (* rien  faire, la règle existe déjà *) 
            | false -> 
              let new_gf = match gf with 
                | GrammarFocus(Grammar(s,rl'), ctx) -> ProductionFocus(Production([]), Rules2X(syn, ([], []), Grammar2X(s, (List.rev rl', []), ctx)))
                | RulesFocus(r, Grammar2X(s,(ll,rr),ctx')) -> ProductionFocus(Production([]), Rules2X(syn, ([], []), Grammar2X(s, (List.rev (Focus.list_of_ctx r (ll, rr)), []), ctx')))  
                | SyntagmFocus(s, ctx) -> 
                  begin match ctx with 
                    | Grammar1(ctx', rl') -> ProductionFocus(Production([]), Rules2X(syn,([],[]), Grammar2X(s, (List.rev rl', []), ctx')))
                    | Rules1(Grammar2X(_, (ll,rr), ctx''), pl) -> ProductionFocus(Production([]),Rules2X(syn, ([],[]), Grammar2X(axiom, (List.rev (Focus.list_of_ctx (Rules(s, pl)) (ll,rr)),[]), ctx'')))
                  end 
                | SymbolFocus _ -> ProductionFocus(Production([]), Rules2X(syn, ([],[]), Grammar2X(axiom, (List.rev rl, []), Root)))
                | _ -> assert false 
              in Some({grammar_focus=Some(new_gf);data})
          end
        else Some(foc)

      | ChangeSyntagm (_, in_select), Some(gf) ->
        let s = in_select#get in 
        if s = initial_select then Some(foc) 
        else 
          let newgf = change_syntagm gf s 
          in Some({grammar_focus=newgf;data})

      | InsertProduction (symbols, in_symbols), Some(gf) -> 
        begin try 
          let sym = in_symbols#get in 
          let sym' = if sym <> initial_select then  
            if Str.string_match regexp_syntagm sym 0 then 
              Var (Str.global_replace regexp_syntagm "" sym) 
            else 
              Item (Str.global_replace regexp_token "" sym)
          else raise No_selection in 
          begin match gf with 
            | ProductionFocus(p, Rules2X(s, (ll,rr), ctx')) -> Some({grammar_focus=Some(ProductionFocus(Production([sym']), Rules2X(s, (List.rev (Focus.list_of_ctx p (ll,rr)), []), ctx')));data}) 
            | RulesFocus(Rules(s, pl), ctx) -> Some({grammar_focus=Some(ProductionFocus(Production([sym']), Rules2X(s, (List.rev pl, []), ctx)));data})
            | SyntagmFocus(s, ctx) -> 
              begin match ctx with 
                | Rules1(ctx', pl) -> Some({grammar_focus=Some(ProductionFocus(Production([sym']), Rules2X(s, (List.rev pl, []), ctx')));data})
                | Grammar1(ctx', rl) -> 
                  begin match List.fold_left 
                    (fun accu r -> match accu, r with 
                      | (ll,None, rr), Rules(s', pl) when s' = s -> (ll, Some(r), rr)
                      | (ll,None, rr), _ -> (r::ll, None, ll) 
                      | (ll,Some(r'), rr), _ ->(ll, Some(r'), r::rr)) 
                    ([], None, []) rl  with 
                    | (ll, Some(Rules(s', pl)), rr) -> Some({grammar_focus=Some(ProductionFocus(Production([sym']), Rules2X(s, (List.rev pl, []), Grammar2X(s, (ll,rr), ctx'))));data})
                    | _ -> assert false 
                  end
              end
            | SymbolFocus(_) -> begin match focus_up foc with | Some (foc',_) -> apply_transf transf foc' | _ -> assert false end 
            | _ -> assert false 
          end
        with | No_selection -> Jsutils.firebug "no selection"; Some(foc) end 

      | InsertSymbolBefore (sl, in_select), Some(gf) ->
        let sym = in_select#get in 
        begin match gf with 
          | SymbolFocus(s', ProductionX((ll,rr), ctx')) -> 
            symbol_transf (fun sym -> Some(SymbolFocus(sym, ProductionX((ll, s'::rr), ctx')))) sym data grammar_focus
          | ProductionFocus(Production(sl'), ctx) -> 
            symbol_transf (fun sym -> Some(SymbolFocus(sym, ProductionX(([], sl'), ctx)))) sym data grammar_focus
          | _ -> assert false 
        end

      | InsertSymbolAfter (sl,in_select), Some(gf) ->
        let sym = in_select#get in 
        begin match gf with 
          | SymbolFocus(s', ProductionX((ll,rr), ctx')) -> 
            symbol_transf (fun sym -> Some(SymbolFocus(sym, ProductionX((s'::ll, rr), ctx')))) sym data grammar_focus
          | ProductionFocus(Production(sl'), ctx) -> 
            symbol_transf (fun sym -> Some(SymbolFocus(sym, ProductionX((List.rev sl', []), ctx)))) sym data grammar_focus
          | _ -> assert false 
        end

      | PutInVariable ((_,in_select), in_syn), Some(gf) ->
        let syn = in_select#get in 
        let syn' = in_syn#get in 
        let newgf = put_in_variable gf syn syn' in 
        Some({grammar_focus=newgf;data})

      | Copy, Some(gf) -> 
        let newgf = 
        begin match gf with 
          | ProductionFocus(p, Rules2X(s, (ll,rr), ctx')) -> Some(ProductionFocus(p, Rules2X(s, (p::ll, rr), ctx')))
          | _ -> assert false 
        end 
        in Some({grammar_focus=newgf; data})
      
      | ExpandAfter(sl, in_select), Some(gf) ->
        let sym = in_select#get in 
        begin match gf with 
          | ProductionFocus(Production(sl), Rules2X(s, (ll,rr), ctx')) -> 
            symbol_transf (fun sym' -> Some(SymbolFocus(sym', ProductionX((List.rev sl, []), Rules2X(s, ((Production(sl))::ll, rr), ctx'))))) sym data grammar_focus
          | SymbolFocus(s, ProductionX((ll,rr), Rules2X(s', (ll',rr'),ctx'))) ->
            symbol_transf (fun sym' -> Some(SymbolFocus(sym', ProductionX((s::ll, rr), Rules2X(s', (Production(Focus.list_of_ctx s (ll,rr))::ll', rr'), ctx'))))) sym data grammar_focus
          | _ -> assert false   
        end 

      | ExpandBefore(sl, in_select), Some(gf) ->
        let sym = in_select#get in 
        begin match gf with 
          | ProductionFocus(Production(sl), Rules2X(s, (ll,rr), ctx')) -> 
            symbol_transf (fun sym' -> Some(SymbolFocus(sym', ProductionX(([], sl), Rules2X(s, ((Production(sl))::ll, rr), ctx'))))) sym data grammar_focus
          | SymbolFocus(s, ProductionX((ll,rr), Rules2X(s', (ll',rr'),ctx'))) ->
            symbol_transf (fun sym' -> Some(SymbolFocus(sym', ProductionX((ll, s::rr), Rules2X(s', (Production(Focus.list_of_ctx s (ll,rr))::ll', rr'), ctx'))))) sym data grammar_focus
          | _ -> assert false   
        end     

      | PutSuffixInVariable((_, in_select), in_syn), Some(gf) ->
        let syn = in_select#get in 
        let syn' = in_syn#get in 
        let newgf =  match gf with 
          | SymbolFocus(s, ctx) -> 
            begin try 
              let syn'', nctx = 
              if syn <> initial_select then 
                syn, ctx
              else if syn' <> "" then 
                match ctx with | ProductionX((ll,rr), ctx') -> 
                  begin match ctx' with 
                    | Rules2X(s', ll_rr', ctx'') when s' <> syn' -> 
                      begin match ctx'' with 
                        | Grammar2X(axiom, (ll'',rr''), ctx''') -> 
                          match List.exists (fun r -> match r with | Rules(s'',_) when s'' = syn' -> true | _  -> false) (Focus.list_of_ctx_none (ll'',rr'')) with 
                          | true -> syn', ctx 
                          | false -> syn', ProductionX((ll,rr), Rules2X(s', ll_rr', Grammar2X(axiom, (ll'',Rules(syn', [])::rr''), ctx'''))) (* le syntagme n'existait pas donc on l'ajoute *)
                      end
                    | _ -> syn', ctx 
                  end
              else 
                raise No_selection
              in 
              match nctx with | ProductionX((ll,rr), ctx') -> 
                begin match ctx' with 
                  | Rules2X(s', (ll',rr'), ctx'') -> 
                    let new_foc = change_syntagm (ProductionFocus(Production(s::rr), Rules2X(s', (Production(List.rev_append ll [Var(syn'')])::ll', rr'), ctx''))) syn''
                    in match focus_down {grammar_focus = new_foc; data = []} with | Some({grammar_focus}) -> grammar_focus | _ -> assert false
                end
            with | No_selection -> Jsutils.firebug "no selection made"; Some gf end
          | _ -> assert false 
        in Some({grammar_focus=newgf; data})

      | SetSymbol (sl,in_select), Some(gf) ->
          let sym = in_select#get in  
          begin match gf with 
            | SymbolFocus(_, ctx) -> symbol_transf (fun sym -> Some(SymbolFocus(sym, ctx))) sym data grammar_focus
            | _ -> assert false 
          end

      | NameAxiom i, Some(gf) -> 
        let name = i#get in 
        if name <> "" then 
          begin match gf with 
            | GrammarFocus(Grammar(axiom, rl), ctx) when axiom = "" -> Some({grammar_focus = Some (GrammarFocus(Grammar(name, List.map (fun r -> match r with | Rules(s,pl) when s = "" -> Rules(name, pl) | _ -> r) rl),ctx));data})
            | RulesFocus(Rules(s,pl), Grammar2X(_, ll_rr, ctx')) when s = "" -> Some({grammar_focus = Some(RulesFocus(Rules(name, pl), Grammar2X(name, ll_rr, ctx')));data})
            | SyntagmFocus(s, Grammar1(ctx, rl)) when s = "" -> Some({grammar_focus = Some (GrammarFocus(Grammar(name, List.map (fun r -> match r with | Rules(s,pl) when s = "" -> Rules(name, pl) | _ -> r) rl),ctx));data})
            | SyntagmFocus(s, Rules1(Grammar2X(_, ll_rr, ctx'), pl)) when s = "" -> Some({grammar_focus = Some(RulesFocus(Rules(name, pl), Grammar2X(name, ll_rr, ctx')));data})
            | _ -> assert false 
          end
        else Some(foc)

      | InsertProduction (tokens, in_tokens), None -> 
        begin try 
          let token = in_tokens#get in 
          let s = if token <> initial_select then Item(Str.global_replace regexp_token "" token) else raise No_selection in 
            Some({grammar_focus=Some(ProductionFocus(Production([s]), Rules2X("", ([], []), Grammar2X("", ([],[]), Root))));data})
        with | No_selection -> Jsutils.firebug "no selection"; Some(foc) end 

      | _ -> assert false
    end 
