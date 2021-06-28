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
  | ClearGrammar
  | SetWords of ((string * string) input * (string input))
  | AddWords of ((string * string) input * (string input))
  | InputWordSeparator of (string input * (string input))
  | ClearWords
  | InsertRule of syntagm input
  | ChangeSyntagm of (syntagm list *(syntagm input))
  | InsertProduction
  | InsertSymbolBefore of (syntagm list *(syntagm input)) * (string input)
  | InsertSymbolAfter of (syntagm list *(syntagm input)) * (string input)
  | PutInVariable of (syntagm list *(syntagm input)) * (syntagm input )
  | Copy 
  | SetSymbol of (syntagm list *(syntagm input)) * string input

let word_of_string (s:string) (sep:string) : Grammar.token array = 
  Array.of_list (Str.split (Str.regexp sep) s)

let words_of_file (contents:string) (sep:string) : word_list = 
  let sl = Str.split (Str.regexp_string "\n") contents in 
  List.map (fun s -> word_of_string s sep) sl 

let initial_select = "Choose syntagm"

let in_data (s:string) (d : word_list) = List.exists (fun w -> Array.exists (fun t -> t = s) w ) d 


let rec change_syntagm (f:grammar_focus) (s:syntagm) : grammar_focus = match f with
  | GrammarFocus(g, ctx) -> begin match g with | Grammar(_,rl) -> GrammarFocus(Grammar(s,rl),ctx) end
  | RulesFocus(r, ctx) -> 
    begin match r with Rules(_,pl) ->
      begin match ctx with 
        | Grammar2X(s',ll_rr, ctx') -> 
          match List.fold_left 
            (fun accu r' -> 
              match accu,r' with 
                | (ll,None,rr), Rules(s'', pl') when s'' = s -> (ll,Some(Rules(s,pl@pl')),rr)
                | (ll,None, rr), _ -> (r'::ll, None, rr)
                | (ll, Some(r''), rr), _ -> (ll, Some(r''), r'::rr)) 
            ([],None,[]) (Focus.list_of_ctx_none ll_rr) with 
            | (ll, Some(r'), rr) -> RulesFocus(r', Grammar2X(s', (ll,rr), ctx'))
            | _ -> f 
      end
    end
  | ProductionFocus(p, ctx) -> 
    let newctx =  match ctx with 
      | Rules2X(s', (ll_rr), ctx') when s' <> s -> 
        begin match ctx' with 
          | Grammar2X(axiom, ll_rr', ctx'') -> 
            let r, newctx' =  match 
            List.fold_left 
            (fun accu r -> match accu, r with 
              | (ll, None, rr) , Rules(s'', pl) when s'' = s -> (ll, Some(Rules(s'', pl)), rr)
              | (ll, None, rr), _ -> (r::ll, None, rr)
              | (ll, Some(r'), rr), _ -> (ll, Some(r'), r::rr) ) 
            ([],None, []) (Focus.list_of_ctx (Rules(s', Focus.list_of_ctx_none ll_rr)) ll_rr') with 
            | (ll, Some(r), rr) -> r, Grammar2X(axiom, (ll,rr), ctx'') 
            | _ -> assert false 
            in match r with Rules(_, pl) ->  Rules2X(s, ([],pl), newctx')
        end
      | _ -> ctx
    in ProductionFocus(p,newctx)
  | SyntagmFocus(s', ctx) ->
    begin match ctx with 
      | Grammar1(ctx',rl) -> SyntagmFocus(s,ctx) 
      | Rules1(ctx', pl) -> 
        begin match change_syntagm (RulesFocus(Rules(s', pl), ctx')) s with 
          | RulesFocus(Rules(_, pl'), ctx'') -> SyntagmFocus(s, Rules1(ctx'', pl'))
          | _ -> assert false  
        end
    end
  | _ -> assert false 

and put_in_variable f syn syn' = match f with 
    | ProductionFocus(p, ctx) -> 
          begin try 
            begin match ctx with 
              | Rules2X(s', (ll,rr), ctx') -> 
              let s, nctx' = 
              if syn <> initial_select then syn, ctx' 
              else if syn' <> "" then 
                if syn' = s' then s', ctx'
                else match ctx' with | Grammar2X(axiom, (ll',rr'), ctx'') ->
                  match List.exists (fun r -> match r with | Rules(s'',_) when s'' = syn' -> true | _  -> false) (Focus.list_of_ctx_none (ll',rr')) with 
                    | true -> syn', ctx' 
                    | false -> syn', Grammar2X(axiom, (ll',Rules(syn', [])::rr'), ctx'') (* le syntagme n'existait pas donc on l'ajoute *)
              else failwith "no selection made" in  
              change_syntagm (ProductionFocus(p,(Rules2X(s', (Production([Var(s)])::ll, rr), nctx')))) s
            end
          with | Failure msg -> Jsutils.firebug msg; f end
    | SymbolFocus(s, ctx) -> Jsutils.firebug "là ?";
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
          failwith "no selection"
        in 
        match nctx with | ProductionX((ll,rr), ctx') -> 
          begin match ctx' with 
            | Rules2X(s', (ll',rr'), ctx'') -> 
              let new_foc = change_syntagm (ProductionFocus(Production([s]), Rules2X(s', (Production(Focus.list_of_ctx (Var(syn'')) (ll,rr))::ll', rr'), ctx''))) syn''
              in match focus_down {grammar_focus = new_foc; data = []} with | Some({grammar_focus}) -> grammar_focus | _ -> failwith "failure"
          end
      with | Failure msg -> Jsutils.firebug msg; f end
    | _ -> assert false

and get_symbol s syn data =              
  if syn <> initial_select then 
    Var(syn)
  else if s <> "" then 
    let token = in_data s data
    in  
      if token then 
        begin Jsutils.firebug "token"; Item(s) end
      else 
        begin Jsutils.firebug "syntagm"; Var(s) end
  else failwith "no selection"

and apply_transf (transf : transf) (foc : focus) : focus option =
  match foc with {grammar_focus;data} -> begin 
    match transf with
      | FocusUp -> Option.map fst (focus_up foc)
      | FocusRight -> focus_right foc
      | Delete -> delete foc
      | ClearGrammar -> Some({grammar_focus = GrammarFocus(Grammar.initial_grammar, Root); data})
      | SetWords (in_file, in_sep) -> 
        let fname, contents = (in_file#get) in 
        let words = words_of_file contents (in_sep#get) in Some({grammar_focus; data = words})
      | AddWords (in_file, in_sep) -> 
        let fname, contents = (in_file#get) in 
        let words = words_of_file contents (in_sep#get) in Some({grammar_focus; data = data@words})
      | InputWordSeparator (in_word, in_sep )-> 
        let word = word_of_string (in_word#get) (in_sep#get) in Some({grammar_focus; data = word::data})
      | ClearWords -> Some({grammar_focus; data=initial_words})
      | InsertRule in_syn ->  
        let syn = (in_syn#get) in 
        let Grammar(_, rl) = grammar_of_focus foc in 
        begin match List.exists (fun r -> match r with | Rules(s, _) when s = syn -> true | _ -> false ) rl with 
          | true -> Some(foc) (* rien  faire, la règle existe déjà *) 
          | false -> 
            begin let new_gf = match grammar_focus with 
              | GrammarFocus(g, ctx) -> begin match g with | Grammar(s,rl') -> RulesFocus(Rules(syn,[Production([])]), Grammar2X(s, (rl', []), ctx)) end
              | RulesFocus(r, ctx)-> begin match ctx with | Grammar2X(s,(ll,rr),ctx') -> RulesFocus(Rules(syn, [Production([])]), Grammar2X(s, (r::ll, rr), ctx')) end  
              | SyntagmFocus(s, ctx) -> 
                begin match ctx with 
                  | Grammar1(ctx', rl') -> RulesFocus(Rules(syn,[Production([])]), Grammar2X(s, (rl', []), ctx')) 
                  | Rules1(ctx', pl) -> 
                    begin match ctx' with 
                      | Grammar2X(axiom, (ll,rr), ctx'') -> RulesFocus(Rules(syn, [Production([])]), Grammar2X(axiom, (Focus.list_of_ctx (Rules(s, pl)) (ll,rr),[]), ctx''))
                    end 
                end 
              | _ -> assert false 
            in Some({grammar_focus=new_gf;data})
            end
        end
      | ChangeSyntagm (_, in_select) -> 
        let s = in_select#get in 
        if s = initial_select then Some(foc) 
        else 
          let newgf = change_syntagm grammar_focus s 
          in Some({grammar_focus=newgf;data})

      | InsertProduction -> 
        let new_gf = match grammar_focus with 
          | ProductionFocus(p,ctx)-> begin match ctx with | Rules2X(s, (ll,rr), ctx') -> ProductionFocus(Production([]), Rules2X(s, (p::ll, rr), ctx')) end
          | RulesFocus(r, ctx) -> begin match r with Rules(s, rl) -> RulesFocus(Rules(s, Production([])::rl), ctx) end
          | SyntagmFocus(s, ctx) -> begin match ctx with | Rules1(ctx', pl) -> SyntagmFocus(s, Rules1(ctx', Production([])::pl))
                                                        | Grammar1(ctx', rl) ->
                                                            let nrl = List.map 
                                                              (fun r -> match r with 
                                                                |Rules(s', pl) when s' = s -> Rules(s', Production([])::pl)
                                                                | _ -> r ) rl 
                                                            in SyntagmFocus(s, Grammar1(ctx', nrl))
                                    end
          | _ -> assert false 
        in Some({grammar_focus=new_gf;data})

      | InsertSymbolBefore ((_, in_select), in_string) -> 
         let syn = in_select#get in 
          let s = in_string#get in 
          let newgf = match grammar_focus with 
            | SymbolFocus(s', ProductionX((ll,rr), ctx')) -> 
              begin try 
                let sym = get_symbol syn s data in 
                SymbolFocus(sym, ProductionX((ll, s'::rr), ctx')) 
              with Failure msg -> Jsutils.firebug msg; grammar_focus end
            | ProductionFocus(Production(sl), ctx) -> 
              begin try 
                let sym = get_symbol syn s data in 
                SymbolFocus(sym, ProductionX(([], sl), ctx)) 
              with Failure msg -> Jsutils.firebug msg; grammar_focus end
            | _ -> assert false 
          in Some({grammar_focus=newgf;data})

      | InsertSymbolAfter ((_,in_select), in_string) -> 
          let syn = in_select#get in 
          let s = in_string#get in 
          let newgf = match grammar_focus with 
            | SymbolFocus(s', ProductionX((ll,rr), ctx')) -> 
              begin try 
                let sym = get_symbol syn s data in 
                SymbolFocus(sym, ProductionX((s'::ll, rr), ctx')) 
              with Failure msg -> Jsutils.firebug msg; grammar_focus end
            | ProductionFocus(Production(sl), ctx) -> 
              begin try 
                let sym = get_symbol syn s data in 
                SymbolFocus(sym, ProductionX((List.rev sl, []), ctx)) 
              with Failure msg -> Jsutils.firebug msg; grammar_focus end
            | _ -> assert false 
          in Some({grammar_focus=newgf;data})
      | PutInVariable ((_,in_select), in_syn) -> 
          let syn = in_select#get in 
          let syn' = in_syn#get in 
          let newgf = put_in_variable grammar_focus syn syn' in 
          Some({grammar_focus=newgf;data})
      | Copy -> 
        let newgf = 
        begin match grammar_focus with 
          | ProductionFocus(p,ctx) -> begin match ctx with Rules2X(s, (ll,rr), ctx') -> ProductionFocus(p, Rules2X(s, (p::ll, rr), ctx')) end
          | _ -> assert false 
        end 
        in Some({grammar_focus=newgf; data})
      | SetSymbol ((_,in_select), in_string) ->           
          let syn = in_select#get in 
          let s = in_string#get in 
          let newgf = match grammar_focus with 
            | SymbolFocus(s', ctx) -> 
              begin try 
                let sym = get_symbol syn s data in 
                SymbolFocus(sym, ctx) 
              with Failure msg -> Jsutils.firebug msg; grammar_focus end
            | _ -> assert false 
          in Some({grammar_focus=newgf;data})
  end