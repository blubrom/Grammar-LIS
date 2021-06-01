open Syntax
open Grammar
open Grammar_focus

(* syntax definition *)
type word = [ `Var of Grammar.syntagm 
            | `Token of Grammar.token ]
type input = [ ]

type syn = (word,input,focus) xml

let rec syn_list ~limit (f : 'a -> syn) (l : 'a list) : syn list =
  match l with
  | [] -> []
  | x::r ->
     if limit=0
     then [ [Kwd "..."] ]
     else f x :: syn_list ~limit:(limit-1) f r
		      
let syn_syntagm (ctx:syntagm_ctx) (s:syntagm) : syn = [Focus(SyntagmFocus(s,ctx), [Word(`Var s)])]
let syn_token (t:token) : syn =  [Word(`Token t)]
let syn_symbol (ctx : symbol_ctx) : symbol -> syn = function 
    | Item(t) -> [Focus(SymbolFocus(Item(t), ctx), syn_token t)] 
    | Var(v) -> [Focus(SymbolFocus(Var(v), ctx), [Word(`Var v)])]

let syn_production (ctx : production_ctx) : production -> syn = function 
    | Production(sl) -> [Focus(
        ProductionFocus(Production(sl), ctx), 
        [Enum("", List.map 
                  (fun (s, ll_rr) -> syn_symbol (ProductionX(ll_rr, ctx)) s) 
                  (Focus.focus_list_of_list sl))])]

let syn_rules (ctx : rules_ctx) : rules -> syn = function 
    | Rules(s, pl) -> [Focus(
        RulesFocus(Rules(s, pl), ctx), 
        [Quote("| ", syn_syntagm (Rules1(ctx, pl)) s @ [
            Kwd " -> " ; 
            Enum(" | ", List.map 
                        (fun (p, ll_rr) -> syn_production (Rules2X(s, ll_rr, ctx)) p) 
                        (Focus.focus_list_of_list pl))]
                , "")])]
    
let syn_grammar (ctx : grammar_ctx) : grammar -> syn = function 
    | Grammar(s, rl) -> [Focus(
        GrammarFocus(Grammar(s,rl), ctx),
        syn_syntagm (Grammar1(ctx, rl)) s @ [Indent([Block(
            List.map (fun (r, ll_rr) -> syn_rules (Grammar2X(s, ll_rr, ctx)) r) (Focus.focus_list_of_list rl)
        )])] 
    )]
     
			   
(* DERIVED *)			      
let rec syn_focus (foc : focus) : syn =
  match foc with
  | GrammarFocus(g, ctx) -> syn_grammar_ctx g ctx [Highlight (syn_grammar ctx g ); ControlCurrentFocus]
  | RulesFocus(r, ctx) -> syn_rules_ctx r ctx [Highlight (syn_rules ctx r); ControlCurrentFocus]
  | ProductionFocus(p, ctx) -> syn_production_ctx p ctx [Highlight (syn_production ctx p); ControlCurrentFocus]
  | SyntagmFocus(s, ctx) -> syn_syntagm_ctx s ctx [Highlight (syn_syntagm ctx s); ControlCurrentFocus]
  | SymbolFocus(s, ctx) -> syn_symbol_ctx s ctx [Highlight (syn_symbol ctx s); ControlCurrentFocus]

and syn_grammar_ctx (g:grammar) (ctx:grammar_ctx) (s:syn) : syn = 
    let  xml_g = [Focus (GrammarFocus(g,ctx), s)] in match ctx with    
        | Root -> xml_g 

and syn_rules_ctx (r:rules) (ctx:rules_ctx) (s:syn) : syn = 
    let  xml_r = [Focus (RulesFocus(r,ctx), s)] in match ctx with 
    | Grammar2X(s', (ll_rr), ctx') -> 
        let rl = Focus.list_of_ctx r ll_rr in
        syn_grammar_ctx 
            (Grammar(s', rl)) 
            ctx' 
            (syn_syntagm (Grammar1(ctx', rl)) s' @ [Indent([Block(
                (Syntax.xml_list_focus
                    (fun (r1, ll_rr1) -> syn_rules (Grammar2X(s', ll_rr1, ctx')) r1)
                    (r, ll_rr) 
                    xml_r)
            )])])

and syn_production_ctx (p:production) (ctx: production_ctx) (s:syn)  : syn = 
    let xml_p = [Focus (ProductionFocus(p, ctx), s)] in match ctx with 
    | Rules2X(s', ll_rr, ctx') -> 
        let pl = Focus.list_of_ctx p ll_rr in 
        syn_rules_ctx 
            (Rules(s', pl))
            ctx' 
            ([ Quote("| ", (syn_syntagm (Rules1(ctx', pl)) s') @ [ Kwd " -> " ;
             Enum("|", 
                (Syntax.xml_list_focus
                    (fun (p1, ll_rr1) -> syn_production (Rules2X(s', ll_rr1, ctx')) p1) 
                    (p, ll_rr) xml_p)
            )], "")])

and syn_syntagm_ctx (v:syntagm) (ctx: syntagm_ctx) (s: syn) : syn = 
    let xml_s = [Focus(SyntagmFocus(v, ctx), s)] in match ctx with 
    | Rules1(ctx', p) -> let pl = Focus.focus_list_of_list p in 
                        syn_rules_ctx 
                            (Rules(v,p)) 
                            ctx' 
                            ([Quote("| ", xml_s @ [ Kwd " -> " ; Enum("|", (List.map (fun (p', ll_rr) -> syn_production (Rules2X(v, ll_rr, ctx')) p') pl))], "")])
    | Grammar1(ctx', r) -> let rl = Focus. focus_list_of_list r in 
                            syn_grammar_ctx 
                                (Grammar(v,r)) 
                                ctx' 
                                (xml_s @ [ Indent([Block((List.map (fun (r', ll_rr) -> syn_rules (Grammar2X(v, ll_rr,ctx')) r') rl))])])

and syn_symbol_ctx (i:symbol) (ctx: symbol_ctx) (s:syn) : syn = 
    let xml_s = [Focus (SymbolFocus(i,ctx), s)] in match ctx with
    | ProductionX((ll_rr), ctx') -> 
        let sl = Focus.list_of_ctx i ll_rr in 
        syn_production_ctx 
        (Production(sl))
        ctx' 
        ([Enum("", 
            (Syntax.xml_list_focus
                (fun(i', ll_rr') -> syn_symbol (ProductionX(ll_rr', ctx')) i' )
                (i, ll_rr) xml_s)
        )])

