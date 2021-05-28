open Syntax
open Grammar
open Grammar_focus

(* syntax definition *)
type word = [`Bool of bool | `Int of int | `Float of float | `String of string | `Filename of string | `Var of var | `Func of string | `ContextItem | `ContextEnv | `Order of order | `TheFocus | `Ellipsis ]
type input = [ `Int of int Focus.input
	     | `Float of float Focus.input
	     | `String of string Focus.input
	     | `Ident of string Focus.input
	     | `FuncSig of (string * string list) Focus.input
	     | `Select of string list * string Focus.input
	     | `FileString of (string * string) Focus.input ]

type syn = (word,input,focus) xml

let rec syn_list ~limit (f : 'a -> syn) (l : 'a list) : syn list =
  match l with
  | [] -> []
  | x::r ->
     if limit=0
     then [ [Kwd "..."] ]
     else f x :: syn_list ~limit:(limit-1) f r
		      
let syn_syntagm (s:syntagm) : syn = [`Word s]
let syn_token (t:token) : syn =  [`Word t]
let syn_symbol : symbol -> syn = function 
    | Item(t) -> syn_token t 
    | Var(v) -> syn_syntagm v

let syn_production : production -> syn = function 
    | Production(p) -> [Enum("", syn_list ~limit:10 (syn_symbol) p)]

let syn_rules : rules -> syn = function 
    | Rules(s, p) -> [Quote("| ", [syn_syntagm s ; Kwd " -> " ; Enum("|"), syn_list ~limit:10 (syn_production) p], "")]
    
let syn_rule (r: rules) : syn = 
let syn_grammar : grammar -> syn = function 
    | Grammar(s, r) -> 
        [syn_syntagm s ; 
        Indent(Block(syn_list ~limit:20 (syn_rules) r))]  
			   
(* DERIVED *)			      
let rec syn_focus (foc : focus) : syn =
  match foc with
  | Grammar_focus(g, ctx) -> syn_grammar_ctx ctx [Highlight (syn_grammar g); ControlCurrentFocus]
  | RulesFocus(r, ctx) -> syn_rules_ctx ctx [Highlight (syn_rules r); ControlCurrentFocus]
  | ProductionFocus(p, ctx) -> syn_production_ctx ctx [Highlight (syn_production p); ControlCurrentFocus]
  | SyntagmFocus(s, ctx) -> syn_syntagm_ctx ctx [Highlight (syn_syntax s); ControlCurrentFocus]
  | SymbolFocus(s, ctx) -> syn_symbol_ctx ctx [Highlight (syn_symbol s); ControlCurrentFocus]

and syn_grammar_ctx (ctx:grammar_ctx) (s:syn) : syn = begin match ctx with    
    | Root -> s 
end
and syn_rules_ctx (ctx:rules_ctx) (s:syn) : syn = begin match ctx with 
    | Grammar2X(s', (ll, rr), ctx') -> 
        syn_grammar_ctx s ([syn_syntagm s' ; 
        Indent(Block((syn_list ~limit:20 (syn_rules) (List.rev ll)) @ s :: (syn_list ~limit:20 (syn_rules) rr)))])
end 
and syn_production_ctx (ctx: production_ctx) (s:syn)  : syn = begin match ctx with 
    | Rules2X(s', (ll,rr), ctx') -> syn_rules_ctx ctx' ([
        Quote("| ", [syn_syntagm s' ; 
        Kwd " -> " ;
        Enum("|"), (syn_list ~limit:10 (syn_production) (List.rev ll)) @ s' :: (syn_list ~limit:10 (syn_production) rr)], "")])
end 
and syn_syntagm_ctx (ctx: syntagm_ctx) (s: syn) : syn = begin match ctx with 
    | Rules1(ctx', p) -> syn_rules_ctx ctx' ([Quote("| ",[s ; Kwd " -> " ; Enum("|"), syn_list ~limit:10 (syn_production) p], "")])
    | Grammar1(ctx', r) -> syn_grammar_ctx ctx' ([s ;  Indent(Block(syn_list ~limit:20 (syn_rules) r))])
end
and syn_symbol_ctx (ctx: symbol_ctx) (s:syn) ; syn = begin match ctx with
    | ProductionX((ll, rr)) -> [Enum("", (syn_list ~limit:10 (syn_symbol) (List.rev ll)) @ s :: (syn_list ~limit:10 (syn_symbol) rr))]
end
