open Syntax
open Grammar
open Grammar_focus

(* syntax definition *)
type word = [ `Var of Grammar.syntagm 
            | `Token of Grammar.token ]
type input = [ `FileString of (string * string) Focus.input
              | `Separator of string Focus.input
              | `Word of string Focus.input
              | `SelectSyntagm of syntagm list * (syntagm Focus.input)
              | `Symbol of string Focus.input 
              | `Syntagm of syntagm Focus.input ]

type syn = (word,input,focus) xml

let rec syn_list ~limit (f : 'a -> syn) (l : 'a list) : syn list =
  match l with
  | [] -> []
  | x::r ->
     if limit=0
     then [ [Kwd "..."] ]
     else f x :: syn_list ~limit:(limit-1) f r
		      
let syn_syntagm (ctx:syntagm_ctx) (s:syntagm) (data:word_list) : syn = 
    if s = " " then 
        [Focus({grammar_focus=SyntagmFocus(s,ctx);data}, [Word(`Var "&blank;")])]
    else
        [Focus({grammar_focus=SyntagmFocus(s,ctx);data}, [Word(`Var s)])]

let syn_token (t:token) : syn =  if t = " " then [Word(`Token "&blank;")] else [Word(`Token t)]
let syn_symbol (ctx : symbol_ctx) (data:word_list) : symbol -> syn = function 
    | Item(t) -> [Focus({grammar_focus=SymbolFocus(Item(t), ctx);data}, syn_token t)] 
    | Var(v) -> [Focus({grammar_focus=SymbolFocus(Var(v), ctx);data}, [Word(`Var v)])]

let syn_production (ctx : production_ctx) (data:word_list) : production -> syn = function 
    | Production(sl) -> 
        let s' =
            if List.length sl = 0 then 
                [Kwd "&epsilon;"]
            else
                [Enum(" ", List.map 
                  (fun (s, ll_rr) -> syn_symbol (ProductionX(ll_rr, ctx)) data s) 
                  (Focus.focus_list_of_list sl))]
                            
        in [Focus( {grammar_focus=ProductionFocus(Production(sl), ctx);data}, s')]

let syn_rules (ctx : rules_ctx) (data:word_list) : rules -> syn = function 
    | Rules(s, pl) -> [Focus(
        {grammar_focus=RulesFocus(Rules(s, pl), ctx);data}, 
        [Quote("| ", (syn_syntagm (Rules1(ctx, pl)) s data) @ [
            Kwd " -> " ; 
            Enum(" | ", List.map 
                        (fun (p, ll_rr) -> syn_production (Rules2X(s, ll_rr, ctx)) data p)
                        (Focus.focus_list_of_list pl))]
                , "")])]
    
let syn_grammar (ctx : grammar_ctx) (data:word_list) : grammar -> syn = function 
    | Grammar(s, rl) -> [Focus(
        {grammar_focus=GrammarFocus(Grammar(s,rl), ctx);data},
        (syn_syntagm (Grammar1(ctx, rl)) s data) @ [Indent([Block(
            List.map (fun (r, ll_rr) -> syn_rules (Grammar2X(s, ll_rr, ctx)) data r) (Focus.focus_list_of_list rl) 
        )])] 
    )]
     
			   
(* DERIVED *)			      
let rec syn_focus (foc : focus) : syn =
  match foc with | {grammar_focus;data} -> 
  begin match grammar_focus with 
    | GrammarFocus(g, ctx) -> syn_grammar_ctx g ctx [Highlight (syn_grammar ctx data g); ControlCurrentFocus] data
    | RulesFocus(r, ctx) -> syn_rules_ctx r ctx [Highlight (syn_rules ctx data r); ControlCurrentFocus] data
    | ProductionFocus(p, ctx) -> syn_production_ctx p ctx [Highlight (syn_production ctx data p); ControlCurrentFocus] data
    | SyntagmFocus(s, ctx) -> syn_syntagm_ctx s ctx [Highlight (syn_syntagm ctx s data); ControlCurrentFocus] data
    | SymbolFocus(s, ctx) -> syn_symbol_ctx s ctx [Highlight (syn_symbol ctx data s); ControlCurrentFocus] data
  end

and syn_grammar_ctx (g:grammar) (ctx:grammar_ctx) (s:syn) (data:word_list) : syn = 
    let  xml_g = [Focus ({grammar_focus=GrammarFocus(g,ctx);data}, s)] in match ctx with    
        | Root -> xml_g 

and syn_rules_ctx (r:rules) (ctx:rules_ctx) (s:syn) (data:word_list) : syn = 
    let  xml_r = [Focus ({grammar_focus=RulesFocus(r,ctx);data}, s)] in match ctx with 
    | Grammar2X(s', (ll_rr), ctx') -> 
        let rl = Focus.list_of_ctx r ll_rr in
        syn_grammar_ctx 
            (Grammar(s', rl)) 
            ctx' 
            ((syn_syntagm (Grammar1(ctx', rl)) s' data)  @ [Indent([Block(
                    (Syntax.xml_list_focus
                        (fun (r1, ll_rr1) -> syn_rules (Grammar2X(s', ll_rr1, ctx')) data r1)
                        (r, ll_rr) 
                        xml_r)
                    )])])
            data

and syn_production_ctx (p:production) (ctx: production_ctx) (s:syn) (data:word_list) : syn = 
    let xml_p = [Focus ({grammar_focus=ProductionFocus(p, ctx); data}, s)] in match ctx with 
    | Rules2X(s', ll_rr, ctx') -> 
        let pl = Focus.list_of_ctx p ll_rr in 
        syn_rules_ctx 
            (Rules(s', pl))
            ctx' 
            ([ Quote("| ", (syn_syntagm (Rules1(ctx', pl)) s' data) @ [ Kwd " -> " ;
             Enum(" | ", 
                (Syntax.xml_list_focus
                    (fun (p1, ll_rr1) -> syn_production (Rules2X(s', ll_rr1, ctx')) data p1) 
                    (p, ll_rr) xml_p)
            )], "")])
            data

and syn_syntagm_ctx (v:syntagm) (ctx: syntagm_ctx) (s: syn) (data:word_list) : syn = 
    let xml_s = [Focus({grammar_focus=SyntagmFocus(v, ctx);data}, s)] in match ctx with 
    | Rules1(ctx', p) -> let pl = Focus.focus_list_of_list p in 
                        syn_rules_ctx 
                            (Rules(v,p)) 
                            ctx' 
                            ([Quote("| ", xml_s @ [ Kwd " -> " ; Enum("|", (List.map (fun (p', ll_rr) -> syn_production (Rules2X(v, ll_rr, ctx')) data p') pl))], "")])
                            data
    | Grammar1(ctx', r) -> let rl = Focus. focus_list_of_list r in 
                            syn_grammar_ctx 
                                (Grammar(v,r)) 
                                ctx' 
                                (xml_s @ [ Indent([Block((List.map (fun (r', ll_rr) -> syn_rules (Grammar2X(v, ll_rr,ctx')) data r') rl))])])
                                data

and syn_symbol_ctx (i:symbol) (ctx: symbol_ctx) (s:syn) (data:word_list) : syn = 
    let xml_s = [Focus ({grammar_focus=SymbolFocus(i,ctx);data}, s)] in match ctx with
    | ProductionX((ll_rr), ctx') -> 
        let sl = Focus.list_of_ctx i ll_rr in 
        syn_production_ctx 
        (Production(sl))
        ctx' 
        ([Enum(" ", 
            (Syntax.xml_list_focus
                (fun(i', ll_rr') -> syn_symbol (ProductionX(ll_rr', ctx')) data i')
                (i, ll_rr) xml_s)
        )])
        data

open Grammar_extent
class color_table = 
object 
    val nb_colors = 8
    val mutable curr : int = 0
    val table : (syntagm, int) Hashtbl.t = Hashtbl.create 8

    method getColor (s:syntagm) : int = match Hashtbl.find_opt table s with 
        | Some(i) -> i 
        | None -> curr <- (curr + 1 mod nb_colors); Hashtbl.add table s curr; curr 
end

let table = new color_table

let rec syn_tree depth t : syn = 
    match t with 
    | Leaf(s) ->  [Quote("<b>",syn_token s,"</b>")]
    | Node(s, l) -> begin 
                    let classe = "color"^(string_of_int (table#getColor s)) in 
                    let q = match l with 
                        | [t'] -> Quote("", syn_tree (depth+1) t', Html.span ~classe ("<sub>"^s^"</sub>"))
                        | _ -> Quote(Html.span ~classe "[", 
                           [Enum("", List.rev(List.fold_left (fun accu t' -> (syn_tree (depth+1) t') :: accu) [] l))],
                           Html.span ~classe ("]<sub>"^s^"</sub>") ) 
                    in 
                    [q]
                    end

let syn_extent_word ext_w : syn = match ext_w with 
    | Word(w) -> 
         let l = 
        List.rev 
        (List.fold_left 
            (fun accu x -> match x with  
                            | Token(t) -> [Suspended(syn_token t)] :: accu 
                            | Tree(t) -> (syn_tree 0 t) :: accu) 
            [] w)
        in [Enum("", l)]

let syn_transf : transf -> syn = function
    | FocusUp -> [Kwd "(focus up)"]
    | FocusRight -> [Kwd "(focus right)"]
    | Delete -> [Kwd "(delete focus)"]
    | ClearGrammar -> [Kwd "Clear"; Kwd "Grammar"]
    | SetWords (ifile, istr) -> [Kwd "Set"; Kwd "phrases"; Input(`FileString ifile); Kwd "separator"; Kwd " : "; Input(`Separator istr)]
    | AddWords (ifile, istr) -> [Kwd "add"; Kwd "phrases"; Input(`FileString ifile); Kwd "separator"; Kwd " : "; Input(`Separator istr)]
    | InputWordSeparator (istr, isep) -> [Kwd "add"; Kwd "phrase"; Input(`Word istr); Kwd "separator"; Kwd " : "; Input(`Separator isep)]
    | ClearWords -> [Kwd "Clear"; Kwd "words"]
    | InsertRule i -> [Kwd "insert"; Kwd "a"; Kwd "new"; Kwd "non"; Kwd "terminal"; Input(`Syntagm i)]
    | ChangeSyntagm i -> [Kwd "change"; Kwd "syntagm"; Kwd " : "; Input(`SelectSyntagm i)]
    | InsertProduction -> [Kwd "insert"; Kwd "a"; Kwd "production"]
    | InsertSymbolBefore (iselect, isymbol) -> [Kwd "insert"; Input(`SelectSyntagm iselect) ; Kwd "or"; Input(`Symbol isymbol); Kwd "before"; Kwd "focus"]
    | InsertSymbolAfter (iselect, isymbol) -> [Kwd "insert"; Input(`SelectSyntagm iselect) ; Kwd "or"; Input(`Symbol isymbol); Kwd "after"; Kwd "focus"]
    | PutInVariable (iselect, isyn) -> [Kwd "move"; Kwd "current"; Kwd "focus"; Kwd "to"; Input(`SelectSyntagm iselect); Kwd "or"; Input(`Syntagm isyn)]
    | Copy -> [Kwd "copy"; Kwd "current"; Kwd "focus"]
    | SetSymbol (iselect, isymbol) -> [Kwd "change"; Kwd "current"; Kwd"focus"; Kwd "to"; Input(`SelectSyntagm iselect); Kwd "or"; Input(`Syntagm isymbol)]




    



