
open Focus
open Grammar
open Grammar_focus

type suggestion = transf

let exists_fully_parsed l s = List.exists (fun it -> match it with | Parsing.Item(s',(_,[]),_,_) when s' = s -> true | _ -> false) l

let get_tokens_after ext data = 
    List.fold_left2 
        (fun accu (tab,s,_) w -> 
            let l = ref [] in begin 
            Array.iteri 
                (fun i il -> if exists_fully_parsed il s then begin try l := ("token "^w.(i)) :: !l with | Invalid_argument _ -> () end) tab 
            ; !l @ accu end) 
        [] ext data

let get_tokens_before ext data = 
    List.fold_left2 
        (fun accu (tab,s,_) w -> 
            let l = ref [] in begin 
            Array.iteri 
                (fun i il -> 
                   (List.iter (fun it -> match it with | Parsing.Item(s', (_,[]), k,_) when s'=s -> begin try l := ("token "^w.(k-1)) :: !l with | Invalid_argument _ -> () end 
                                                       | _ -> ()) il)) 
                tab
            ; !l @ accu end) 
        [] ext data

let init_earley_everey_rules g i = match g with 
    | Grammar(_, rl) -> List.fold_left (fun accu r -> match r with | Rules(s,_) when s <> "" -> (Parsing.items_of_rule i r) @ accu | _ -> accu) [] rl

let get_syntagms_after ext foc = 
    (* could fail if the grammar_focus is empty. However, this function should only be called when the grammar_focus is non empty*)
    let g, phrases = match foc with {data} -> grammar_of_focus foc, data in
    let parsing_syntagms = List.map (fun w -> Parsing.earley (init_earley_everey_rules g) g w) phrases in
    List.fold_left2 
    (fun accu (tab,s,_) tab' -> 
        let len = Array.length tab in 
        let l = ref [] in begin 
        Array.iteri 
            (fun i il -> if exists_fully_parsed il s then begin 
                for j = i to (len - 1) do 
                    let syns = List.fold_left (fun accu' it -> match it with | Parsing.Item(s', (_,[]),k,_) when k = i -> ("syntagm "^s')::accu' | _ -> accu') [] tab'.(j) 
                    in l := syns @ !l done; end) 
            tab 
        ; !l @ accu end) 
    [] ext parsing_syntagms

let get_syntagms_before ext foc = 
    (* could fail if the grammar_focus is empty. However, this function should only be called when the grammar_focus is non empty*)
    let g, phrases = match foc with {data} -> grammar_of_focus foc, data in
    let parsing_syntagms = List.map (fun w -> Parsing.earley (init_earley_everey_rules g) g w) phrases in
    List.fold_left2 
    (fun accu (tab,s,_) tab' -> 
        let l = ref [] in begin 
        Array.iteri 
            (fun i il -> 
                let indices = (List.fold_left (fun accu' it -> match it with | Parsing.Item(s', (_,[]), k,_) when s'=s -> k::accu' | _ -> accu') [] il) in 
                let syns = List.fold_left 
                    (fun accu' k -> begin try 
                        (List.fold_left (fun accu'' it -> match it with | Parsing.Item(s', (_, []), _,_) -> ("syntagm "^s')::accu'' | _ -> accu'') accu' tab'.(k-1)) 
                        with | Invalid_argument _ -> accu' end) 
                    [] indices in
                l := syns @ !l)
            tab 
        ; !l @ accu end) 
    [] ext parsing_syntagms

let get_tokens data = List.fold_left (fun accu w -> let l = ref [] in begin Array.iter (fun t -> l := ("token "^t) :: !l) w; !l @ accu end) [] data 

let rec filter_uniq l = match l with 
    | [] -> []
    | h :: tl -> if List.mem h tl then filter_uniq tl else h::(filter_uniq tl)
 
let suggestions (ext : (((Parsing.item list) array * syntagm * syntagm) list) option) (foc : focus) : suggestion Lis.forest list =
    let forest_modify = ref [] in
    let forest_add = ref [] in
    let forest_data = ref [] in
    let add kind ?(path : string list = []) tr =
        if apply_transf tr foc <> None then (
            let forest =
            match kind with
                | `Add -> forest_add
                | `Modify -> forest_modify
                | `Data -> forest_data
            in forest := Lis.insert_suggestion path tr !forest
    ) in 
    let () =
        add `Modify FocusUp;
        add `Modify FocusRight;
        add `Modify Delete;
        add `Modify ClearGrammar;
        add `Data (SetWords (new input ("",""), new input(" ")));
        add `Data (InputWordSeparator (new input (""), new input(" ")));
        add `Data (ClearWords);
        add `Data (AddWords (new input ("",""), new input(" ")));
        let f,data = match foc with {grammar_focus;data} -> grammar_focus,data in 
        (* à modifier pour trier par ordre décroissant de fréquence d'apparition dans la liste *)
        let tokens = initial_select :: (filter_uniq (get_tokens data)) in
        match f with 
            | Some gf -> 
                let Grammar(_, rl) = grammar_of_focus foc in 
                let sl = List.filter (fun s -> s<>"") (List.map (fun r -> match r with Rules(s,_) -> s) rl) in
                let ext' = match ext with | Some(x) -> x | _ -> assert false in 
                (* à modifier pour trier par ordre décroissant de fréquence d'apparition dans la liste *)
                let sym_before = initial_select :: (filter_uniq (get_syntagms_before ext' foc)) @ (filter_uniq (get_tokens_before ext' data)) in
                let sym_after = initial_select :: (filter_uniq (get_syntagms_after ext' foc)) @ (filter_uniq (get_tokens_after ext' data)) in 
                let prod_starters = tokens @ (List.map (fun s -> "syntagm "^s) sl) in 
                begin match gf with 
                    | GrammarFocus(Grammar(axiom, _),_) -> 
                        if axiom = "" then 
                            add `Modify (NameAxiom(new input ""))
                        (*add `Add (InsertRule (new input ""));*)
                        (*add `Modify (ChangeSyntagm((initial_select::sl, new input initial_select)));*)

                    | RulesFocus(Rules(s,_),_) ->  
                        add `Add (InsertProduction(prod_starters, new input initial_select));
                        if s = "" then 
                            add `Add (PutInVariable ((initial_select::sl, new input initial_select), new input ""));
                            add `Modify (NameAxiom(new input ""))
                        (*add `Modify (ChangeSyntagm((initial_select::sl, new input initial_select)))*)
                        (*add `Add (InsertRule (new input ""));*)

                    | ProductionFocus(_) -> 
                        add `Add (InsertProduction(prod_starters, new input initial_select));
                        add `Add (PutInVariable ((initial_select::sl, new input initial_select), new input ""));
                        (* add `Add ExtendAfter*)
                        (* add `Add ExtendBefore*)
                        add `Add (InsertSymbolBefore (sym_before, new input initial_select));
                        add `Add (InsertSymbolAfter (sym_after, new input initial_select)) 
                        (*
                        add `Modify (ChangeSyntagm((initial_select::sl, new input initial_select)));
                        add `Add (Copy);
                        *)

                    | SymbolFocus(_, ctx) -> 
                        (* add `Add ExtendAfter*)
                        (* add `Add ExtendBefore*)
                        add `Add (InsertProduction(prod_starters, new input initial_select));
                        begin match ctx with 
                            | ProductionX((_,[]),_) -> add `Add (InsertSymbolAfter (sym_after, new input initial_select));
                                                        add `Add (PutInVariable((initial_select::sl, new input initial_select), new input ""))
                            | ProductionX(([],_),_) -> add `Add (InsertSymbolBefore (sym_before, new input initial_select))
                            | _ -> ()
                        end
                        (*
                        add `Modify (SetSymbol((initial_select::sl, new input initial_select), new input ""));
                        *)

                    | SyntagmFocus(s,_) -> 
                        add `Add (InsertProduction(prod_starters, new input initial_select));
                        if s = "" then 
                            add `Modify (NameAxiom (new input ""))
                        (*add `Modify (ChangeSyntagm((initial_select::sl, new input initial_select)))*)
                        (*add `Add (InsertRule (new input ""));*)
                end
            | None ->  
                if data <> [] then 
                    add `Add (InsertProduction(tokens, new input initial_select))
                (*add `Add(InsertRule (new input ""));*)

    in
    [!forest_modify; !forest_add; !forest_data]
