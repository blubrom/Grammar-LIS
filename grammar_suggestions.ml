
open Focus
open Grammar
open Grammar_focus

type suggestion = transf
 
let suggestions (foc : focus) : suggestion Lis.forest list =
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
        let f = match foc with {grammar_focus} -> grammar_focus in 
        match f with 
            | Some gf -> 
                let Grammar(_, rl) = grammar_of_focus foc in 
                let sl = List.map (fun r -> match r with Rules(s,_) -> s) rl in
                begin match gf with 
                | GrammarFocus(_) -> add `Add (InsertRule (new input ""));
                                    add `Modify (ChangeSyntagm((initial_select::sl, new input initial_select)))

                | RulesFocus(_) -> add `Add (InsertRule (new input ""));
                                    add `Add (InsertProduction);
                                    add `Modify (ChangeSyntagm((initial_select::sl, new input initial_select)))

                | ProductionFocus(_) -> add `Add (InsertProduction);
                                        add `Add (PutInVariable ((initial_select::sl, new input initial_select), new input ""));
                                        add `Modify (ChangeSyntagm((initial_select::sl, new input initial_select)));
                                        add `Add (Copy);
                                        add `Add (InsertSymbolBefore ((initial_select::sl, new input initial_select), new input ""));
                                        add `Add (InsertSymbolAfter ((initial_select::sl, new input initial_select), new input ""))

                | SymbolFocus(_) -> add `Modify (SetSymbol((initial_select::sl, new input initial_select), new input ""));
                                    add `Add (InsertSymbolBefore ((initial_select::sl, new input initial_select), new input ""));
                                    add `Add (InsertSymbolAfter ((initial_select::sl, new input initial_select), new input ""));
                                    add `Add (PutInVariable ((initial_select::sl, new input initial_select), new input ""));
                                    add `Add (InsertProduction)

                | SyntagmFocus(_) -> add `Add (InsertRule (new input ""));
                                    add `Add (InsertProduction);
                                    add `Modify (ChangeSyntagm((initial_select::sl, new input initial_select)))
                end
            | None ->  add `Add(InsertRule (new input ""))

    in
    [!forest_modify; !forest_add; !forest_data]