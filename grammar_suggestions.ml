
open Focus
open Grammar
open Grammar_focus
module Sem = Grammar_semantics

type suggestion = transf

(*
let focus_types_lengths_fields (extent : Sem.extent) : Sem.TypSet.t * int Bintree.t * string Bintree.t * int =
  let max_bindings = 20 in
  let max_items_per_cell = 20 in
  List.fold_left
    (fun (typs,lens,fields,nbindings) binding ->
     try
       let d0 = List.assoc Sem.field_focus binding in
       let li, _ = Seq.take max_items_per_cell d0 in
       let typs, len, fields =
	 List.fold_left
	   (fun (typs,len,fields) i ->
	    let typs, fields =
	      match i with
	      | `Bool _ -> Sem.TypSet.add `Bool typs, fields
	      | `Int _ -> Sem.TypSet.add `Int typs, fields
	      | `Float _ -> Sem.TypSet.add `Float typs, fields
	      | `String _ -> Sem.TypSet.add `String typs, fields
	      | `Null -> typs, fields
	      | `Assoc pairs ->
		 Sem.TypSet.add `Object typs,
		 List.fold_left (fun fields (k,_) -> Bintree.add k fields) fields pairs
	      | `List _ -> Sem.TypSet.add `Array typs, fields in
	    typs, len+1, fields)
	   (typs,0,fields) li in
       typs, Bintree.add len lens, fields, nbindings+1
     with Not_found -> typs, lens, fields, nbindings)
    (Sem.TypSet.empty, Bintree.empty, Bintree.empty, 0)
    (fst (Seq.take max_bindings extent.Sem.bindings))

*)

    
let suggestions (foc : focus) : suggestion Lis.forest list =
(*
  let focus_typs, focus_lens, fields, nbindings = focus_types_lengths_fields extent in
  let focus_typs =
    if Sem.TypSet.is_empty focus_typs (* empty sequence () *)
    then Sem.all_typs
    else focus_typs in
  let ctx_typs = sem.Sem.annot#typs in
  (*  let allows_any_type = sem.Sem.annot#allows_any_type in *)
  let allowed_typs = Sem.TypSet.inter ctx_typs focus_typs in
  let multiple_items = Bintree.fold (fun n ok -> ok || n > 1) focus_lens false in
  let multiple_bindings = nbindings > 1 in
  *)
    let forest_add = ref [] in
    let forest_insert = ref [] in
    let add kind ?(path : string list = []) tr =
        if apply_transf tr foc <> None then (
            let forest =
            match kind with
                | `Add -> forest_add
                | `Insert -> forest_insert
            in forest := Lis.insert_suggestion path tr !forest
    ) in 
    let () =
        add `Insert FocusUp;
        add `Insert FocusRight;
        add `Insert Delete;
        add `Add (InputFileString (new input ("","")));
        add `Add (InsertRule (new input ""));
        add `Insert InsertProduction;
        add `Insert InsertSymbol;
    in
    [!forest_insert; !forest_add]