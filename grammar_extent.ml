open Grammar

type derivation_tree = | Leaf of token | Node of syntagm  * derivation_tree list
type extent_item = | Token of token | Tree of derivation_tree
type extent = extent_item list 

(*
(* returns true iff a is a subtree of b*)
let rec subtree (b:derivation_tree) (a:derivation_tree) : bool = 
    if a = b then true 
    else match b, a with 
        | Node(s, l), Node(s',l') when s=s' -> sons_included l l' 
        | Node(_,l) , _ -> List.fold_left (fun accu t -> accu || (subtree t a)) false l 
        | _ -> false
and sons_included ?(started=false) (bsons: derivation_tree list) (asons: derivation_tree list) : bool = match bsons, asons with 
    | [Leaf(t)], [Leaf(t')] when t=t'-> true
    | b::tl, a::tl' when b=a -> sons_included ~started:true tl tl'
    | Node(s,l)::tl, Node(s',l')::tl' when s=s' -> (sons_included l l') && (sons_included ~started:true tl tl')
    | _ , [] when started -> true 
    | _ -> false 
*)

(* returns true iff a is a subtree of b*)
let rec subtree (b:derivation_tree) (a:derivation_tree) : bool = 
    if a = b then true 
    else match b with 
        | Node(_, l) -> List.fold_left (fun accu t -> accu || (subtree t a)) false l 
        | _ -> false

(* returns true iff a is a subtree of a tree of l*)
let subtree_l (l:derivation_tree list) (a:derivation_tree) : bool = List.fold_left (fun accu t -> accu || (subtree t a)) false l

(* we are certain this item exists because we only have fully parsed items in t *)
(* we seek an item that was started at position h, ends at the positions marked by tp and wich var is v
    if there are more than one item that matches theses conditions, 
    we don't care the one chosen because we only want one possible derivation tree that leads to recognising the word
*)
let get_item_var tp start v = match (List.find_opt (fun it -> match it with 
                                                        | Parsing.Item(s, (_,[]), k, _) when (s = v && k = start) -> true 
                                                        | _ -> false) tp) with 
                                | Some(it) -> it 
                                | None -> failwith "there should alway's be at least one item, because we are recreating a tree that could have been used to recognise the partern"

let rec tree_of_item (t: (Parsing.item list) array) (i:int) (it:Parsing.item) : derivation_tree * (int*int) = match it with 
                                    | Parsing.Item(s, (tl, []), k, il) ->
                                        let (l,_,_) = List.fold_left 
                                                (fun accu' symb -> match accu', symb with 
                                                    | (l', pos, h::il'), Var(v) -> let (son, _) = tree_of_item t pos (get_item_var t.(pos) h v) in ( son :: l', h, il') 
                                                    | (l', pos, il'), Item(i') -> (Leaf(i') :: l', pos-1, il') 
                                                    | _ -> failwith "there should always be as much indexes in the list of the item as variables in the production used"
                                                )
                                                ([], i, il) tl (* the accumulator contains the sons of the current Node, 
                                                                the position at wich we must search to get the Node corresponding to the next non terminal
                                                                and the starting postitions of recognitions of the remmaining non terminals to proces*) 
                                        in
                                        (Node(s, l), (k,i))
                                    | _ -> failwith "shouldn't happen, we only use the fully parsed items"

let get_tree (t: (Parsing.item list) array) (i: int)  : (derivation_tree * (int*int)) option = 
    if List.length t.(i) = 0 then None else (* we are sure we will calculate at leat one tree because there is at least one item in l *)
        let l = (List.map (tree_of_item t i) t.(i)) in 
        List.fold_left (* get the biggest tree, i.e. the one that begins the earliest and wich isn't a subtree of any other tree in l *)
            (fun accu c -> match accu, c with 
                            | Some(b_tree, (k_min,_)), (curr_tree, (k,_)) -> if (k < k_min) || (k=k_min && not (subtree b_tree curr_tree)) then Some(c) else accu
                            | _ -> failwith "accu should alway's be non empty" 
            ) 
            (Some(List.hd l))
            (List.tl l)
            

(** t only contains the fully parsed items, i.e. those where the list context is empty on the right *)
(* returns all of the trees that were used in the parsing the int*int gives the starting and ending position of the pattern *)
let get_derivations (t : (Parsing.item list) array) : (derivation_tree * (int*int)) list  = let n = Array.length t in 
    let derivations_list = ref [] in let i = ref (n-1) in 
    while !i > 0 do 
        let ht = get_tree t !i 
        in match ht with
            | Some(c,(k,_)) -> derivations_list:= (c, (k,!i))::!derivations_list; i := (k-1) 
            | None -> decr i
    done;
    !derivations_list

let get_derivations_of_earley t = let t'= Parsing.get_fully_parsed t in get_derivations t'

let rec derivations_of_focus (w : token array) (f: Grammar_focus.focus) : ((derivation_tree * (int*int)) list) = match f with 
    | GrammarFocus(g, ctx) -> let g' = Grammar_focus.grammar_of_focus f in
                              get_derivations_of_earley (Parsing.earley (Parsing.init_earley g') g' w)  (* simplement lancer earley  *)
   
    | RulesFocus(r,ctx) -> let Grammar(_,rl) = Grammar_focus.grammar_of_focus f in 
                           let s = match r with 
                            | Rules(s,_) -> s
                            in let g = Grammar(s,rl) in 
                            get_derivations_of_earley (Parsing.earley (Parsing.init_earley g) g w) (* lancer earley en ayant mis comme axiome le syntagme de r *)
    
    | ProductionFocus(p, ctx) -> let g =  Grammar_focus.grammar_of_focus f in 
                                 let s = match ctx with | Rules2X(s, _, _) -> s in
                            get_derivations_of_earley (Parsing.earley (Parsing.init_earley_production s p) g w)(* lancer earley avec une initialisation modifiée qui ne met que la règle s->p dans le tableau*)
   
    | SyntagmFocus(s, ctx) -> begin match Grammar_focus.focus_up f with  (* se rammener au cas grammar ou rules *)
                                | Some (f,_) -> derivations_of_focus w f
                                | None -> failwith "there should always be a focus rechable upwards from a syntagm"
                             end
    | SymbolFocus(s, ctx) -> begin match Grammar_focus.focus_up f with 
                                | Some (f,_) -> derivations_of_focus w f
                                | None -> failwith "there should always be a focus rechable upwards from a symbol"
                             end
                                 (* se ramener au cas production mais en plus on voudra surligner le symbole dans les blocs reconnus*)


let compute_extent w f : extent = 
    let res = ref [] in 
    let l = ref (derivations_of_focus w f) in 
    let n = Array.length w in 
    let i = ref 0 in 
    while !i < n do 
        match !l with 
            | (t, (k,j)) :: tl when k = !i -> l := tl; i := j; res := (Tree(t))::!res
            | _ -> res := (Token(w.(!i)))::!res; incr i  
    done;
    List.rev (!res)

(* Tests *)
(*
(*let _ = Array.fold_left (fun accu l -> Printf.printf "%d items; " (List.length l)) () Parsing.t ; Printf.printf "\n"*)

let fp = Parsing.get_fully_parsed (Parsing.t) 

(*let _ = Array.fold_left (fun accu l -> Printf.printf "%d items fully_parsed; " (List.length l)) () fp; Printf.printf "\n"*)

let _ = Array.iteri ( fun i l -> let s = if i = 0 then "_" else Parsing.w.(i-1) in Printf.printf "after reading %s, tab %d : %d items in this cell, " s i (List.length l); 
                            List.fold_left (fun accu' it -> match it with | Parsing.Item(s, (_,_), j, _) -> Printf.printf "%s starting in %d, " s j) () l;
                            Printf.printf "\n") fp 

let d = get_derivations (fp)

let _ = Printf.printf "combien d'arbres ? : %d\n" (List.length d)

let rec print_tree t = match t with 
    | Node(s, l) -> print_string ("Node " ^ s ^ "( "); List.fold_left (fun accu t' -> print_tree t') () l; print_string ")\n"
    | Leaf(s) -> Printf.printf "%s \n" s 

let _ = List.fold_left (fun accu (t, (k,e)) -> print_tree t; Printf.printf "start %d , end %d \n" k e ) () d
*)