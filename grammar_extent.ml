open Grammar
open Parsing

type derivation_tree = | Leaf of token | Node of syntagm * derivation_tree list 

(* returns true iff a is a subtree of b*)
let rec subtree (b:derivation_tree) (a:derivation_tree) : bool = 
    if a = b then true 
    else match b with 
        | Node(_, l) -> List.fold_left (fun accu t -> accu || (subtree t a)) false l 
        | _ -> false

(* returns true iff a is a subtree of a tree of l*)
let subtree (l:derivation_tree list) (a:derivation_tree) : bool = List.fold_left (fun accu t -> accu || (subtree t a)) false l

(* we are certain this item exists because we only have fully parsed items in t *)
let get_item_var t pos v = match (List.fold_left   (fun accu it -> match accu, it with 
                                                                            | None, Item(s, (ll,[]), k, il) when s = v-> Some(Item(s, (ll,[]), k, il)) 
                                                                            | _ -> accu) 
                                                            None t.(pos)) with 
                                | Some(it) -> it 
                                | None -> assert false 

let rec tree_of_item (t: (Parsing.item list) array) (i:int) (it:Parsing.item) : derivation_tree = match it with 
                                    | Item(s, (tl, []), _, il) -> 
                                        let (l,_,_) = List.fold_left 
                                                (fun accu' symb -> match accu', symb with 
                                                    | (l', pos, h::il'), Var(v) -> (tree_of_item t pos (get_item_var t pos v) :: l', h-1, il') 
                                                    | (l', pos, il'), Item(i') -> (Leaf(i') :: l', pos-1, il') 
                                                    | _ -> failwith "there should always be as much indexes in the list of the item as variables in the production used"
                                                )
                                                ([], i, il) tl (* the accumulator contains the sons of the current Node, 
                                                                the position at wich we must search to get the Node corresponding to the next non terminal
                                                                and the starting postitions of recognitions of the remmaining non terminals to proces*) 
                                        in
                                        Node(s, List.rev l)
                                    | _ -> assert false 

(** t only contains the fully parsed items, i.e. those where the list context is empty on the right *)
let get_derivations (t : (Parsing.item list) array) : derivation_tree list = let n = Array.length t in 
    let derivations_list = ref [] in 
    for i = (n-1) to 0 do 
        derivations_list := List.fold_left 
                                (fun accu t -> if (accu = []) || not (subtree accu t) then t::accu else accu) 
                                (!derivations_list) 
                                (List.map (tree_of_item t i) t.(i))
    done;
    !derivations_list