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
let get_item_var t pos v = match (List.fold_left   (fun accu it -> match accu, it with 
                                                                            | None, Parsing.Item(s, (ll,[]), k, il) when s = v-> Some(Parsing.Item(s, (ll,[]), k, il)) 
                                                                            | _ -> accu) 
                                                            None t.(pos)) with 
                                | Some(it) -> it 
                                | None -> assert false 

let rec tree_of_item (t: (Parsing.item list) array) (i:int) (it:Parsing.item) : derivation_tree * (int*int) = match it with 
                                    | Parsing.Item(s, (tl, []), k, il) -> 
                                        let (l,_,_) = List.fold_left 
                                                (fun accu' symb -> match accu', symb with 
                                                    | (l', pos, h::il'), Var(v) -> let (son, _) = tree_of_item t pos (get_item_var t pos v) in ( son :: l', h-1, il') 
                                                    | (l', pos, il'), Item(i') -> (Leaf(i') :: l', pos-1, il') 
                                                    | _ -> failwith "there should always be as much indexes in the list of the item as variables in the production used"
                                                )
                                                ([], i, il) tl (* the accumulator contains the sons of the current Node, 
                                                                the position at wich we must search to get the Node corresponding to the next non terminal
                                                                and the starting postitions of recognitions of the remmaining non terminals to proces*) 
                                        in
                                        (Node(s, List.rev l), (k,i))
                                    | _ -> assert false 

(* TODO : modifier *)
let get_tree (t: (Parsing.item list) array) (i: int)  : (derivation_tree * (int*int)) option = 
    if List.length t.(i) = 0 then None else (* we are sure we will calculate at leat one tree because there is at least one item in l *)
        let l = (List.map (tree_of_item t i) t.(i)) in 
        List.fold_left (* get the biggest tree, i.e. the one that begins the earliest and wich isn't a subtree of any other tree in l *)
            (fun accu c -> match accu, c with 
                            | Some(b_tree, (k_min,_)), (curr_tree, (k,_)) -> if (k < k_min) || (k=k_min && not (subtree b_tree curr_tree)) then Some(c) else accu
                            | _ -> assert false (* we are sure accu contains something *)
            ) 
            (Some(List.hd l))
            (List.tl l)
            

(** t only contains the fully parsed items, i.e. those where the list context is empty on the right *)
(* returns all of the trees that were used in the parsing the int*int gives the starting and ending position of the pattern *)
let get_derivations (t : (Parsing.item list) array) : (derivation_tree * (int*int)) list  = let n = Array.length t in 
    let derivations_list = ref [] in let i = ref (n-1) in 
    while !i > 0 do 
        let ht = get_tree t !i 
        in match ht with | Some(c,(k,_)) -> derivations_list:= (c, (k,!i))::!derivations_list; i := (k-1) | None -> decr i
    done;
    !derivations_list

(* Tests *)

let t1 = Node("S", [Node("E", [Node("E", [Node("N", [Node("F", [Leaf("a")])])]); Leaf("+"); Node("N", [Node("F", [Leaf("a")])])])])
let t2 = Node("S", [Node("E", [Node("N", [Node("F", [Leaf("a")])])])])

let d = List.map (fun (x,_) -> x) (get_derivations (Parsing.get_fully_parsed (Parsing.t))) 

let _ = Printf.printf "test subtree : l'arbre est-il bien inclus ? : %B"  (subtree t1 t2) 

let _ = Printf.printf "l'arbre est-il bien inclus dans la liste des résultats ? : %B" (List.mem t1 d)