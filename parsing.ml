open Grammar

(*  the int corresponds to the index where we started to recognize the production 
    whereas the int list corresponds to the positions where we started to recognise each variable in the production 
*)
type item = Item of syntagm * symbol Focus.list_ctx * int * (int list)

let item_of_production (i : int) (s: syntagm) (p: production) : item = match p with
    | Production (sl) -> Item(s, ([],sl), i, [])

let items_of_rule (i:int) (r:rules) : item list = match r with 
    | Rules(s, pl) -> List.fold_left (fun accu p -> (item_of_production i s p) :: accu) [] pl

let get_items_s ?(i = 0) (s:syntagm) (accu:item list) (r: rules) : item list = match r with 
    | Rules(s',pl) when s'=s -> (items_of_rule i r) @ accu 
    | _ -> accu

let initial_items (i:int) : grammar -> item list = function 
    | Grammar(s,r) -> List.fold_left (get_items_s ~i s) [] r  

let init_earley (g:grammar) (i: int) : item list = initial_items i g 

let init_earley_production (s: syntagm) (p: production) (i:int) : item list = [item_of_production i  s p] 

let get_scans (t:token) (accu:item list) (it: item) : item list = match it with 
    | Item(s, (ll,rr), i, il) -> begin match rr with 
                                | t'::tl when Grammar.Item(t)=t' -> Item(s ,(t'::ll, tl), i, il)::accu 
                                | _ -> accu 
                            end

let scan (i: int) (t: token) (tab: (item list) array) (g:grammar) : (item list) array =  
    tab.(i) <- (List.fold_left (get_scans t) [] tab.(i-1)); tab 

let contains (l: item list) (v:syntagm) (step:int) : bool = List.fold_left (fun accu i -> match accu, i with 
                                                                                | false, Item(s,_,k,_) when s=v && k=step -> true 
                                                                                | _ -> accu) 
                                                            false l

let predict_step (step:int) (g:grammar) (accu: item list) (it: item) : item list =  match g, it with 
    | Grammar(_,r), Item(_, (_, rr), _, _) -> begin match rr with 
                                            | v::tl -> begin match v with 
                                                        | Var(s) -> if not (contains accu s step) then (* TOUT CASSE !*)
                                                                       List.fold_left (get_items_s ~i:step s) accu r  
                                                                    else accu 
                                                        | _ -> accu 
                                                    end 
                                            | _ -> accu 
                                            end

let predict (i: int) (tab: (item list) array) (g:grammar) : (item list) array = let cond = ref false in
    while not !cond do 
        let prec = tab.(i) in
        tab.(i) <- List.fold_left (predict_step i g) tab.(i) tab.(i);
        cond := List.length prec = List.length tab.(i); (* on n'a rien ajouté de nouveau donc les deux listes sont de même longueur *)
    done;
    tab

let complete_step (i : int) (tab: (item list) array) (accu:item list) (it:item) : item list = match it with 
    | Item(v,(_,rr), j, il) -> begin match rr with 
        | [] -> List.fold_left 
                (fun accu' it' -> begin match it' with 
                    | Item(c, (ll, v'::rr'), k, il') when v' = Var(v) -> let ni = Item(c, (v'::ll, rr'), k, i::il') in 
                                                                   if (not (List.mem ni tab.(i))) && (not (List.mem ni accu')) then 
                                                                        ni::accu' 
                                                                    else accu'
                    | _ -> accu' 
                    end)
                accu tab.(j)
        | _ -> accu 
    end

let complete (i: int) (tab: (item list) array) : (item list) array = let cond = ref false in 
    while not !cond do 
        let prec = tab.(i) in
        tab.(i) <- List.fold_left (complete_step i tab) tab.(i) tab.(i);
        cond := List.length prec = List.length tab.(i); (* on n'a rien ajouté donc les deux listes sont de même longueur*)
    done;
    tab

(* on veut pouvoir spécifier la fonction d'initialisation en fonction du focus courant *)
let earley (init_func: int -> (item list)) (g: grammar) (w: token list) : (item list) array = match g with 
    | Grammar(s, r) -> let n = List.length w in 
                       let tab = Array.init (n+1) (init_func) in 
                       let t = ref tab in 
                       let w' = ref w in 
                        t := predict 0 !t g;
                        t := complete 0 !t;
                       for i = 1 to n do 
                            t := scan i (List.hd !w') !t g; 
                            w' := List.tl !w'; (** on a lu le premier charactère de la liste *)
                            t := predict i !t g;
                            t := complete i !t
                       done;
                       !t

let get_fully_parsed (t: (item list) array) : (item list) array = let n = Array.length t in 
    Array.init n (fun i -> (List.fold_left (fun accu it -> match it with | Item(_, (_, []), _, _) -> it::accu | _ -> accu ) [] t.(i))) 


(* TEST *)

(*
let equal_l (t'i: item list)  (ti: item list) : bool =  List.for_all (fun a -> List.mem a t'i) ti 

let t' = [| [
                Item("S", ([], [Var("E")]), 0); 
                Item("E", ([], [Var("E"); Item("+"); Var("N")]), 0); 
                Item("E", ([], [Var("E"); Item("-"); Var("N")]), 0);
                Item("E", ([], [Var("E")]), 0);
                Item("N", ([], [Var("N"); Item("*"); Var("F")]), 0);
                Item("N", ([], [Var("N"); Item("/"); Var("F")]), 0);
                Item("N", ([], [Var("F")]), 0);
                Item("F", ([], [Item("a")]), 0);
                Item("F", ([], [Item("-"); Var("F")]), 0);
                Item("F", ([], [Item("+"); Var("F")]), 0);
                Item("F", ([], [Item("("); Var("E"); Item(")")]), 0)
            ];
            [
                Item("F", ([Item("a")], []), 0);
                Item("N", ([Var("F")], []), 0);
                Item("N", ([Var("N")], [Item("*"); Var("F")]), 0);
                Item("N", ([Var("N")], [Item("/"); Var("F")]), 0);
                Item("E", ([Var("N")], []), 0);
                Item("E", ([Var("E")], [Item("-"); Var("N")]), 0);
                Item("E", ([Var("E")], [Item("+"); Var("N")]), 0);
                Item("S", ([Var("E")], []), 0)
            ]; 
            [
                Item("E", ([Item("+"); Var("E")], [Var("N")]), 0);
                Item("N", ([], [Var("N"); Item("*"); Var("F")]), 2);
                Item("N", ([], [Var("N"); Item("/"); Var("F")]), 2);
                Item("N", ([], [Var("F")]), 2);
                Item("F", ([], [Item("a")]), 2);
                Item("F", ([], [Item("-"); Var("F")]), 2);
                Item("F", ([], [Item("+"); Var("F")]), 2);
                Item("F", ([], [Item("("); Var("E"); Item(")")]), 2)
            ]; 
            [
                Item("F", ([Item("a")], []), 2);
                Item("N", ([Var("F")], []), 2);
                Item("N", ([Var("N")], [Item("*"); Var("F")]), 2);
                Item("N", ([Var("N")], [Item("/"); Var("F")]), 2);
                Item("E", ([Var("E")], [Item("-"); Var("N")]), 0);
                Item("E", ([Var("E")], [Item("+"); Var("N")]), 0);
                Item("S", ([Var("E")], []), 0);
                Item("E", ([Var("N"); Item("+"); Var("E")],[]), 0)
            ] 
|]

let g = Grammar("S", [Rules("S", [Production([Var("E")])]); 
    Rules("E", [Production([Var("E"); Item("+"); Var("N")]); Production([Var("E"); Item("-"); Var("N")]); Production([Var("N")])]);
    Rules("N", [Production([Var("N"); Item("*"); Var("F")]); Production([Var("N"); Item("/"); Var("F")]); Production([Var("F")])]);
    Rules("F", [Production([Item("a")]); Production([Item("-"); Var("F")]); Production([Item("+"); Var("F")]); Production([Item("("); Var("E"); Item(")")])])]) 

let w = ["a"; "+"; "a"]
let t = earley (init_earley g) g w
*)
(* let b =  Printf.printf "A t'on le bon resultat ? : %B" (Array.for_all2 equal_l t' t) *)
