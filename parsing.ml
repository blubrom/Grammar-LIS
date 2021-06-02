open Grammar

type item = Item of syntagm * symbol Focus.list_ctx * int 

let item_of_production (i : int) (s: syntagm) (p: production) : item = match p with
    | Production (sl) -> Item(s, ([],sl), i)

let items_of_rule (i:int) (r:rules) : item list = match r with 
    | Rules(s, pl) -> List.fold_left (fun accu p -> (item_of_production i s p) :: accu) [] pl

let get_items_s (s:syntagm) (accu:item list) (r: rules) : item list = match r with 
    | Rules(s',pl) when s'=s -> (items_of_rule 0 r) @ accu 
    | _ -> accu

let initial_items : grammar -> item list = function 
    | Grammar(s,r) -> List.fold_left (get_items_s s) [] r  

let init_earley (g:grammar) (i: int) : item list = if i = 0 then initial_items g  else []

let get_scans (t:token) (accu:item list) (it: item) : item list = match it with 
    | Item(s, (ll,rr), i) -> begin match rr with 
                                | t::tl -> Item(s ,(t::ll, tl), i)::accu 
                                | _ -> accu 
                            end

let scan (i: int) (t: token) (tab: (item list) array) (g:grammar) : (item list) array =  
    tab.(i) <- (List.fold_left (get_scans t) [] tab.(i-1)); tab 

let contains (l: item list) (v:syntagm) : bool = List.fold_left (fun accu i -> match accu, i with | false, Item(s,_,_) when s=v -> true | _ -> accu) false l

let predict_step (g:grammar) (ti:item list) (accu: item list) (it: item) : item list = match g, it with 
    | Grammar(_,r), Item(_, (_, rr), _) -> begin match rr with 
                                            | v::tl -> begin match v with 
                                                        | Var(s) -> if not (contains ti s) && not (contains accu s) then 
                                                                       List.fold_left (get_items_s s) accu r  
                                                                    else accu 
                                                        | _ -> accu 
                                                    end 
                                            | _ -> accu 
                                            end 

let predict (i: int) (tab: (item list) array) (g:grammar) : (item list) array = let cond = ref true in 
    while not !cond do 
        let prec = tab.(i) in
        tab.(i) <- List.fold_left (predict_step g tab.(i)) [] tab.(i);
        cond := prec = tab.(i);
        Printf.printf "predict : %B " !cond 
    done;
    tab

let complete_step (i : int) (tab: (item list) array) (accu:item list) (it:item) : item list = match it with 
    | Item(v,(_,rr), j) -> begin match rr with 
        | [] -> List.fold_left 
                (fun accu' it' -> begin match it' with 
                    | Item(c, (ll, v'::rr), k) when v' = Var(v) -> let ni = Item(c, (v'::ll, rr), k) in 
                                                                   if not (List.mem ni tab.(i)) && not (List.mem ni accu') then 
                                                                        ni::accu' 
                                                                    else accu'
                    | _ -> accu' 
                    end)
                accu tab.(j)
        | _ -> accu 
    end

let complete (i: int) (tab: (item list) array) : (item list) array = let cond = ref true in 
    while not !cond do 
        let prec = tab.(i) in
        tab.(i) <- List.fold_left (complete_step i tab) [] tab.(i);
        cond := prec = tab.(i);
        Printf.printf "complete %B " !cond
    done;
    tab

let earley (g: grammar) (w: token list) : (item list) array = match g with 
    (** renvoyer vrai si un item de la forme Item(s, (ll, []), 0) appartient à T.(n) *)
    | Grammar(s, r) -> let n = List.length w in 
                       let tab = Array.init (n+1) (init_earley g) in 
                       let t = ref tab in 
                       let w' = ref w in 
                        t := predict 0 !t g;
                        print_string "predict réussi \n";
                        t := complete 0 !t;
                        print_string "complete réussi\n";
                       for i = 1 to n do 
                            t := scan i (List.hd !w') !t g; 
                            print_string "scan réussi\n";
                            w' := List.tl !w'; (** on a lu le premier charactère de la liste *)
                            t := predict i !t g;
                            print_string "predict réussi \n";
                            t := complete i !t;
                            print_string "complete réussi\n"
                       done;
                       !t

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
let t = earley g w
let b =  Printf.printf "A t'on le bon resultat ? : %B" (Array.for_all2 equal_l t' t)
*)