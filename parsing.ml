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
    | Item(s, (ll,t'::rr), i, il) when Grammar.Item(t)=t'-> Item(s ,(t'::ll, rr), i, il)::accu
    | _ -> accu 
    
let scan (i: int) (t: token) (tab: (item list) array) (g:grammar) : (item list) array =  
    tab.(i) <- (List.fold_left (get_scans t) tab.(i) tab.(i-1)); tab 
(*
let contains (l: item list) (v:syntagm) (step:int) : bool = List.fold_left (fun accu i -> match accu, i with 
                                                                                | false, Item(s,_,k,_) when s=v && k=step -> true 
                                                                                | _ -> accu) 
                                                            false l
*)

let predict_step (i:int) (g:grammar) (accu: item list) (it: item) : item list =  match g, it with 
    | Grammar(_,rl), Item(_, (_, Var(s)::tl), _, _) -> begin try let r = List.find (fun r' -> match r' with | Rules(s',_) when s'=s -> true | _ -> false) rl in 
                                                       List.fold_left (fun accu it -> if List.mem it accu then accu else it::accu) accu (items_of_rule i r)
                                                       with | Not_found -> accu end
    | _ -> accu 

let predict (i: int) (tab: (item list) array) (g:grammar) : (item list) array = let cond = ref false in
    while not !cond do 
        let prec = tab.(i) in
        tab.(i) <- List.fold_left (predict_step i g) tab.(i) tab.(i);
        cond := List.length prec = List.length tab.(i); (* on n'a rien ajouté de nouveau donc les deux listes sont de même longueur *)
    done;
    tab

let complete_step (tab: (item list) array) (accu:item list) (it:item) : item list = match it with 
    | Item(v,(_,[]), j, il) -> List.fold_left 
                                (fun accu' it' -> begin match it' with (* pour chaque item qui attendait un v à l'étae j on ajoute un nouvel item ou on a lu un v qui commençait en j *)
                                    | Item(c, (ll, v'::rr'), k, il') when v' = Var(v) -> let ni = Item(c, (v'::ll, rr'), k, j::il') in 
                                                                    if not (List.mem ni accu') then 
                                                                        ni::accu'
                                                                    else accu'
                                    | _ -> accu' 
                                end) accu tab.(j)
    | _ -> accu 

let complete (i: int) (tab: (item list) array) : (item list) array = let cond = ref false in 
    while not !cond do 
        let prec = tab.(i) in
        tab.(i) <- List.fold_left (complete_step tab) tab.(i) tab.(i);
        cond := List.length prec = List.length tab.(i); (* on n'a rien ajouté donc les deux listes sont de même longueur*)
    done;
    tab

(*
let print_state tab (msg:string) =
        Printf.printf "%s" msg; 
        Array.fold_left (fun accu l -> Printf.printf "%d items; " (List.length l)) () tab; 
        Printf.printf "\n" 
*)

(* on veut pouvoir spécifier la fonction d'initialisation en fonction du focus courant *)
let earley (init_func: int -> (item list)) (g: grammar) (w: token array) : (item list) array = match g with 
    | Grammar(s, r) -> let n = Array.length w in 
                       let tab = Array.init (n+1) (init_func) in 
                       let t = ref tab in 
                        t := predict 0 !t g;
                        t := complete 0 !t;
                       for i = 1 to n do 
                            t := scan i w.(i-1) !t g; 
                            t := predict i !t g;
                            t := complete i !t
                       done;
                       !t

let get_fully_parsed (t: (item list) array) : (item list) array = let n = Array.length t in 
    Array.init n (fun i -> (List.filter (fun it -> match it with | Item(_, (_, []), _, _) -> true | _ -> false ) t.(i))) 

(*
open Grammar_focus

class words = 
object (s)
    val mutable w : word_list = 
        [
        [| "b" ; "a" ; "+" ; "(" ; "a" ; "*" ; "a"; ")"; "w"|];
        [| "a" ; "+" ; "a"|];
        [| "b"; "-" ; "(" ; "a" ; "/" ; "a" ; "+" ; "a" ; ")" |]
        ]

    method get = w

    method add word = w <- word :: w 

    method set w' = w <- w'

    method remove word = List.filter (fun w' -> w' <> word) w

    method clear = w <- []

    method of_yojson y = try (match word_list_of_yojson y with 
                                | Result.Ok(w) -> s#set w 
                                | _ -> failwith "Invalid serialization of words") with 
                        Failure s -> Jsutils.firebug s;
    method to_yojson = word_list_to_yojson w 
end

let w = new words 
*)
(* TEST *)
(*
let g = Grammar("S", [Rules("S", [Production([Var("E")])]); 
    Rules("E", [Production([Var("E"); Item("+"); Var("F")]); Production([Var("E"); Item("-"); Var("F")]); Production([Var("F")])]);
    (*Rules("N", [Production([Var("N"); Item("*"); Var("F")]); Production([Var("N"); Item("/"); Var("F")]); Production([Var("F")])]);*)
    Rules("F", [Production([Item("a")]); (*Production([Item("-"); Var("F")]); Production([Item("+"); Var("F")]); *)Production([Item("("); Var("E"); Item(")")])])]) 

let w = [| "b" ; "a" ; "+" ; "(" ; "a" ; ")"; "w"|]

let t = earley (init_earley g) g w
*)
(* let b =  Printf.printf "A t'on le bon resultat ? : %B" (Array.for_all2 equal_l t' t) *)
