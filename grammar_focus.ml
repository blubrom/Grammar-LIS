open Focus

type grammar_ctx = rule list_ctx

type focus = rule list_focus


let is_empty_focus = function
  | _ -> true
  | _ -> false

(* focus moves *)

(* DERIVED *)
let rec focus_up : focus -> (focus * path) option = function
  | AtExpr (e, Root) -> None
  | AtExpr (e, ctx) -> Some (focus_expr_up e ctx)
  | AtFlower (f, ctx) -> Some (focus_flower_up f ctx)

(* conversions between focus and (expr,path) *)
			  
(* DERIVED *)
let rec focus_of_path_focus path : focus -> focus (* raises Invalid_path *) = function
  | AtExpr (e,ctx) -> focus_of_path_expr ctx (path,e)
  | AtFlower (f,ctx) -> focus_of_path_flower ctx (path,f)
		
let expr_path_of_focus (foc : focus) : expr * path =
  let rec aux foc path =
    match focus_up foc, foc with
    | None, AtExpr (e,Root) -> e, path
    | None, _ -> assert false
    | Some (foc',path'), _ -> aux foc' (path'@path) in
  aux foc []

let focus_of_expr_path (e, path : expr * path) : focus =
  focus_of_path_expr Root (path,e)

let focus_down (foc : focus) : focus option =
  try Some (focus_of_path_focus [DOWN] foc)
  with Invalid_path -> None
    
let focus_right (foc : focus) : focus option =
  match focus_up foc with
  | None -> None
  | Some (foc',path') ->
     try Some (focus_of_path_focus (path'@[RIGHT]) foc')
     with Invalid_path -> None

let focus_left (foc : focus) : focus option =
  match focus_up foc with
  | None -> None
  | Some (foc',path') ->
     match List.rev path' with
     | [] -> None
     | DOWN::_ -> None
     | RIGHT::path'' ->
        try Some (focus_of_path_focus path'' foc')
        with Invalid_path -> None

let rec focus_succ (foc : focus) : focus option =
  match focus_down foc with
  | Some foc' -> Some foc'
  | None -> focus_succ_aux foc
and focus_succ_aux foc =
  match focus_right foc with
  | Some foc' -> Some foc'
  | None ->
     match focus_up foc with
     | Some (foc',_) -> focus_succ_aux foc'
     | None -> None

let rec focus_pred (foc : focus) : focus option =
  match focus_left foc with
  | Some foc' -> focus_pred_down_rightmost foc'
  | None ->
     match focus_up foc with
     | Some (foc',_) -> Some foc'
     | None -> None
and focus_pred_down_rightmost foc =
  match focus_down foc with
  | None -> Some foc
  | Some foc' -> focus_pred_rightmost foc'
and focus_pred_rightmost foc =
  match focus_right foc with
  | Some foc' -> focus_pred_rightmost foc'
  | None -> focus_pred_down_rightmost foc
                           
(* focus (de)serialization *)

let focus_to_yojson (foc : focus) : Yojson.Safe.t =
  let e, path = expr_path_of_focus foc in
  `Assoc [ "expression", Jsoniq.expr_to_yojson e;
	   "path", Focus.path_to_yojson path ]

let focus_of_yojson (x : Yojson.Safe.t) : (focus,string) Result.result =
  match x with
  | `Assoc ["expression", x_e; "path", x_path] ->
     Result.bind
       (expr_of_yojson x_e)
       (fun e ->
	Result.bind
	  (path_of_yojson x_path)
	  (fun path ->
	   let foc = focus_of_expr_path (e,path) in
	   Result.Ok foc))
  | _ -> Result.Error "Invalid serialization of a focus"
      
(* focus transformations and navigation paths *)
			   
let initial_focus = ("X",Epsilon),([],[])