open Grammar
module Lis = Grammar_lis

(* LIS building *)
let make_lis (args : (string * string) list) = new Lis.lis

let html_of_word : Grammar_syntax.word -> Html.t = function 
  | `Var v -> Html.span ~classe:"word-var" v
  | `Token t -> Html.span ~classe:"word-token" t

(* UI widgets *)
			      
let w_focus =
  new Widget_focus.widget
      ~id:"lis-focus"
      ~html_of_word
      
let render_place place k =
  Jsutils.firebug "XML of place";
  let xml = Grammar_syntax.syn_focus place#focus in
  Jsutils.firebug "focus#set_syntax";
  w_focus#set_syntax xml;
  w_focus#on_focus_change (fun foc ->
    let p = new Grammar_lis.place place#lis foc in
    k ~push_in_history:false p);
  w_focus#on_focus_up (fun () -> 
    match Grammar_focus.focus_up place#focus with
     | Some (foc,_) -> let p = new Grammar_lis.place place#lis foc in
	      k ~push_in_history:false p
     | None -> ());
  w_focus#on_focus_delete (fun () ->
    match Grammar_focus.delete place#focus with
     | Some foc -> let p = new Grammar_lis.place place#lis foc in
	      k ~push_in_history:true p
     | None -> ())


let handle_document_keydown ev place k =
  let open Js_of_ocaml in
  if not (Js.to_bool ev##.altKey) && not (Js.to_bool ev##.ctrlKey)
  then
    let foc = place#focus in
    let push_in_history, new_foc_opt =
      match ev##.keyCode with
      | 37 (* ArrowLeft *) -> false, Grammar_focus.focus_pred foc
      | 38 (* ArrowUp *) -> false,
         ( match Grammar_focus.focus_up place#focus with
           | None -> None
           | Some (f,_) -> Some f )
      | 39 (* ArrowRight *) -> false, Grammar_focus.focus_succ foc
      | 40 (* ArrowDown *) -> false, Grammar_focus.focus_down foc
      | 46 (* Delete *) -> true, Grammar_focus.delete foc
      | _ -> true, None in
    match new_foc_opt with
    | None -> false
    | Some new_foc ->
       let new_place = new Grammar_lis.place place#lis new_foc in
       k ~push_in_history new_place;
       true
  else false
  
let error_message : exn -> string = function 
  | Failure msg -> msg
  | exn -> "Unexpected error: " ^ Printexc.to_string exn

let _ =
  Webapp.start
    ~make_lis
    ~render_place
    ~handle_document_keydown
    ~error_message
