open Grammar
open Jsutils
module Lis = Grammar_lis

(* LIS building *)
let make_lis (args : (string * string) list) = new Lis.lis

let html_of_word : Grammar_syntax.word -> Html.t = function 
  | `Var v -> Html.span ~classe:"word-var" v
  | `Token t -> Html.span ~classe:"word-token" t

let html_info_of_input (input : Grammar_syntax.input) : Html.input_info =
  (* exceptions are captured by caller of updates *)
  match input with
  | `FileString input -> Html.fileElt_info (fun fname_contents k -> input#set fname_contents; k ())
  | `Separator input -> Html.string_info (fun contents k -> input#set contents; k ())
  | `Word input -> Html.string_info (fun contents k -> input#set contents; k ())
  | `SelectSyntagm (syntagms, input) -> Html.selectElt_info syntagms (fun x k -> input#set x; k ()) 
  | `Symbol input -> Html.string_info (fun contents k -> input#set contents; k ())
  | `Syntagm input -> Html.string_info (fun contents k -> input#set contents; k ())


(* UI widgets *)

class results
  ~(id : Html.id) (* where to insert the widget in the DOM *)
  =
object
  method set_contents (ext:Grammar_extent.extent) : unit = match ext with 
    |Extent(e) ->
        let strl = List.map (fun ext_w -> let xml = Grammar_syntax.syn_extent_word ext_w in Html.syntax ~html_of_word xml) e
        in let html =  Html.ul (List.map (fun s -> (None, None, None, s)) strl) in 
        Jsutils.jquery_set_innerHTML (Html.selector_id id) html
  end

let w_result =  new results 
    ~id:"lis-results"

let suggestions_cols = ["col-md-4 col-xs-12";	"col-md-4 col-xs-12"; "col-md-4 col-xs-12"]

let w_suggestions : Grammar_suggestions.suggestion Widget_suggestions.widget =
  new Widget_suggestions.widget
      ~id:"lis-suggestions"
      ~html_of_suggestion:(fun ~input_dico sugg ->
			   Html.syntax ~input_dico
				       ~html_of_word ~html_info_of_input
				       (Grammar_syntax.syn_transf sugg))
			      
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
     | None -> ());
  Jsutils.firebug "place#eval";
  place#eval
    (fun ext -> Jsutils.firebug "ext computed"; 
                w_result#set_contents ext)
    (fun suggestions -> 
      Jsutils.firebug "suggestions computed";
      w_suggestions#set_suggestions suggestions_cols suggestions;
      let suggestion_handler =
        (fun sugg ->
          match place#activate sugg with
          | Some p -> k ~push_in_history:true p
          | None -> assert false) in
      w_suggestions#on_suggestion_selection suggestion_handler)


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
    ~error_message;
