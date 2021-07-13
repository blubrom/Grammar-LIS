
module Focus = Grammar_focus
module Extension = Grammar_extent
module Suggestions = Grammar_suggestions 
		       
class place (lis : lis) (focus : Focus.focus) =
object
  inherit [lis,Focus.focus,(Parsing.item list array * Grammar.syntagm) list option,Suggestions.suggestion] Lis.place lis focus

  (*val mutable extent : ((Parsing.item array * syntagm) list) option = None*)
								    
  method eval k_extent k_suggestions = 
      let ext = Extension.compute_extent focus in
      k_extent ext;
      let lfsugg = Suggestions.suggestions ext focus in
      k_suggestions lfsugg

  method activate sugg = 
    let transf = sugg in
    match Focus.apply_transf transf focus with
    | Some new_focus -> Some (new place lis new_focus)
    | None -> None

  method abort = ()

  method json = Focus.focus_to_yojson focus

  method results = ("","")
end
and lis =
object (self)
  inherit [place] Lis.lis

  method initial_place =
    new place (self :> lis) Focus.initial_focus

  method place_of_json json =
    match Focus.focus_of_yojson json with
    | Result.Ok foc -> new place (self :> lis) foc
    | Result.Error msg -> invalid_arg msg
end