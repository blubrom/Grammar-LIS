
module Focus = Grammar_focus
module Extension = Grammar_extent
module Suggestions = Grammar_suggestions
		       
class place (lis : lis) (focus : Focus.focus) =
object
  inherit [lis,Focus.focus,Extent.extent,Suggestions.suggestion] Lis.place lis focus

  val mutable extent : Extent.extent option = None
								    
  method eval k_extent k_suggestions = ()

  method activate sugg = ()

  method abort = ()

  method json = Focus.focus_to_yojson focus

  method results =
    match extent with
    | None -> failwith "Results are not yet available"
    | Some ext -> Jsoniq_files.mime_contents_of_extent ext
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