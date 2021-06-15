
module Focus = Grammar_focus
module Extension = Grammar_extent
(*
module Suggestions = Grammar_suggestions
*)

type suggestion = unit 
		       
class place (lis : lis) (focus : Focus.focus) =
object
  inherit [lis,Focus.focus,Extension.extent,suggestion] Lis.place lis focus

  val mutable extent : Extension.extent option = None

  val words : (Grammar.token array) list= 
    [
      [| "b" ; "a" ; "+" ; "(" ; "a" ; "*" ; "a"; ")"; "w"|];
      [| "a" ; "+" ; "a"|];
      [| "b"; "-" ; "(" ; "a" ; "/" ; "a" ; "+" ; "a" ; ")" |]
    ]
								    
  method eval k_extent k_suggestions = 
      let ext = Extension.compute_extent words focus in
      extent <- Some ext;
      k_extent ext;

  method activate sugg = None

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