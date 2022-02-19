open GraphRepr
open ListMonad
open InfixOperators
open SimilarityHandler
open Chain
open EdgeLabel
module G = GraphRepr.G

exception TODO

let move_to_line (lines : string array) (goto : int) : string =
  (* NOTE line numbers start from 1 *)
  lines.(goto - 1)


let location_is_correct (line : string) : bool = String.is_suffix ~suffix:";" (String.strip line)

let rectify_return_stmt_location (before_location : int) (file_containing_method : string) : int =
  (* NOTE: `file_containing_method` should be an absolute directory. *)
  let lines = Array.of_list @@ In_channel.read_lines file_containing_method in
  let current_location = ref before_location in
  let current_location_line = ref (move_to_line lines !current_location) in
  while not @@ location_is_correct !current_location_line do
    current_location := !current_location + 1 ;
    current_location_line := move_to_line lines !current_location
  done ;
  !current_location

let before_location = 110

let file_containing_method = "/Users/jslee/Taint-Analysis/Code/benchmarks/realworld/sagan/sagan-renderer/src/main/java/sagan/renderer/github/GithubClient.java"


