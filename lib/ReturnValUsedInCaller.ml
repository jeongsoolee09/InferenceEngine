open ListMonad
open InfixOperators
open Utils
open Chain
open GraphRepr
module LV = G.LiteralVertex

let move_to_line (lines : string array) (goto : int) : string =
  (* NOTE line numbers start from 1 *)
  lines.(goto - 1)


let returnval_not_used_really =
  let cache = Hashtbl.create 777 in
  fun (chain_slice : ChainSlice.t) : bool ->
    match Hashtbl.find_opt cache chain_slice with
    | None ->
        let out =
          assert (ChainSlice.is_call chain_slice || ChainSlice.is_voidcall chain_slice) ;
          let caller = ChainSlice.get_current_method chain_slice
          and location =
            List.hd_exn @@ LocationSet.to_int_list @@ ChainSlice.get_location chain_slice
          in
          let decl_file = Method.get_declaration_file caller in
          let decl_file_line_by_line = Array.of_list @@ In_channel.read_lines decl_file in
          print_endline
          @@ F.sprintf "%s: %d for %s" decl_file location (ChainSlice.to_string chain_slice) ;
          let line_containing_method = move_to_line decl_file_line_by_line location in
          (not @@ String.is_prefix ~prefix:"if" (String.strip line_containing_method))
          && (not @@ String.is_prefix ~prefix:"while" (String.strip line_containing_method))
        in
        Hashtbl.add cache chain_slice out ;
        out
    | Some res ->
        res


let filter_really_returnval_not_used (chain_slices : ChainSlice.t list) : ChainSlice.t list =
  List.filter chain_slices ~f:(fun chain_slice ->
      Method.is_api (ChainSlice.get_callee chain_slice) && returnval_not_used_really chain_slice )
