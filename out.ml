open Types

exception IllegalOut of string


let string_of_break_and_indent indent = "\n" ^ (if indent > 0 then String.make (indent * 2) ' ' else "")


(* abstract_tree -> string *)
let rec main value = stringify (erase_soft_break (flatten 0 value))

(* int -> abstract_tree -> output_unit list *)
and flatten indent value =
  match value with
  | StringEmpty                 -> []
  | StringConstant(c)           -> [OString(c)]
  | Concat(vf, vl)              -> (flatten indent vf) @ (flatten indent vl)
  | DeeperIndent(value_content) -> flatten (indent + 1) value_content
  | BreakAndIndent              -> [OBreakAndIndent(indent)]
  | SoftBreakAndIndent          -> [OSoftBreakAndIndent(indent)]
  | ReferenceFinal(value_key)   ->
      let str_key = out indent value_key in
        begin
          try
            match !(Hashtbl.find global_hash_env str_key) with
            | MutableValue(mutvalue) -> flatten indent mutvalue
            | _                      -> begin
                                          print_string ("!!!! reference key \"" ^ str_key ^ "\" contains non-mutable value") ;
                                          assert false
                                        end
          with
          | Not_found -> raise (IllegalOut("undefined reference key \"" ^ str_key ^ "\""))
        end
  | other -> begin print_string ("!!!! cannot output\n\n    " ^ (Display.string_of_ast other)) ; assert false end

and erase_soft_break opu =
  match opu with
  | []                 -> []
  | OBreakAndIndent(indent1) :: OSoftBreakAndIndent(_) :: tail ->
      erase_soft_break (OBreakAndIndent(indent1) :: tail)
  | OSoftBreakAndIndent(_) :: OSoftBreakAndIndent(indent2) :: tail ->
      erase_soft_break (OSoftBreakAndIndent(indent2) :: tail)
  | OSoftBreakAndIndent(_) :: OBreakAndIndent(indent2) :: tail ->
      erase_soft_break (OBreakAndIndent(indent2) :: tail)
  | head :: tail -> head :: (erase_soft_break tail)

and stringify opu =
  match opu with
  | []                                  -> ""
  | OString(c) :: tail                  -> c ^ (stringify tail)
  | OBreakAndIndent(indent) :: tail     -> (string_of_break_and_indent indent) ^ (stringify tail)
  | OSoftBreakAndIndent(indent) :: tail -> (string_of_break_and_indent indent) ^ (stringify tail)


(* int -> abstract_tree -> string *)
and out indent value =
  match value with
  | StringEmpty                 -> ""
  | StringConstant(c)           -> c
  | Concat(vf, vl)              -> (out indent vf) ^ (out indent vl)
(*
  | DeeperIndent(value_content) -> out (indent + 1) value_content
  | BreakAndIndent              -> string_of_break_and_indent indent
  | ReferenceFinal(value_key)   ->
      let str_key = out indent value_key in
        begin
          try
            match !(Hashtbl.find global_hash_env str_key) with
            | MutableValue(mutvalue) -> out indent mutvalue
            | _                      -> begin
                                          print_string ("!!!! reference key \"" ^ str_key ^ "\" contains non-mutable value") ;
                                          assert false
                                        end
          with
          | Not_found -> raise (IllegalOut("undefined reference key \"" ^ str_key ^ "\""))
        end
  | other -> begin print_string ("!!!! cannot output\n\n    " ^ (Display.string_of_ast other)) ; assert false end
*)
  | DeeperIndent(_)    -> raise (IllegalOut("invalid string for reference key; it consists 'deeper' operation"))
  | BreakAndIndent     -> raise (IllegalOut("invalid string for reference key; it contains break"))
  | SoftBreakAndIndent -> raise (IllegalOut("invalid string for reference key; it contains soft break"))
  | ReferenceFinal(_)  -> raise (IllegalOut("invalid string for reference key; it contains '!!' operation"))
  | other              -> begin
                            print_string ("!!!! cannot output\n\n    " ^ (Display.string_of_ast other)) ;
                            assert false
                          end
