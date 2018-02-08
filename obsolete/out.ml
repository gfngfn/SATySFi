(*
open Types

exception IllegalOut of string


let string_of_break_and_indent indent = "\n" ^ (if indent > 0 then String.make (indent * 2) ' ' else "")


(* abstract_tree -> string *)
let rec main value = stringify 0 (erase_soft_break (flatten value))

(* abstract_tree -> output_unit list *)
and flatten value =
  match value with
  | StringEmpty                 -> []
  | StringConstant(c)           -> [OString(c)]
  | Concat(v1, v2)              ->
      let o1 = flatten v1 in
      let o2 = flatten v2 in
        begin
          match (o1, o2) with
          | (OString(c1) :: [], OString(c2) :: tail) -> OString(c1 ^ c2) :: tail
          | _                                        -> o1 @ o2
        end
  | DeeperIndent(value_content) -> [ODeepen] @ (flatten value_content) @ [OShallow]
  | BreakAndIndent              -> [OBreakAndIndent]
  | SoftBreakAndIndent          -> [OSoftBreakAndIndent]
  | ReferenceFinal(value_key)   ->
      let str_key = out 0 value_key in
        begin
          try
            match !(Hashtbl.find global_hash_env str_key) with
            | Location(loc) -> flatten (!loc)
            | _             ->
                begin
                  print_string ("!!!! reference key \"" ^ str_key ^ "\" contains non-mutable value") ;
                  assert false
                end
          with
          | Not_found -> raise (IllegalOut("undefined reference key \"" ^ str_key ^ "\""))
        end
  | other ->
      begin
        print_string ("!!!! cannot output\n\n    " ^ (Display.string_of_ast other)) ;
        assert false
      end


and erase_soft_break (opu : output_unit list) =
  match opu with
  | []                                                 -> []
  | OBreakAndIndent :: OSoftBreakAndIndent :: tail     -> erase_soft_break (OBreakAndIndent :: tail)
  | OSoftBreakAndIndent :: OSoftBreakAndIndent :: tail -> erase_soft_break (OSoftBreakAndIndent :: tail)
  | OSoftBreakAndIndent :: OBreakAndIndent :: tail     -> erase_soft_break (OBreakAndIndent :: tail)
  | OSoftBreakAndIndent :: OShallow :: tail            -> erase_soft_break (OShallow :: tail)
  | head :: tail                                       -> head :: (erase_soft_break tail)


and stringify (indent : int) (opu : output_unit list) =
  match opu with
  | []                                         -> ""
  | OString(c) :: tail                         -> c ^ (stringify indent tail)
  | ( OBreakAndIndent :: OShallow :: tail
    | OSoftBreakAndIndent :: OShallow :: tail) -> (string_of_break_and_indent (indent - 1)) ^ (stringify (indent - 1) tail)
  | ( OBreakAndIndent :: tail
    | OSoftBreakAndIndent :: tail )            -> (string_of_break_and_indent indent) ^ (stringify indent tail)
  | ODeepen :: tail                            -> stringify (indent + 1) tail
  | OShallow :: tail                           -> stringify (indent - 1) tail


(* int -> abstract_tree -> string *)
and out (indent : int) (value : abstract_tree) : string =
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
  | DeeperIndent(_)    -> raise (IllegalOut("invalid string for reference key; it contains 'deeper' operation"))
  | BreakAndIndent     -> raise (IllegalOut("invalid string for reference key; it contains break"))
  | SoftBreakAndIndent -> raise (IllegalOut("invalid string for reference key; it contains soft break"))
  | ReferenceFinal(_)  -> raise (IllegalOut("invalid string for reference key; it contains '!!' operation"))
  | other              -> begin
                            print_string ("!!!! cannot output\n\n    " ^ (Display.string_of_ast other)) ;
                            assert false
                          end
*)
