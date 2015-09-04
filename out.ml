open Types

exception IllegalOut of string


(* abstract_tree -> string *)
let rec main value = out 0 value

(* int -> abstract_tree -> string *)
and out indent value =
  match value with
  | StringEmpty -> ""
  | StringConstant(c) -> c
  | Concat(vf, vl) -> (out indent vf) ^ (out indent vl)
  | DeeperIndent(value_content) -> out (indent + 1) value_content
  | BreakAndIndent -> "\n" ^ (if indent > 0 then String.make (indent * 2) ' ' else "")
  | ReferenceFinal(value_key) ->
      let str_key = out indent value_key in
      ( try
          match !(Hashtbl.find global_hash_env str_key) with
          | MutableValue(mutvalue) -> out indent mutvalue
          | _                      -> raise (IllegalOut("this cannot happen:\n"
                                        ^ "    reference key \"" ^ str_key ^ "\" contains non-mutable value")) 
        with
        | Not_found -> raise (IllegalOut("undefined reference key \"" ^ str_key ^ "\""))
      )
  | other -> raise (IllegalOut("this cannot happen: cannot output\n\n    " (* ^ (string_of_ast other)*) ))
