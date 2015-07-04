(* module Mcdout *)
  open Types
  open Typeenv

  exception IllegalOut of string

  let report_error errmsg =
    print_string ("! [ERROR IN OUT] " ^ errmsg ^ ".") ; print_newline ()

  (* abstract_tree -> string *)
  let rec main value =
    string_of_abstract_tree 0 value

  (* int -> abstract_tree -> string *)
  and string_of_abstract_tree indent value =
    match value with
    | StringEmpty -> ""

    | Concat(vf, vl) ->
        (string_of_abstract_tree indent vf)
        ^ (string_of_abstract_tree indent vl)

    | StringConstant(c) -> c

    | DeeperIndent(value_content) -> string_of_abstract_tree (indent + 1) value_content

    | BreakAndIndent -> "\n" ^ (if indent > 0 then String.make (indent * 2) ' ' else "")

    | ReferenceFinal(value_key) ->
        let str_key = string_of_abstract_tree indent value_key in
        ( try
            match !(Hashtbl.find global_hash_env str_key) with
            | MutableValue(mutvalue) -> string_of_abstract_tree indent mutvalue
            | _                      -> raise (IllegalOut("this cannot happen:\n"
                                          ^ "    reference key \"" ^ str_key ^ "\" contains non-mutable value")) 
          with
          | Not_found -> raise (IllegalOut("undefined reference key \"" ^ str_key ^ "\""))
        )

    | other -> raise (IllegalOut("this cannot happen:\n    cannot output\n\n      " ^ (string_of_ast other)))
