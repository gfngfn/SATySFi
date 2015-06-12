(* module Mcdout *)
  open Types

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

    | ListCons(vf, vl) -> raise (IllegalOut("cannot output list"))
    | EndOfList -> raise (IllegalOut("cannot output list"))

    | NumericConstant(nc) -> raise (IllegalOut("cannot output int " ^ (string_of_int nc)))
    | BooleanConstant(bc) -> raise (IllegalOut("cannot output bool " ^ (string_of_bool bc)))
    | FuncWithEnvironment(_, _, _) -> raise (IllegalOut("cannot output function"))
    | NoContent -> raise (IllegalOut("cannot output no-content"))
    | _ -> raise (IllegalOut("unknown error"))
