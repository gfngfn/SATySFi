(* module Mcdout *)
  open Types

  (* abstract_tree -> string *)
  let rec mcdout abstr =

    match abstr with
      EmptyAbsBlock -> ""
    | AbsBlock(abstr_former, abstr_latter) ->
        (mcdout abstr_former) ^ (mcdout abstr_latter)
    | Output(c) -> c
    | _ -> raise IllegalOut
