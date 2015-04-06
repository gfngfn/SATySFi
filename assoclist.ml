(* module Assoclist *)
  open Types

  type ('a, 'b) t = ('a * 'b) list

  let empty = []

  let rec add key value asclst =
  (*
    (key, value) :: asclst
  *)
    match asclst with
      [] -> [(key, value)]
    | (k, v) :: tail ->
        if (compare key k) == 0 then
          (key, value) :: tail
        else
          (k, v) :: (add key value tail)
  

  (* 'a list -> 'b list -> ('a, 'b) t -> ('a, 'b) t *)
  let rec add_list key_list value_list asclst =
    match key_list with
      [] -> asclst
    | key_head :: key_tail -> (
          match value_list with
            [] -> raise IllegalLengthOfLists
          | value_head :: value_tail -> (
              let asclstsub = (add key_head value_head asclst) in
                (add_list key_tail value_tail asclstsub)
            )
        )

  (* ('a, 'b) t -> 'a -> 'b *)
  let rec get_value asclst key =
    match asclst with
      [] -> raise ValueNotFound
    | (k, v) :: tail ->
      if (compare k key) == 0 then v else get_value tail key

  (* for test *)
  let rec print_key asclst =
    match asclst with
      [] -> ()
    | (k, v) :: tail -> ( print_string k ; print_key tail )
