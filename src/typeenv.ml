open Types

module ModuleTree = HashTree.Make(String)

type single_environment = (var_name * poly_type) list

type t = single_environment ModuleTree.t


let empty = ModuleTree.empty []


let from_list (lst : (var_name * poly_type) list) = ModuleTree.empty lst


let map f (tyenv : t) = ModuleTree.update tyenv [] (List.map f)


let add (tyenv : t) (varnm : var_name) (pty : poly_type) =
  let rec aux sgl =
    match sgl with
    | []                                -> (varnm, pty) :: []
    | (vn, pt) :: tail  when vn = varnm -> (varnm, pty) :: tail
    | (vn, pt) :: tail                  -> (vn, pt) :: (aux tail)
  in
    ModuleTree.update tyenv [] aux


let find (tyenv : t) (varnm : var_name) =
  let rec aux sgl =
    match sgl with
    | []                               -> raise Not_found
    | (vn, ts) :: tail when vn = varnm -> ts
    | (vn, ts) :: tail                 -> aux tail
  in
    aux (ModuleTree.get tyenv [])


(* ---- following are all for debugging --- *)
(*
let string_of_type_environment (tyenv : t) (msg : string) =
  let rec iter (tyenv : t) =
    match tyenv with
    | []               -> ""
    | (vn, ts) :: tail ->
            "    #  "
              ^ ( let len = String.length vn in if len >= 16 then vn else vn ^ (String.make (16 - len) ' ') )
       ^ " : " ^ ((* string_of_mono_type ts *) "type") ^ "\n"
              ^ (iter tail)
  in
      "    #==== " ^ msg ^ " " ^ (String.make (58 - (String.length msg)) '=') ^ "\n"
    ^ (iter tyenv)
    ^ "    #================================================================\n"


let string_of_control_sequence_type (tyenv : t) =
  let rec iter (tyenv : t) =
    match tyenv with
    | []               -> ""
    | (vn, ts) :: tail ->
        ( match String.sub vn 0 1 with
          | "\\" ->
              "    #  "
                ^ ( let len = String.length vn in if len >= 16 then vn else vn ^ (String.make (16 - len) ' ') )
                ^ " : " ^ ((* string_of_mono_type ts *) "type") ^ "\n" (* remains to be implemented *)
          | _    -> ""
        ) ^ (iter tail)
  in
      "    #================================================================\n"
    ^ (iter tyenv)
    ^ "    #================================================================\n"
*)
