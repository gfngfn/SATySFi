open Types

module ModuleTree = HashTree.Make(String)

module VarMap = Map.Make(String)

type single_environment = poly_type VarMap.t

type t = single_environment ModuleTree.t


let empty : t = ModuleTree.empty VarMap.empty


let from_list (lst : (var_name * poly_type) list) =
  let vmapinit = List.fold_left (fun vmap (varnm, pty) -> VarMap.add varnm pty vmap) VarMap.empty lst in
    ModuleTree.empty vmapinit


let add (tyenv : t) (varnm : var_name) (pty : poly_type) =
  ModuleTree.update tyenv [] (VarMap.add varnm pty)


let find (tyenv : t) (varnm : var_name) =
  VarMap.find varnm (ModuleTree.get tyenv [])


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
