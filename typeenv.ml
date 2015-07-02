open Types

type t = (var_name * type_struct) list

let empty = []

let add tyenv varnm tystr = (varnm, tystr) :: tyenv

let rec find tyenv varnm =
  match tyenv with
  | []               -> raise Not_found
  | (vn, ts) :: tail ->
      if vn = varnm then ts else find tail varnm
