type t = string

let show t =
  let paren = Printf.sprintf "(%s)" in
  match t with
  | "" -> t
  | "before" | "mod" | "not" ->
    paren t
  | _ ->
    begin match t.[0] with
    | 'a' .. 'z'
    | 'A' .. 'Z' -> t
    | _ ->
      paren t
    end
