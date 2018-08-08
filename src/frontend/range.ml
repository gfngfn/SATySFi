
type t =
  | Dummy of string
  | Normal of string * int * int * int * int
[@@deriving show]


let dummy msg = Dummy(msg)


let is_dummy rng =
  match rng with
  | Dummy(_) -> true
  | _        -> false


let message rng =
  match rng with
  | Dummy(msg)            -> msg
  | Normal(_, _, _, _, _) -> "*NORMAL*"


let to_string rng =
  let s = string_of_int in
    match rng with
    | Dummy(msg) ->
        "dummy range '" ^ msg ^ "'"

    | Normal(fname, ln1, pos1, ln2, pos2) ->
        if ln1 = ln2 then
          "\"" ^ fname ^ "\", line " ^ (s ln1) ^ ", characters " ^ (s pos1) ^ "-" ^ (s pos2)
        else
          "\"" ^ fname ^ "\", line " ^ (s ln1) ^ ", character " ^ (s pos1) ^ " to line " ^ (s ln2) ^ ", character " ^ (s pos2)


let unite rng1 rng2 =
  match (rng1, rng2) with
  | (Normal(fname, ln1, pos1, _, _), Normal(_, _, _, ln2, pos2)) -> Normal(fname, ln1, pos1, ln2, pos2)
  | (Normal(_, _, _, _, _), _)                                   -> rng1
  | (_, Normal(_, _, _, _, _))                                   -> rng2
  | _                                                            -> Dummy("unite")


let make fname ln pos1 pos2 =
  Normal(fname, ln, pos1, ln, pos2)
