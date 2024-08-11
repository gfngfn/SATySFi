let ( >>= ) x f =
  match x with
  | None as n -> n
  | Some(v)   -> f v

let return v = Some(v)
