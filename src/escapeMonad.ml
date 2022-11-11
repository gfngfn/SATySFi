let ( >>= ) x f =
  match x with
  | `Continue(v)    -> f v
  | `Escape(_) as e -> e

let continue v = `Continue(v)

let escape e = `Escape(e)

let force x =
  match x with
  | `Escape(e) -> e
