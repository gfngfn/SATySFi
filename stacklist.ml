(* module Stacklist *)
  open Types

  type 'a t = 'a list

  let empty = []

  (* 'a t ref -> 'a *)
  let pop rfstk =
    match !rfstk with
      [] -> raise StackUnderflow
    | head :: tail -> (rfstk := tail ; head)

  (* 'a t ref -> 'a -> unit *)
  let push rfstk cnt =
    rfstk := cnt :: !rfstk

  (* 'a t ref -> 'a *)
  let top rfstk =
    match !rfstk with
      [] -> raise StackUnderflow
    | head :: tail -> head

  (* 'a t -> 'a t -> 'a t *)
  let rec concat lsta lstb =
    match lsta with
      [] -> lstb
    | head :: tail -> head :: (concat tail lstb)

  (* 'a t -> 'a list *)
  let rec to_list stk =
    match stk with
      [] -> []
    | head :: tail -> concat (to_list tail) [head]
