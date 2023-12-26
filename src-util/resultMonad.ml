let ( >>= ) x f =
  match x with
  | Ok(v)         -> f v
  | Error(_) as e -> e

let return v = Ok(v)

let err e = Error(e)

let ( let* ) = ( >>= )

let foldM f acc vs =
  vs |> List.fold_left (fun res v ->
    res >>= fun acc ->
    f acc v
  ) (return acc)

let mapM f vs =
  vs |> foldM (fun acc v ->
    f v >>= fun y ->
    return @@ Alist.extend acc y
  ) Alist.empty >>= fun acc ->
  return (Alist.to_list acc)

let optionM f = function
  | None    -> return None
  | Some(v) -> f v >>= fun y -> return @@ Some(y)
