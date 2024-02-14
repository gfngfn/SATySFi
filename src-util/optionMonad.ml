let ( >>= ) x f =
  match x with
  | None as n -> n
  | Some(v)   -> f v

let return v = Some(v)

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
