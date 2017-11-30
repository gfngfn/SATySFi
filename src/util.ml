
let list_some lst =
  let accres =
    lst |> List.fold_left (fun acc opt ->
      match opt with
      | None    -> acc
      | Some(p) -> p :: acc
    ) []
  in
    List.rev accres


let list_fold_adjacent f init lst =
  let rec aux leftopt init lst =
    match lst with
    | [] ->
        init

    | head :: [] ->
        let initnew = f init head leftopt None in
        initnew

    | head :: ((right :: _) as tail) ->
        let initnew = f init head leftopt (Some(right)) in
        aux (Some(head)) initnew tail
  in
  aux None init lst


let option_map f opt =
  match opt with
  | None    -> None
  | Some(x) -> Some(f x)


let pickup lst predicate e =
  match lst |> List.filter predicate with
  | head :: _ -> Ok(head)
  | []        -> Error(e)


let ( += ) r n =
  r := !r + n


let ( @|> ) = ( |> )
  (* ----
      right-associative version;
      `y @|> x @|> f ` is equivalent to `f x y`
     ---- *)
