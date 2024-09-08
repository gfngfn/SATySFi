
(* TODO: remove this *)
let string_of_uchar_list =
  UtfUtil.encode_utf8


let rec range i j =
  if i > j then [] else
    i :: (range (i + 1) j)


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


let ( @|> ) = ( |> )
  (* ----
      right-associative version;
      `y @|> x @|> f ` is equivalent to `f x y`
     ---- *)


type abs_path = AbsPath.t
[@@deriving show { with_path = false }]


let encode_yaml (yaml : Yaml.value) : string =
  match Yaml.to_string ~encoding:`Utf8 ~layout_style:`Block ~scalar_style:`Plain yaml with
  | Ok(data) -> data
  | Error(_) -> assert false


type 'a cycle =
  | Loop  of 'a
  | Cycle of 'a TupleList.t
[@@deriving show { with_path = false; }]


let map_cycle f = function
  | Loop(v)   -> Loop(f v)
  | Cycle(vs) -> Cycle(TupleList.map f vs)
