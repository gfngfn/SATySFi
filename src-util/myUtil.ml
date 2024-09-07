
exception RemainsToBeImplemented of string


let remains_to_be_implemented msg =
  raise (RemainsToBeImplemented(msg))


let string_of_uchar_list (uchs : Uchar.t list) : string =
  let buffer = Buffer.create ((List.length uchs) * 4) in
  List.iter (fun u -> Uutf.Buffer.add_utf_8 buffer u) uchs;
  Buffer.contents buffer


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


let basename_abs (abspath : abs_path) =
  Filename.basename (AbsPath.to_string abspath)


let make_abs_path pathstr =
  AbsPath.of_string_exn pathstr


let get_abs_path_string =
  AbsPath.to_string


let make_absolute_if_relative ~(origin : string) (s : string) : abs_path =
  let abspath_str = if Filename.is_relative s then Filename.concat origin s else s in
  make_abs_path abspath_str


let append_to_abs_directory (absdir : abs_path) (filename : string) : abs_path =
  make_abs_path (Filename.concat (get_abs_path_string absdir) filename)


let dirname (abspath : abs_path) : abs_path =
  make_abs_path (Filename.dirname (get_abs_path_string abspath))


let basename (abspath : abs_path) : string =
  Filename.basename (get_abs_path_string abspath)


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
