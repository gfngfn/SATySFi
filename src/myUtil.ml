
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


type abs_path = AbsPath of string
[@@deriving show { with_path = false }]

type lib_path = LibPath of string


let open_in_abs (AbsPath(pathstr)) =
  open_in pathstr


let basename_abs (AbsPath(pathstr)) =
  Filename.basename pathstr


let make_abs_path pathstr = AbsPath(pathstr)

let make_lib_path pathstr = LibPath(pathstr)

let get_abs_path_string (AbsPath(pathstr)) = pathstr

let get_lib_path_string (LibPath(pathstr)) = pathstr


module AbsPath = struct
  type t = abs_path

  let compare ap1 ap2 = String.compare (get_abs_path_string ap1) (get_abs_path_string ap2)
end


let read_file (abspath : abs_path) : (string, string) result =
  let open ResultMonad in
  try
    return @@ Core.In_channel.read_all (get_abs_path_string abspath)
  with
  | Sys_error(s) ->
      err s
