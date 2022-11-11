
exception RemainsToBeImplemented of string

type abs_path = AbsPath of string
[@@deriving show { with_path = false }]

type lib_path = LibPath of string


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


module OptionMonad = struct
  let ( >>= ) x f =
    match x with
    | None as n -> n
    | Some(v)   -> f v

  let return v = Some(v)
end

module ResultMonad = struct
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

end

module EscapeMonad = struct
  let ( >>= ) x f =
    match x with
    | `Continue(v)    -> f v
    | `Escape(_) as e -> e

  let continue v = `Continue(v)

  let escape e = `Escape(e)

  let force x =
    match x with
    | `Escape(e) -> e
end


let ( += ) r n =
  r := !r + n


let ( @|> ) = ( |> )
  (* ----
      right-associative version;
      `y @|> x @|> f ` is equivalent to `f x y`
     ---- *)


let first_some f lst =
  let rec aux = function
    | [] ->
        None

    | x :: xs ->
        let opt = f x in
        begin
          match opt with
          | Some(_) -> opt
          | None    -> aux xs
        end
  in
    aux lst


let open_in_abs (AbsPath(pathstr)) =
  open_in pathstr


let open_in_bin_abs (AbsPath(pathstr)) =
  open_in_bin pathstr


let open_out_abs (AbsPath(pathstr)) =
  open_out pathstr


let dirname_abs (AbsPath(pathstr)) =
  Filename.dirname pathstr


let basename_abs (AbsPath(pathstr)) =
  Filename.basename pathstr


let make_abs_path pathstr = AbsPath(pathstr)

let make_lib_path pathstr = LibPath(pathstr)

let get_abs_path_string (AbsPath(pathstr)) = pathstr

let get_lib_path_string (LibPath(pathstr)) = pathstr

let get_abs_path_extension (AbsPath(pathstr)) = Filename.extension pathstr


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
