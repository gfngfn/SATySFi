
exception RemainsToBeImplemented of string

type abs_path = AbsPath of string

type lib_path = LibPath of string


let remains_to_be_implemented msg =
  raise (RemainsToBeImplemented(msg))


let uchar_of_char ch =
  Uchar.of_int (Char.code ch)


let ascii_capital_of_index i =
  Uchar.of_int ((Char.code 'A') + i)


let ascii_small_of_index i =
  Uchar.of_int ((Char.code 'a') + i)


let string_of_uchlst uchlst =
  let buffer = Buffer.create ((List.length uchlst) * 4) in
    List.iter (fun u -> Uutf.Buffer.add_utf_8 buffer u) uchlst;
    Buffer.contents buffer


let rec range i j =
  if i > j then [] else
    i :: (range (i + 1) j)


let list_make n c =
  let rec aux acc n =
    if n <= 0 then List.rev acc else
      aux (c :: acc) (n - 1)
  in
  aux [] n


let list_fold_left_index f init lst =
  let (_, ret) =
    lst |> List.fold_left (fun (i, acc) x -> (i + 1, f i acc x)) (0, init)
  in
    ret


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


let string_of_file (abspath : abs_path) : (string, string) result =
  try
    let ic = open_in_bin_abs abspath in
    let bufsize = 65536 in
    let stepsize = 65536 in
    let buf = Buffer.create bufsize in
    let bytes = Bytes.create stepsize in
    let flag = ref true in
    try
      while !flag do
        let c = input ic bytes 0 bufsize in
        if c = 0 then
          flag := false
        else
          Buffer.add_subbytes buf bytes 0 c
      done;
      close_in ic;
      let s = Buffer.contents buf in
      Ok(s)
    with
    | Failure(_) -> close_in ic; assert false
  with
  | Sys_error(msg) -> Error(msg)

(*
let string_of_file (srcpath : file_path) : string =
  let bufsize = 65536 in  (* arbitrary constant; the initial size of the buffer for loading font format file *)
  let buf : Buffer.t = Buffer.create bufsize in
  let byt : bytes = Bytes.create bufsize in
  let ic =
    try
      open_in_bin srcpath
    with
    | Sys_error(msg) -> raise (FailToLoadFontOwingToSystem(srcpath, msg))
  in

  let rec aux () =
    let c = input ic byt 0 bufsize in
      if c = 0 then
        begin
          close_in ic;
          Buffer.contents buf
        end
      else
        begin
          Buffer.add_subbytes buf byt 0 c;
          aux ()
        end
  in
  try
    aux ()
  with
  | Failure(_)     -> begin close_in ic; raise (FailToLoadFontOwingToSize(srcpath)) end
  | Sys_error(msg) -> begin close_in ic; raise (FailToLoadFontOwingToSystem(srcpath, msg)) end
*)

let make_abs_path pathstr = AbsPath(pathstr)

let make_lib_path pathstr = LibPath(pathstr)

let get_abs_path_string (AbsPath(pathstr)) = pathstr

let get_lib_path_string (LibPath(pathstr)) = pathstr
