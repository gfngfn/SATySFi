
type file_path = string


let uchar_of_char ch =
  Uchar.of_int (Char.code ch)


let ascii_capital_of_index i =
  Uchar.of_int ((Char.code 'A') + i)


let ascii_small_of_index i =
  Uchar.of_int ((Char.code 'a') + i)


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


let string_of_file (srcpath : file_path) : string =
  let ic = open_in_bin srcpath in
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
    Buffer.contents buf
  with
  | Failure(_) -> close_in ic; assert false
