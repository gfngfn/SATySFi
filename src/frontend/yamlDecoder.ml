
open MyUtil


type error =
  | FieldNotFound of string
  | NotAFloat
  | NotAString
  | NotABool
  | NotAnArray
  | NotAnObject
  | OtherMessage of string


let pp_error (ppf : Format.formatter) =
  let p = Format.fprintf in
  function
  | FieldNotFound(field) -> p ppf "field '%s' not found" field
  | NotAFloat            -> p ppf "not a float value"
  | NotAString           -> p ppf "not a string value"
  | NotABool             -> p ppf "not a Boolean value"
  | NotAnArray           -> p ppf "not an array"
  | NotAnObject          -> p ppf "not an object"
  | OtherMessage(msg)    -> p ppf "%s" msg


type 'a t = Yaml.value -> ('a, error) result


let run (d : 'a t) (s : string) : ('a, error) result =
  let open ResultMonad in
  match Yaml.of_string s with
  | Ok(yval)       -> d yval
  | Error(`Msg(s)) -> err (OtherMessage(s))


let succeed (a : 'a) : 'a t =
  fun _ -> Ok(a)


let failure (msg : string) : 'a t =
  fun _ -> Error(OtherMessage(msg))


let bind (d : 'a t) (df : 'a -> 'b t) : 'b t =
fun yval ->
  match d yval with
  | Ok(a)         -> df a yval
  | Error(_) as e -> e


let ( >>= ) = bind


let get_scheme (field : string) (d : 'a t) (k : unit -> ('a, error) result) : 'a t =
  let open ResultMonad in
  function
  | `O(keyvals) ->
      begin
        match
          List.find_map (fun (k, v) -> if String.equal k field then Some(v) else None) keyvals
        with
        | None    -> k ()
        | Some(v) -> d v
      end

  | _ ->
      err NotAnObject


let get (field : string) (d : 'a t) : 'a t =
  let open ResultMonad in
  get_scheme field d (fun () -> err (FieldNotFound(field)))


let get_opt (field : string) (d : 'a t) : ('a option) t =
  let d_some =
    d >>= fun v -> succeed (Some(v))
  in
  let open ResultMonad in
  get_scheme field d_some (fun () -> return None)


let get_or_else (field : string) (d : 'a t) (default : 'a) : 'a t =
  let open ResultMonad in
  get_scheme field d (fun () -> return default)


let number : float t =
  let open ResultMonad in
  function
  | `Float(x) -> return x
  | _         -> err NotAFloat


let string : string t =
  let open ResultMonad in
  function
  | `String(x) -> return x
  | _          -> err NotAString


let bool : bool t =
  let open ResultMonad in
  function
  | `Bool(x) -> return x
  | _        -> err NotABool


let list (d : 'a t) : ('a list) t =
  let open ResultMonad in
  function
  | `A(yvals) ->
      yvals |> List.fold_left (fun res yval ->
        res >>= fun acc ->
        d yval >>= fun a ->
        return (Alist.extend acc a)
      ) (return Alist.empty) >>= fun acc ->
      return (Alist.to_list acc)

  | _ ->
      err NotAnArray


type 'a branch = string * 'a t


let branch (field : string) (branches : ('a branch) list) ~on_error:(errorf : string -> string) : 'a t =
  get field string >>= fun tag_gotten ->
  match
    branches |> List.find_map (fun (tag_candidate, d) ->
      if String.equal tag_gotten tag_candidate then Some(d) else None
    )
  with
  | None    -> failure (errorf tag_gotten)
  | Some(d) -> d


let ( ==> ) (label : string) (d : 'a t) : 'a branch = (label, d)


let map (f : 'a -> 'b) (d : 'a t) : 'b t =
  let open ResultMonad in
  fun yval ->
    d yval >>= fun a ->
    return (f a)


let map2 (f : 'a1 -> 'a2 -> 'b) (d1 : 'a1 t) (d2 : 'a2 t) : 'b t =
  let open ResultMonad in
  fun yval ->
    d1 yval >>= fun a1 ->
    d2 yval >>= fun a2 ->
    return (f a1 a2)


let map3 (f : 'a1 -> 'a2 -> 'a3 -> 'b) (d1 : 'a1 t) (d2 : 'a2 t) (d3 : 'a3 t) : 'b t =
  let open ResultMonad in
  fun yval ->
    d1 yval >>= fun a1 ->
    d2 yval >>= fun a2 ->
    d3 yval >>= fun a3 ->
    return (f a1 a2 a3)
