
open MyUtil


type context_element =
  | Field of string
  | Index of int

type context =
  context_element list

type error_main =
  | FieldNotFound of string
  | NotAFloat
  | NotAString
  | NotABool
  | NotAnArray
  | NotAnObject
  | OtherMessage of string

type error =
  context * error_main


let show_error ((context, errmain) : error) =
  let s_main =
    match errmain with
    | FieldNotFound(field) -> Printf.sprintf "field '%s' not found" field
    | NotAFloat            -> Printf.sprintf "not a float value"
    | NotAString           -> Printf.sprintf "not a string value"
    | NotABool             -> Printf.sprintf "not a Boolean value"
    | NotAnArray           -> Printf.sprintf "not an array"
    | NotAnObject          -> Printf.sprintf "not an object"
    | OtherMessage(msg)    -> Printf.sprintf "%s" msg
  in
  match context with
  | [] ->
      s_main

  | _ :: _ ->
      let s_context =
        context |> List.map (function
        | Field(field) -> Printf.sprintf ".%s" field
        | Index(index) -> Printf.sprintf ".[%d]" index
        ) |> String.concat ""
      in
      Printf.sprintf "%s (context: %s)" s_main s_context


type 'a t = context_element Alist.t * Yaml.value -> ('a, error) result


let run (d : 'a t) (s : string) : ('a, error) result =
  let open ResultMonad in
  match Yaml.of_string s with
  | Ok(yval)       -> d (Alist.empty, yval)
  | Error(`Msg(s)) -> err ([], OtherMessage(s))


let succeed (a : 'a) : 'a t =
  fun _ -> Ok(a)


let failure (msg : string) : 'a t =
  fun (context, _) -> Error(Alist.to_list context, OtherMessage(msg))


let bind (d : 'a t) (df : 'a -> 'b t) : 'b t =
  fun s ->
    match d s with
    | Ok(a)         -> df a s
    | Error(_) as e -> e


let ( >>= ) = bind


let get_scheme (field : string) (d : 'a t) (k_absent : context_element Alist.t -> ('a, error) result) : 'a t =
  fun (context, yval) ->
    let open ResultMonad in
    match yval with
    | `O(keyvals) ->
        begin
          match
            keyvals |> List.find_opt (fun (k, _v) -> String.equal k field)
              (* According to the specification of YAML, every key can occur at most once here. *)
          with
          | None         -> k_absent context
          | Some((k, v)) -> d (Alist.extend context (Field(k)), v)
        end

    | _ ->
        err (Alist.to_list context, NotAnObject)


let get (field : string) (d : 'a t) : 'a t =
  let open ResultMonad in
  get_scheme field d (fun context -> err (Alist.to_list context, FieldNotFound(field)))


let get_opt (field : string) (d : 'a t) : ('a option) t =
  let d_some =
    d >>= fun v -> succeed (Some(v))
  in
  let open ResultMonad in
  get_scheme field d_some (fun _ -> return None)


let get_or_else (field : string) (d : 'a t) (default : 'a) : 'a t =
  let open ResultMonad in
  get_scheme field d (fun _ -> return default)


let number : float t =
  fun (context, yval) ->
    let open ResultMonad in
    match yval with
    | `Float(x) -> return x
    | _         -> err (Alist.to_list context, NotAFloat)


let string : string t =
  fun (context, yval) ->
    let open ResultMonad in
    match yval with
    | `String(x) -> return x
    | _          -> err (Alist.to_list context, NotAString)


let bool : bool t =
  fun (context, yval) ->
    let open ResultMonad in
    match yval with
    | `Bool(x) -> return x
    | _        -> err (Alist.to_list context, NotABool)


let list (d : 'a t) : ('a list) t =
  fun (context, yval) ->
    let open ResultMonad in
    match yval with
    | `A(yvals) ->
        yvals |> foldM (fun (index, acc) yval ->
          d (Alist.extend context (Index(index)), yval) >>= fun a ->
          return (index + 1, Alist.extend acc a)
        ) (0, Alist.empty) >>= fun (_, acc) ->
        return (Alist.to_list acc)

    | _ ->
        err (Alist.to_list context, NotAnArray)


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
