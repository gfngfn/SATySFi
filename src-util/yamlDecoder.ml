
type context_element =
  | Field of string
  | Index of int
[@@deriving show { with_path = false }]

type context =
  context_element list
[@@deriving show { with_path = false }]


let show_yaml_context (context : context) : string =
  match context with
  | [] ->
      "top"

  | _ :: _ ->
      context |> List.map (function
      | Field(field) -> Printf.sprintf ".%s" field
      | Index(index) -> Printf.sprintf ".[%d]" index
      ) |> String.concat ""


module type ErrorType = sig
  type t

  val parse_error : string -> t

  val field_not_found : context -> string -> t

  val not_a_float : context -> t

  val not_a_string : context -> t

  val not_a_bool : context -> t

  val not_an_array : context -> t

  val not_an_object : context -> t

  val branch_not_found : context -> string list -> string list -> t

  val more_than_one_branch_found : context -> string list -> string list -> t
end



module Make (Err : ErrorType) = struct

  type 'a t = context_element Alist.t * Yaml.value -> ('a, Err.t) result


  let run (d : 'a t) (s : string) : ('a, Err.t) result =
    let open ResultMonad in
    match Yaml.of_string s with
    | Ok(yval)       -> d (Alist.empty, yval)
    | Error(`Msg(s)) -> err (Err.parse_error s)


  let succeed (a : 'a) : 'a t =
    fun _ -> Ok(a)


  let failure (errf : context -> Err.t) : 'a t =
    fun (context, _) -> Error(errf (Alist.to_list context))


  let bind (d : 'a t) (df : 'a -> 'b t) : 'b t =
    fun s ->
      match d s with
      | Ok(a)         -> df a s
      | Error(_) as e -> e


  let ( >>= ) = bind


  let get_scheme (field : string) (d : 'a t) (k_absent : context_element Alist.t -> ('a, Err.t) result) : 'a t =
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
          err @@ Err.not_an_object (Alist.to_list context)


  let get (field : string) (d : 'a t) : 'a t =
    let open ResultMonad in
    get_scheme field d (fun context -> err @@ Err.field_not_found (Alist.to_list context) field)


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
      | _         -> err @@ Err.not_a_float (Alist.to_list context)


  let string : string t =
    fun (context, yval) ->
      let open ResultMonad in
      match yval with
      | `String(x) -> return x
      | _          -> err @@ Err.not_a_string (Alist.to_list context)


  let bool : bool t =
    fun (context, yval) ->
      let open ResultMonad in
      match yval with
      | `Bool(x) -> return x
      | _        -> err @@ Err.not_a_bool (Alist.to_list context)


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
          err @@ Err.not_an_object (Alist.to_list context)


  type 'a branch = string * 'a t


  let branch (branches : ('a branch) list) : 'a t =
    fun (context, yval) ->
      let open ResultMonad in
      match yval with
      | `O(fields) ->
          let hits =
            fields |> List.concat_map (fun (tag_got, yval_got) ->
              branches |> List.filter_map (fun (tag_expected, d) ->
                if String.equal tag_got tag_expected then
                  Some((tag_got, yval_got, d))
                else
                  None
              )
            )
          in
          begin
            match hits with
            | [] ->
                let expected_tags = branches |> List.map Stdlib.fst in
                let got_tags = fields |> List.map Stdlib.fst in
                err @@ Err.branch_not_found (Alist.to_list context) expected_tags got_tags

            | [ (tag, yval_sub, d) ] ->
                d (Alist.extend context (Field(tag)), yval_sub)

            | _ ->
                let expected_tags = fields |> List.map Stdlib.fst in
                let got_tags = hits |> List.map (fun (tag, _, _) -> tag) in
                err @@ Err.more_than_one_branch_found (Alist.to_list context) expected_tags got_tags
          end

      | _ ->
          err @@ Err.not_an_object (Alist.to_list context)


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

end
