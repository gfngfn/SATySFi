
module Impl : sig

  type component = string
  [@@deriving show]

  type t
  [@@deriving show]

  val compare : t -> t -> int

  val of_string : string -> t option

  val to_string : t -> string

  val to_relative_string : from:t -> t -> string

  val to_relative_string_if_descendant : from:t -> t -> string

  val to_components : t -> component list

end = struct

  (** the type for components each of which stand for a single directory. *)
  type component = string
  [@@deriving show { with_path = false }]

  type t = AbsPath of component list
  [@@deriving show { with_path = false }]

  type non_normal_component =
    | Component of component
    | Current
    | Parent


  let compare (AbsPath(compos1)) (AbsPath(compos2)) =
    List.compare String.compare compos1 compos2


  let make_non_normal_components (uchs : Uchar.t list) : non_normal_component =
    match UtfUtil.encode_utf8 uchs with
    | "" | "." -> Current
    | ".."     -> Parent
    | s        -> Component(s)


  let rec separate_to_non_normal_components (ncompoacc : non_normal_component Alist.t) (uchacc : Uchar.t Alist.t) (uchs : Uchar.t list) : non_normal_component list =
    match uchs with
    | [] ->
        let ncompo_last = make_non_normal_components (Alist.to_list uchacc) in
        Alist.to_list (Alist.extend ncompoacc ncompo_last)

    | uch_first :: uchs_rest ->
        if Uchar.equal uch_first (Uchar.of_char '/') then
          let ncompo = make_non_normal_components (Alist.to_list uchacc) in
          separate_to_non_normal_components
            (Alist.extend ncompoacc ncompo)
            Alist.empty
            uchs_rest
        else
          separate_to_non_normal_components
            ncompoacc
            (Alist.extend uchacc uch_first)
            uchs_rest


  let normalize (ncompos : non_normal_component list) : (component list) option =
    let open OptionMonad in
    let* compoacc =
      ncompos |> foldM (fun compoacc ncompo ->
        match ncompo with
        | Current          -> return compoacc
        | Parent           -> Alist.chop_last compoacc >>= fun (compoacc, _) -> return compoacc
        | Component(compo) -> return (Alist.extend compoacc compo)
      ) Alist.empty
    in
    return (Alist.to_list compoacc)


  let of_string (s : string) : t option =
    let uchs = UtfUtil.decode_utf8 s in
    match uchs with
    | [] ->
        None

    | uch_first :: uchs_rest ->
        if Uchar.equal uch_first (Uchar.of_char '/') then
          let ncompos = separate_to_non_normal_components Alist.empty Alist.empty uchs_rest in
          match normalize ncompos with
          | None         -> None
          | Some(compos) -> Some(AbsPath(compos))
        else
          None


  let to_string (AbsPath(compos) : t) : string =
    Printf.sprintf "/%s" (String.concat "/" compos)


  type relative_component =
    | RelParent
    | RelComponent of string


  let stringify_relative_component = function
    | RelParent           -> ".."
    | RelComponent(compo) -> compo


  let rec get_relative_components compos_seen_from compos_target =
    match (compos_seen_from, compos_target) with
    | (compo_seen_from :: compos_seen_from_rest, compo_target :: compos_target_rest) ->
        if String.equal compo_seen_from compo_target then
          get_relative_components compos_seen_from_rest compos_target_rest
        else
          let ncompos0 = List.map (fun _ -> RelParent) compos_seen_from in
          let ncompos1 = List.map (fun compo -> RelComponent(compo)) compos_target in
          List.append ncompos0 ncompos1

    | (_ :: _, []) ->
        List.map (fun _ -> RelParent) compos_seen_from

    | ([], _) ->
        List.map (fun compo -> RelComponent(compo)) compos_target


  let to_relative_string ~from:(AbsPath(compos_seen_from) : t) (AbsPath(compos_target) : t) =
    let relcompos = get_relative_components compos_seen_from compos_target in
    match relcompos with
    | []     -> "."
    | _ :: _ -> String.concat "/" (List.map stringify_relative_component relcompos)


  let to_relative_string_if_descendant ~from:(AbsPath(compos_seen_from) : t) (AbsPath(compos_target) as abspath_target : t) =
    let relcompos = get_relative_components compos_seen_from compos_target in
    match relcompos with
    | []                   -> "."
    | RelComponent(_) :: _ -> String.concat "/" (List.map stringify_relative_component relcompos)
    | RelParent :: _       -> to_string abspath_target


  let to_components (AbsPath(compos) : t) = compos

end


include Impl


let of_string_exn (s : string) : t =
  match of_string s with
  | None          -> assert false
  | Some(abspath) -> abspath


let make_absolute_if_relative ~(origin : t) (s : string) : t =
  of_string_exn (if Filename.is_relative s then Filename.concat (to_string origin) s else s)


let append_to_directory (absdir : t) (relpath : string) : t =
  of_string_exn (Filename.concat (to_string absdir) relpath)


let dirname (abspath : t) : t =
  of_string_exn (Filename.dirname (to_string abspath))


let basename (abspath : t) : string =
  Filename.basename (to_string abspath)


let replace_extension ~(extension_without_dot : string) (abspath : t) : t =
  let abspathstr_without_dot_and_extension = Filename.remove_extension (to_string abspath) in
  of_string_exn (Printf.sprintf "%s.%s" abspathstr_without_dot_and_extension extension_without_dot)
