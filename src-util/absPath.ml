
(** the type for components each of which stand for a single directory. *)
type component = string
[@@deriving show { with_path = false }]

type t = AbsPath of component list
[@@deriving show { with_path = false }]

type non_normal_component =
  | Component of component
  | Current
  | Parent


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


let of_string_exn (s : string) : t =
  let uchs = UtfUtil.decode_utf8 s in
  match uchs with
  | [] ->
      assert false

  | uch_first :: uchs_rest ->
      if Uchar.equal uch_first (Uchar.of_char '/') then
        let ncompos = separate_to_non_normal_components Alist.empty Alist.empty uchs_rest in
        match normalize ncompos with
        | None         -> assert false
        | Some(compos) -> AbsPath(compos)
      else
        assert false


let to_string (AbsPath(compos) : t) : string =
  Printf.sprintf "/%s" (String.concat "/" compos)


let to_components (AbsPath(compos) : t) = compos


let compare (AbsPath(compos1)) (AbsPath(compos2)) =
  List.compare String.compare compos1 compos2


type relative_component =
  | RelParent
  | RelComponent of string


let stringify_relative_component = function
  | RelParent           -> ".."
  | RelComponent(compo) -> compo


let make_relative ~from:(AbsPath(compos_seen_from) : t) (AbsPath(compos_target) : t) =
  let rec aux compos_seen_from compos_target =
    match (compos_seen_from, compos_target) with
    | (compo_seen_from :: compos_seen_from_rest, compo_target :: compos_target_rest) ->
        if String.equal compo_seen_from compo_target then
          aux compos_seen_from_rest compos_target_rest
        else
          let ncompos0 = List.map (fun _ -> RelParent) compos_seen_from in
          let ncompos1 = List.map (fun compo -> RelComponent(compo)) compos_target in
          List.append ncompos0 ncompos1

    | (_ :: _, []) ->
        List.map (fun _ -> RelParent) compos_seen_from

    | ([], _) ->
        List.map (fun compo -> RelComponent(compo)) compos_target
  in
  let relcompos = aux compos_seen_from compos_target in
  match relcompos with
  | []     -> "."
  | _ :: _ -> String.concat "/" (List.map stringify_relative_component relcompos)
