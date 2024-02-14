
(* TODO: separate UTF-8-related functions to one module *)
let decode_utf8 (str_utf8 : string) : Uchar.t list =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String(str_utf8)) in
  let rec loop (uchacc : Uchar.t Alist.t) =
    match Uutf.decode decoder with
    | `Await        -> assert false
    | `End          -> Alist.to_list uchacc
    | `Malformed(_) -> assert false
    | `Uchar(uch)   -> loop (Alist.extend uchacc uch)
  in
  loop Alist.empty


let encode_utf8 (uchs : Uchar.t list) : string =
  let buffer = Buffer.create (List.length uchs * 4) in
  let encoder = Uutf.encoder `UTF_8 (`Buffer(buffer)) in
  uchs |> List.iter (fun uch -> Uutf.encode encoder (`Uchar(uch)) |> ignore);
  Uutf.encode encoder `End |> ignore;
  Buffer.contents buffer


(** the type for components each of which stand for a single directory. *)
type component = string
[@@deriving show { with_path = false }]

type t = AbsPath of component list
[@@deriving show { with_path = false }]

type non_normal_component =
  | Component of component
  | Current
  | Parent
[@@deriving show { with_path = false }]


let make_non_normal_components (uchs : Uchar.t list) : non_normal_component =
  match encode_utf8 uchs with
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
  let uchs = decode_utf8 s in
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
