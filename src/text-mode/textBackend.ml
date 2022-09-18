
type context_main = {
  indent      : int;
  escape_list : (Uchar.t list * Uchar.t list) list;
    [@printer (fun ppf _ -> Format.fprintf ppf "<escape-list>")]
}
[@@deriving show { with_path = false; }]


let get_initial_text_mode_context () = {
  indent      = 0;
  escape_list = [];
}


let deepen_indent i tctx =
  { tctx with indent = tctx.indent + (max i 0); }


let get_indent tctx =
  tctx.indent


let set_escape_list elst tctx =
  { tctx with escape_list = elst; }


let rec prefix uchlst1 uchlst2 =
  match (uchlst1, uchlst2) with
  | ([], _)                    -> Some(uchlst2)
  | (_, [])                    -> None
  | (uch1 :: tl1, uch2 :: tl2) -> if Uchar.equal uch1 uch2 then prefix tl1 tl2 else None


let rec first_match uchlst elst =
  match elst with
  | [] ->
      None

  | (ulfrom, ulto) :: etail ->
      begin
        match prefix ulfrom uchlst with
        | None          -> first_match uchlst etail
        | Some(uchrest) -> Some((ulto, uchrest))
      end


let uchar_line_feed = Uchar.of_int 0x0A
let uchar_space = Uchar.of_int 0x20

let spaces i =
  List.init i (fun _ -> uchar_space)


let insert_indent i uchlst =
  uchlst |> List.fold_left (fun acc uch ->
    if Uchar.equal uch uchar_line_feed then
      Alist.append (Alist.extend acc uchar_line_feed) (spaces i)
    else
      Alist.extend acc uch
  ) Alist.empty |> Alist.to_list


let stringify uchlst tctx =
  let elst = tctx.escape_list in
  let rec escape acc uchlst =
    match uchlst with
    | [] ->
        Alist.to_list acc

    | uchhead :: uchtail ->
        begin
          match first_match uchlst elst with
          | Some((ulto, uchrest)) -> escape (Alist.append acc ulto) uchrest
          | None                  -> escape (Alist.extend acc uchhead) uchtail
        end
  in
  let uchlst = escape Alist.empty uchlst in
  insert_indent tctx.indent uchlst
