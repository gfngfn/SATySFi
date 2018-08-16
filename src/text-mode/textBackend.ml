
type text_mode_context = {
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


let stringify uchlst tctx =
  let elst = tctx.escape_list in
  let rec aux acc uchlst =
    match uchlst with
    | [] ->
        Alist.to_list acc

    | uchhead :: uchtail ->
        begin
          match first_match uchlst elst with
          | Some((ulto, uchrest)) -> aux (Alist.append acc ulto) uchrest
          | None                  -> aux (Alist.extend acc uchhead) uchtail
        end
  in
    aux Alist.empty uchlst
