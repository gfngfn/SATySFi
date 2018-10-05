

type middle_record =
  {
    pre_contents        : Omd.t;
    current_heading     : Omd.t;
    current_accumulated : Omd.element Alist.t;
    accumulated         : (Omd.t * Omd.t) Alist.t;
  }

type accumulator =
  | Beginning of Omd.element Alist.t
  | Middle    of middle_record

type element =
  | Raw     of Omd.t
  | Section of Omd.t * t

and t = element list


let finish_section (midrcd : middle_record) =
  let inner = Alist.to_list midrcd.current_accumulated in
  let pair = (midrcd.current_heading, inner) in
  Alist.extend midrcd.accumulated pair


let normalize_section nomf md =
  let acc =
    md |> List.fold_left (fun acc mde ->
      match nomf mde with
      | Some(heading) ->
          begin
            match acc with
            | Beginning(eacc) ->
                Middle{
                  pre_contents        = Alist.to_list eacc;
                  current_heading     = heading;
                  current_accumulated = Alist.empty;
                  accumulated         = Alist.empty;
                }

            | Middle(midrcd) ->
                let mainacc = finish_section midrcd in
                Middle{
                  pre_contents        = midrcd.pre_contents;
                  current_heading     = heading;
                  current_accumulated = Alist.empty;
                  accumulated         = mainacc;
                }
          end

      | None ->
          begin
            match acc with
            | Beginning(eacc) ->
                Beginning(Alist.extend eacc mde)

            | Middle(midrcd) ->
                Middle({ midrcd with
                  current_accumulated = Alist.extend midrcd.current_accumulated mde;
                })
          end

    ) (Beginning(Alist.empty))
  in
  match acc with
  | Beginning(acc) ->
      (Alist.to_list acc, [])

  | Middle(midrcd) ->
      let mainacc = finish_section midrcd in
      (midrcd.pre_contents, Alist.to_list mainacc)


let normalize_h3 md =
  let (pre, inner) = normalize_section (function Omd.H3(heading) -> Some(heading) | _ -> None) md in
  Raw(pre) :: (inner |> List.map (fun (heading, mdsub) -> Section(heading, [Raw(mdsub)])))


let normalize_h2 md =
  let (pre, inner) = normalize_section (function Omd.H2(heading) -> Some(heading) | _ -> None) md in
  Raw(pre) :: (inner |> List.map (fun (heading, mdsub) -> Section(heading, normalize_h3 mdsub)))


let normalize_h1 md =
  let (pre, inner) = normalize_section (function Omd.H1(heading) -> Some(heading) | _ -> None) md in
  Raw(pre) :: (inner |> List.map (fun (heading, mdsub) -> Section(heading, normalize_h2 mdsub)))


let decode s =
  let md = Omd.of_string s in
    normalize_h1 md
