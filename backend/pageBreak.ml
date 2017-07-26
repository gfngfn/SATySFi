open HorzBox

(* for test *)
let print_evaled_vert_box evvb =
  match evvb with
  | EvVertLine(_, _, evhblst) ->
      begin
        Format.printf "@[(vert@ " ;
        evhblst |> List.iter (function
          | EvHorzFixedBoxAtom(wid, FixedString(_, str)) -> Format.printf "@[(fixed@ \"%s\"@ :@ %s)@]@ " str (SkipLength.show wid)
          | EvHorzFixedBoxAtom(wid, FixedEmpty(_))       -> Format.printf "@[(fixed-empty@ %s)@]@ " (SkipLength.show wid)
          | EvHorzOuterBoxAtom(wid, _)                   -> Format.printf "@[(outer@ :@ %s)@]@ " (SkipLength.show wid)
        ) ;
        Format.printf ")@]@ " ;
      end
  | EvVertFixedEmpty(vskip) ->
      begin
        Format.printf "@[(vskip@ %s)@]@ " (SkipLength.show vskip) ;
      end


let main (pdfscheme : HandlePdf.t) (vblst : vert_box list) =

  let chop_single_page imvbacc =
  (* Some((List.rev imvbacc, [])) *)
    None
    (* temporary; should be dependent upon accumulated current contribution list *)
  in

  let determine_heights (imvblst : intermediate_vert_box list) =
    (* temporary; should determine the height of vertical boxes *)
    imvblst |> List.map (fun imvb ->
      match imvb with
      | ImVertLine(hgt, dpt, evhblst) -> EvVertLine(hgt, dpt, evhblst)
      | ImVertFixedBreakable(vskip)   -> EvVertFixedEmpty(vskip)
    )
  in

  (* --
    `let (evvblstO, imvbaccO, vblstO) = pickup_page imvbaccI vblstI`
    - `vblstI`   : input vertical list
    - `imvbaccI` : (inverted) recent contribution list before picking up a page
    - `evvblstO` : vertical list (of evaluated form) for single page
    - `imvbaccO` : (inverted) recent contribution list after picking up a page (to be read next)
    - `vblstO`   : vertical list to be read next
  -- *)
  let rec pickup_page (imvbacc : intermediate_vert_box list) (vblst : vert_box list)
      : evaled_vert_box list * (intermediate_vert_box list * vert_box list) option =
    match vblst with
    | [] ->
        let imvblst = List.rev imvbacc in
        let evvblst = determine_heights imvblst in
          (evvblst, None)

    | VertFixedBreakable(vskip) :: tail ->
        let imvbaccnew = ImVertFixedBreakable(vskip) :: imvbacc in
        begin
          match chop_single_page imvbaccnew with
          | None                             -> pickup_page imvbaccnew tail
          | Some((imvblstpage, imvbaccrest)) ->
              let evvblstpage = determine_heights imvblstpage in
                (evvblstpage, Some((imvbaccrest, tail)))
        end

    | VertParagraph(leading, hblst) :: tail ->
        let imvblst = LineBreak.main leading hblst in
        let imvbaccnew = List.rev_append imvblst imvbacc in
        begin
          match chop_single_page imvbaccnew with
          | None                             -> pickup_page imvbaccnew tail
          | Some((imvblstpage, imvbaccrest)) ->
              let evvblstpage = determine_heights imvblstpage in
              (evvblstpage, Some((imvbaccrest, tail)))
        end
  in

  let rec iteration (pdfscheme : HandlePdf.t) (imvbacc : intermediate_vert_box list) (vblst : vert_box list) =
    let (evvblstpage, opt) = pickup_page imvbacc vblst in
    let pdfschemenext = pdfscheme |> HandlePdf.write_page Pdfpaper.a4 evvblstpage in
      match opt with
      | None -> begin HandlePdf.write_to_file pdfschemenext ; end
      | Some((imvbaccnext, vblstnext)) ->
        (* begin: for debug *)
          let () = Format.printf "--------@\n" in
          let () = List.iter print_evaled_vert_box evvblstpage in
          let () = Format.printf "@\n--------@\n" in
        (* end: for debug *)
          iteration pdfschemenext imvbaccnext vblstnext
  in
    iteration pdfscheme [] vblst


let penalty_break_space = 100
let penalty_soft_hyphen = 1000


let () =
  let ( ~% ) = SkipLength.of_pdf_point in
  begin
    FontInfo.initialize () ;
    let font0 = ("TimesIt", ~% 16.) in
    let font1 = ("Hlv", ~% 16.) in
    let word s = HorzFixedBoxAtom(FixedString(font0, s)) in
    let word1 s = HorzFixedBoxAtom(FixedString(font1, s)) in
    let space = HorzDiscretionary(penalty_break_space, Some(HorzOuterBoxAtom(OuterEmpty(~% 8., ~% 1., ~% 4.))), None, None) in
    let indentation = HorzFixedBoxAtom(FixedEmpty(~% 64.)) in
    let fill = HorzOuterBoxAtom(OuterFil) in
    let paragraph_skip = ~% 32.0 in
    let soft_hyphen = HorzDiscretionary(penalty_soft_hyphen, None, Some(HorzFixedBoxAtom(FixedString(font0, "-"))), None) in
    let soft_hyphen1 = HorzDiscretionary(penalty_soft_hyphen, None, Some(HorzFixedBoxAtom(FixedString(font1, "-"))), None) in
    let vblst =
      [
        VertParagraph(~% 24., [
          word1 "discre"; soft_hyphen; word1 "tionary"; space; word1 "hyphen"; space;
          word1 "discre"; soft_hyphen1; word1 "tionary"; space; word1 "hyphen"; space;
  (*        word1 "5000"; space; word1 "cho-yen"; space; word1 "hoshii!"; space; *)
          word "discre"; soft_hyphen; word "tionary"; space; word "hyphen"; space;
          word "discre"; soft_hyphen; word "tionary"; space; word "hyphen"; space;
          word "The"; space; word "quick"; space; word "brown"; space; word "fox"; space;
          word "jumps"; space; word "over"; space; word "the"; space; word "lazy"; space; word "dog.";
          space;
          word "My"; space; word "quiz"; space; word "above"; space; word "the"; space; word "kiwi"; space; word "juice"; space;
          word "needs"; space; word "price"; soft_hyphen ; word "less"; space; word "fixing."; fill;
        ]);
        VertFixedBreakable(paragraph_skip);
        VertParagraph(~% 24., [
          indentation;
          word1 "Lorem"; space; word1 "ipsum"; space; word "dolor"; space; word "sit"; space; word "amet,"; space;
          word "consectetur"; space; word "adipiscing"; space; word "elit,"; space;
          word "sed"; space; word "do"; space; word "eiusmod"; space; word "tempor"; space; word "incididunt"; space;
          word "ut"; space; word "labore"; space; word "et"; space; word "dolore"; space; word "magna"; space; word "aliqua."; fill;
        ]);
      ]
    in
    let pdfscheme = HandlePdf.create_empty_pdf "hello2.pdf" in
    begin
      main pdfscheme vblst ;
    end
  end
