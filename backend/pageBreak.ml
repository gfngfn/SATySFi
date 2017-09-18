(* -*- coding: utf-8 -*- *)

open HorzBox

(* for test *)
let print_for_debug msg = ()
let ppf_for_debug =  Format.printf
let print_evaled_vert_box evvb =
  ()
(*
  match evvb with
  | EvVertLine(_, _, evhblst) ->
      begin
        Format.printf "@[(vert@ " ;
        evhblst |> List.iter (function
          | EvHorzFixedBoxAtom(wid, FixedString(_, str)) -> Format.printf "@[(fixed@ \"%s\"@ :@ %s)@]@ " str (Length.show wid)
          | EvHorzFixedBoxAtom(wid, FixedEmpty(_))       -> Format.printf "@[(fixed-empty@ %s)@]@ " (Length.show wid)
          | EvHorzOuterBoxAtom(wid, _)                   -> Format.printf "@[(outer@ :@ %s)@]@ " (Length.show wid)
        ) ;
        Format.printf ")@]@ " ;
      end
  | EvVertFixedEmpty(vskip) ->
      begin
        Format.printf "@[(vskip@ %s)@]@ " (Length.show vskip) ;
      end
*)

let page_height = Length.of_pdf_point 650.  (* temporary; should be variable *)


let main_scheme (pdfscheme : HandlePdf.t) (imvbacc : intermediate_vert_box list) (vblst : vert_box list) =

  let calculate_badness_of_page_break hgttotal =
    let hgtdiff = page_height -% hgttotal in
      if hgtdiff <% Length.zero then 10000 else
        int_of_float (hgtdiff /% (Length.of_pdf_point 0.1))
  in

  let chop_single_page imvbacc =
    let rec aux (vpbprev : pure_badness) (imvbacc : intermediate_vert_box list) (imvbaccbreakable : intermediate_vert_box list) (hgttotal : Length.t) (imvblst : intermediate_vert_box list) =
      match imvblst with
      | (ImVertLine(hgt, dpt, _) as head) :: tail ->
          let hgttotalnew = hgttotal +% hgt +% (Length.negate dpt) in
          let vpb = calculate_badness_of_page_break hgttotalnew in
          if vpb > vpbprev then
            let () = print_for_debug ("CL " ^ (Length.show hgttotal) ^ " ===> " ^ (Length.show hgttotalnew) ^ "\n") in  (* for debug *)
              Some((List.rev imvbacc, List.rev imvblst))  (* -- discard breakables -- *)
          else
            aux vpb (head :: (List.append imvbaccbreakable imvbacc)) [] hgttotalnew tail

      | (ImVertFixedBreakable(vskip) as head) :: tail ->
          let hgttotalnew = hgttotal +% vskip in
          let vpb = calculate_badness_of_page_break hgttotalnew in
          if vpb > vpbprev then
            let () = print_for_debug ("CB " ^ (Length.show hgttotal) ^ " ===> " ^ (Length.show hgttotalnew) ^ "\n") in  (* for debug *)
              Some((List.rev imvbacc, List.rev imvblst))  (* -- discard breakables -- *)
          else
            aux vpb imvbacc (head :: imvbaccbreakable) hgttotalnew tail

      | [] ->
          let () = print_for_debug ("CE " ^ (Length.show hgttotal) ^ " ===> None\n") in  (* for debug *)
            None
    in
    let imvblst = List.rev imvbacc in
      aux 100000 [] [] Length.zero imvblst
  in

  let determine_heights (imvblst : intermediate_vert_box list) =
    imvblst |> List.map (fun imvb ->
      match imvb with
      | ImVertLine(hgt, dpt, evhblst) -> EvVertLine(hgt, dpt, evhblst)
      | ImVertFixedBreakable(vskip)   -> EvVertFixedEmpty(vskip)
    )
  in

  (* --
    `let (evvblstO, Some(imvbaccO, vblstO)) = pickup_page imvbaccI vblstI`
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
          match chop_single_page imvbaccnew with  (* temporary; extremely inefficient *)
          | None                             -> pickup_page imvbaccnew tail
          | Some((imvblstpage, imvbaccrest)) ->
              let evvblstpage = determine_heights imvblstpage in
                (evvblstpage, Some((imvbaccrest, tail)))
        end

    | VertParagraph(leading, hblst) :: tail ->
        let imvblst = LineBreak.main leading hblst in
        let imvbaccnew = List.rev_append imvblst imvbacc in
        begin
          match chop_single_page imvbaccnew with  (* temporary; extremely inefficient *)
          | None                             -> pickup_page imvbaccnew tail
          | Some((imvblstpage, imvbaccrest)) ->
              let evvblstpage = determine_heights imvblstpage in
                (evvblstpage, Some((imvbaccrest, tail)))
        end
  in

  let rec iteration (pdfscheme : HandlePdf.t) (imvbacc : intermediate_vert_box list) (vblst : vert_box list) =
    let (evvblstpage, opt) = pickup_page imvbacc vblst in
    let pdfschemenext = pdfscheme |> HandlePdf.write_page Pdfpaper.a4 evvblstpage in
    (* begin: for debug *)
      let () = print_for_debug "--------\n" in
      let () = List.iter print_evaled_vert_box evvblstpage in
      let () = print_for_debug "\n--------\n" in
    (* end: for debug *)
        match opt with
        | None                           -> begin HandlePdf.write_to_file pdfschemenext; end
        | Some((imvbaccnext, vblstnext)) -> iteration pdfschemenext imvbaccnext vblstnext
  in
    iteration pdfscheme imvbacc vblst


let main_for_unit_test (pdfscheme : HandlePdf.t) (vblst : vert_box list) : unit =
  main_scheme pdfscheme [] vblst


let main (pdfscheme : HandlePdf.t) (imvblst : intermediate_vert_box list) : unit =
  main_scheme pdfscheme (List.rev imvblst) []


let penalty_break_space = 100
let penalty_soft_hyphen = 1000

(*
let () =
  let ( ~% ) = Length.of_pdf_point in
  begin
    FontInfo.initialize () ;
    let font0 = ("Arno", ~% 16.) in
    let font1 = (* ("Hlv", ~% 16.) *) font0 in
    let fontL = (* ("Hlv", ~% 32.) *) ("Arno", ~% 32.) in

    let fontK = (* ("KozMin", ~% 12.) *) ("Osaka", ~% 12.) in

    let word s = HorzPure(PHFixedString(font0, InternalText.of_utf_8 s)) in
    let word1 s = HorzPure(PHFixedString(font1, InternalText.of_utf_8 s)) in
    let wordL s = HorzPure(PHFixedString(fontL, InternalText.of_utf_8 s)) in

    let wordK s = HorzPure(PHFixedString(fontK, InternalText.of_utf_8 s)) in

    let margin = ~% 2. in
    let pads = {
      paddingL = ~% 2. +% margin;
      paddingR = ~% 2. +% margin;
      paddingT = ~% 2. +% margin;
      paddingB = ~% 2. +% margin;
    } in
    let decostd =
      (fun (xpos, ypos) wid hgt dpt ->
        let xposb = xpos +% margin in
        let hgtb = hgt -% margin in
        let dptb = dpt +% margin in
        let widb = wid -% margin *% 2. in
        [
          Rectangle((xposb, ypos +% dptb), (widb, hgtb -% dptb));
        ]
      )
    in
    let decoH =
      (fun (xpos, ypos) wid hgt dpt ->
        let xposb = xpos +% margin in
        let hgtb = hgt -% margin in
        let dptb = dpt +% margin in
        let widb = wid -% margin in
        [
          GeneralPath((xposb +% widb, ypos +% hgtb), [
            LineTo(xposb, ypos +% hgtb);
            LineTo(xposb, ypos +% dptb);
            LineTo(xposb +% widb, ypos +% dptb);
          ]);
        ]
      )
    in
    let decoM =
      (fun (xpos, ypos) wid hgt dpt ->
        let xposb = xpos in
        let hgtb = hgt -% margin in
        let dptb = dpt +% margin in
        let widb = wid in
        [
          GeneralPath((xposb, ypos +% hgtb), [LineTo(xposb +% widb, ypos +% hgtb)]);
          GeneralPath((xposb, ypos +% dptb), [LineTo(xposb +% widb, ypos +% dptb)]);
        ]
      )
    in
    let decoT =
      (fun (xpos, ypos) wid hgt dpt ->
        let xposb = xpos in
        let hgtb = hgt -% margin in
        let dptb = dpt +% margin in
        let widb = wid -% margin in
        [
          GeneralPath((xposb, ypos +% hgtb), [
            LineTo(xposb +% widb, ypos +% hgtb);
            LineTo(xposb +% widb, ypos +% dptb);
            LineTo(xposb, ypos +% dptb);
          ]);
        ]
      )
    in
    let framed hblst = HorzPure(PHOuterFrame(pads, decostd, hblst)) in
    let iframed hblst = HorzPure(PHInnerFrame(pads, decostd, hblst)) in
    let fframed wid hblst = HorzPure(PHFixedFrame(pads, wid, decostd, hblst)) in
    let bframed hblst = HorzFrameBreakable(pads, ~% 5., ~% 5., decostd, decoH, decoM, decoT, hblst) in
    let space = HorzDiscretionary(penalty_break_space, Some(PHOuterEmpty(~% 6., ~% 2., ~% 3.)), None, None) in
    let space1 = HorzDiscretionary(penalty_break_space, Some(PHOuterEmpty(~% 7.5, ~% 3., ~% 3.)), None, None) in
    let spaceL = HorzDiscretionary(penalty_break_space, Some(PHOuterEmpty(~% 16., ~% 2., ~% 6.)), None, None) in
    let indentation = HorzPure(PHFixedEmpty(~% 64.)) in
    let fill = HorzPure(PHOuterFil) in
    let leading = ~% 24. in
    let paragraph_skip = ~% 16. in
    let soft_hyphen = HorzDiscretionary(penalty_soft_hyphen, None, Some(PHFixedString(font0, InternalText.of_utf_8 "-")), None) in
    let soft_hyphen1 = HorzDiscretionary(penalty_soft_hyphen, None, Some(PHFixedString(font1, InternalText.of_utf_8 "-")), None) in
    let rec repeat n lst = if n <= 0 then [] else lst @ (repeat (n - 1) lst) in
    let vblst =
      [
        VertParagraph(~% 24., [
          fill; wordL "Sample"; spaceL; wordL "Text"; fill;
        ]);
        VertFixedBreakable(paragraph_skip);
        VertParagraph(~% 24., [
          framed [fill; wordL "Sample"; spaceL; wordL "Text"; fill;];
        ]);
        VertFixedBreakable(paragraph_skip);
        VertParagraph(leading, [
          word "discre"; soft_hyphen; word "tionary"; space; word "hyphen"; space;
          word "discre"; soft_hyphen; word "tionary"; space; word "hyphen"; space;
          word "discre"; soft_hyphen; word "tionary"; space; word "hyphen"; space;
          word "The"; space; word "quick"; space; word "brown"; space; word "fox"; space;
          word "jumps"; space; word "over"; space; word "the"; space; word "lazy"; space; word "dog.";
          space;
          word "My"; space; word "quiz"; space; word "above"; space; word "the"; space; word "kiwi"; space; word "juice"; space;
          word "needs"; space; word "price"; soft_hyphen ; word "less"; space; word "fixing."; space;
          word "fluffy"; space; word "soufflés"; space; word "office"; space; word "Té"; fill;
        ]);

        VertFixedBreakable(paragraph_skip);
        VertParagraph(leading, [
          word "Now"; space; word "we"; space; word "deal"; space; word "with"; space;
          framed [word1 "kerning"; space; word1 "pair";]; space; word "information!"; fill;
        ]);

        VertFixedBreakable(paragraph_skip);
        VertParagraph(leading, [
(*
          wordK "スペーシングの上"; space; wordK "行分割"; space; wordK "されてるけど，"; space;
          wordK "これでも"; space; wordK "和文フォントが"; space; wordK "埋め込まれた"; space;
          wordK "立派な"; space; wordK "PDF"; space; wordK "です。"; space;
          wordK "←"; space; wordK "しかし"; space; wordK "見ての通り"; space;
          wordK "メトリック情報の"; space; wordK "埋め込みに"; space; wordK "関しては"; space; wordK "不完全。";
          space;
          word1 "A"; space1;
*)
          framed [
            word1 "My"; space1; word1 "quiz"; space1; word1 "above"; space1; word1 "the"; space1; framed [word1 "kiwi"; space1; word1 "juice";];]; space1;
            word1 "needs"; space1; word1 "price"; soft_hyphen1 ; word1 "less"; space1; word1 "fixing.";
          fill;
        ]);

        VertFixedBreakable(paragraph_skip);
        VertParagraph(leading, [
(*
          wordK "スペーシングの上"; space; wordK "行分割"; space; wordK "されてるけど，"; space;
          wordK "これでも"; space; wordK "和文フォントが"; space; wordK "埋め込まれた"; space;
          wordK "立派な"; space; wordK "PDF"; space; wordK "です。"; space;
          wordK "←"; space; wordK "しかし"; space; wordK "見ての通り"; space;
          wordK "メトリック情報の"; space; wordK "埋め込みに"; space; wordK "関しては"; space; wordK "不完全。";
*)
          word1 "A"; space1;
          iframed [
            word1 "My"; space1; word1 "quiz"; space1; word1 "above"; space1; word1 "the"; space1; iframed [word1 "kiwi"; space1; word1 "juice";];]; space1;
            word1 "needs"; space1; word1 "price"; soft_hyphen1 ; word1 "less"; space1; word1 "fixing.";
          fill;
        ]);
(*
        VertFixedBreakable(paragraph_skip);
        VertParagraph(~% 20., [
          wordK "スペーシングの上"; space; wordK "行分割"; space; wordK "されてるけど，"; space;
          wordK "これでも"; space; wordK "和文フォントが"; space; wordK "埋め込まれた"; space;
          wordK "立派な"; space; wordK "PDF"; space; wordK "です。"; space;
          wordK "←"; space; wordK "しかし"; space; wordK "見ての通り"; space;
          wordK "メトリック情報の"; space; wordK "埋め込みに"; space; wordK "関しては"; space; wordK "不完全。";
          fframed (~% 300.) [
            word1 "My"; space1; word1 "quiz"; space1; word1 "above"; space1; word1 "the"; space1; fframed (~% 120.) [word1 "kiwi"; space1; word1 "juice";];]; space1;
            word1 "needs"; space1; word1 "price"; soft_hyphen1 ; word1 "less"; space1; word1 "fixing.";
          fill;
        ]);
*)
        VertFixedBreakable(paragraph_skip);
        VertParagraph(leading, [
          indentation;
          bframed [
            word1 "Lorem"; space1; word1 "ipsum"; space1; word1 "dolor"; space1; word1 "sit"; space1; word1 "amet,"; space1;
            word1 "consectetur"; space1; word1 "adipiscing"; space1; word1 "elit,"; space1;
            word1 "sed"; space1; word1 "do"; space1; word1 "eiusmod"; space1; word1 "tempor"; space1; word1 "incididunt"; space1;
            word1 "ut"; space1; word1 "labore"; space1; word1 "et"; space1; word1 "dolore"; space1; word1 "magna"; space1; word1 "aliqua."; space1;
            bframed [
              word1 "Ut"; space1; word1 "enim"; space1; word1 "ad"; space1; word1 "minim"; space1; word1 "veniam,";
            ]; space1;
            word1 "quis"; space1; word1 "nostrud"; space1; word1 "exercitation"; space1; word1 "ullamco"; space1;
            word1 "laboris"; space1; word1 "nisi"; space1; word1 "ut"; space1; word1 "aliquip"; space1;
            word1 "ex"; space1; word1 "ea"; space1; word1 "commodo"; space1; word1 "consequat.";

          ]; fill;
        ]);

      ] @ repeat 2 [
        VertFixedBreakable(paragraph_skip);
        VertParagraph(leading, [
          indentation;
          word1 "Lorem"; space; word1 "ipsum"; space; word "dolor"; space; word "sit"; space; word "amet,"; space;
          word "consectetur"; space; word "adipiscing"; space; word "elit,"; space;
          word "sed"; space; word "do"; space; word "eiusmod"; space; word "tempor"; space; word "incididunt"; space;
          word "ut"; space; word "labore"; space; word "et"; space; word "dolore"; space; word "magna"; space; word "aliqua."; space;
          word "Ut"; space; word "enim"; space; word "ad"; space; word "minim"; space; word "veniam,"; space;
          word " quis"; space; word "nostrud"; space; word "exercitation"; space; word "ullamco"; space;
          word "laboris"; space; word "nisi"; space; word "ut"; space; word "aliquip"; space;
          word "ex"; space; word "ea"; space; word "commodo"; space; word "consequat."; space;
          word "Duis"; space; word "aute"; space; word "irure"; space; word "dolor"; space;
          word "in"; space; word "reprehenderit"; space; word "in"; space; word "voluptate"; space;
          word "velit"; space; word "esse"; space; word "cillum"; space; word "dolore"; space;
          word "eu"; space; word "fugiat"; space; word "nulla"; space; word "pariatur."; space;
          word "Excepteur"; space; word "sint"; space; word "occaecat"; space; word "cupidatat"; space;
          word "non"; space; word "proident,"; space; word "sunt"; space; word "in"; space; word "culpa"; space;
          word "qui"; space; word "officia"; space; word "deserunt"; space; word "mollit"; space; word "anim"; space;
          word "id"; space; word "est"; space; word "laborum."; fill;
        ]);
      ]
    in
    let pdfscheme = HandlePdf.create_empty_pdf "hello5.pdf" in
    try
      begin
        main pdfscheme vblst ;
      end
    with
    | FontFormat.FontFormatBroken(e) -> Otfm.pp_error Format.std_formatter e
  end
*)
