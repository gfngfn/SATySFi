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
*)

let page_height = SkipLength.of_pdf_point 650.  (* temporary; should be variable *)

let main (pdfscheme : HandlePdf.t) (vblst : vert_box list) =

  let calculate_badness_of_page_break hgttotal =
    let hgtdiff = page_height -% hgttotal in
      if hgtdiff <% SkipLength.zero then 10000 else
        int_of_float (hgtdiff /% (SkipLength.of_pdf_point 0.1))
  in

  let chop_single_page imvbacc =
    let rec aux (vpbprev : pure_badness) (imvbacc : intermediate_vert_box list) (imvbaccbreakable : intermediate_vert_box list) (hgttotal : SkipLength.t) (imvblst : intermediate_vert_box list) =
      match imvblst with
      | (ImVertLine(hgt, dpt, _) as head) :: tail ->
          let hgttotalnew = hgttotal +% hgt +% dpt in
          let vpb = calculate_badness_of_page_break hgttotalnew in
          if vpb > vpbprev then
            let () = print_for_debug ("CL " ^ (SkipLength.show hgttotal) ^ " ===> " ^ (SkipLength.show hgttotalnew) ^ "\n") in  (* for debug *)
              Some((List.rev imvbacc, List.rev imvblst))  (* -- discard breakables -- *)
          else
            aux vpb (head :: (List.append imvbaccbreakable imvbacc)) [] hgttotalnew tail

      | (ImVertFixedBreakable(vskip) as head) :: tail ->
          let hgttotalnew = hgttotal +% vskip in
          let vpb = calculate_badness_of_page_break hgttotalnew in
          if vpb > vpbprev then
            let () = print_for_debug ("CB " ^ (SkipLength.show hgttotal) ^ " ===> " ^ (SkipLength.show hgttotalnew) ^ "\n") in  (* for debug *)
              Some((List.rev imvbacc, List.rev imvblst))  (* -- discard breakables -- *)
          else
            aux vpb imvbacc (head :: imvbaccbreakable) hgttotalnew tail

      | [] ->
          let () = print_for_debug ("CE " ^ (SkipLength.show hgttotal) ^ " ===> None\n") in  (* for debug *)
            None
    in
    let imvblst = List.rev imvbacc in
      aux 100000 [] [] SkipLength.zero imvblst
  in

  let determine_heights (imvblst : intermediate_vert_box list) =
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
        | None -> begin HandlePdf.write_to_file pdfschemenext ; end
        | Some((imvbaccnext, vblstnext)) ->
            iteration pdfschemenext imvbaccnext vblstnext
  in
    iteration pdfscheme [] vblst


let penalty_break_space = 100
let penalty_soft_hyphen = 1000


let () =
  let ( ~% ) = SkipLength.of_pdf_point in
  begin
    FontInfo.initialize () ;
    let font0 = ("Arno", ~% 16.) in
    let font1 = ("Hlv", ~% 16.) in
    let fontL = ("Hlv", ~% 32.) in

    let fontK = ("KozMin", ~% 12.) in

    let word s = HorzPure(PHFixedString(font0, InternalText.of_utf_8 s)) in
    let word1 s = HorzPure(PHFixedString(font1, InternalText.of_utf_8 s)) in
    let wordL s = HorzPure(PHFixedString(fontL, InternalText.of_utf_8 s)) in

    let wordK s = HorzPure(PHFixedString(fontK, InternalText.of_utf_8 s)) in

    let pads = { paddingL = ~% 2.; paddingR = ~% 2.; paddingT= ~% 2.; paddingB = ~% 2.} in
    let framed hblst = HorzPure(PHOuterFrame(pads, hblst)) in
    let iframed hblst = HorzPure(PHInnerFrame(pads, hblst)) in
    let fframed wid hblst = HorzPure(PHFixedFrame(pads, wid, hblst)) in
    let bframed hblst = HorzFrameBreakable(pads, ~% 5., ~% 5., hblst) in
    let space = HorzDiscretionary(penalty_break_space, Some(PHOuterEmpty(~% 6., ~% 1., ~% 3.)), None, None) in
    let space1 = HorzDiscretionary(penalty_break_space, Some(PHOuterEmpty(~% 8., ~% 1., ~% 3.)), None, None) in
    let spaceL = HorzDiscretionary(penalty_break_space, Some(PHOuterEmpty(~% 16., ~% 2., ~% 6.)), None, None) in
    let indentation = HorzPure(PHFixedEmpty(~% 64.)) in
    let fill = HorzPure(PHOuterFil) in
    let paragraph_skip = ~% 32.0 in
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
        VertParagraph(~% 24., [
          word "discre"; soft_hyphen; word "tionary"; space; word "hyphen"; space;
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
          word "Now"; space; word "we"; space; word "deal"; space; word "with"; space;
          framed [word1 "kerning"; space; word1 "pair";]; space; word "information!"; fill;
        ]);

        VertFixedBreakable(paragraph_skip);
        VertParagraph(~% 20., [
          wordK "スペーシングの上"; space; wordK "行分割"; space; wordK "されてるけど，"; space;
          wordK "これでも"; space; wordK "和文フォントが"; space; wordK "埋め込まれた"; space;
          wordK "立派な"; space; wordK "PDF"; space; wordK "です。"; space;
          wordK "←"; space; wordK "しかし"; space; wordK "見ての通り"; space;
          wordK "メトリック情報の"; space; wordK "埋め込みに"; space; wordK "関しては"; space; wordK "不完全。";
          framed [
            word1 "My"; space1; word1 "quiz"; space1; word1 "above"; space1; word1 "the"; space1; framed[word1 "kiwi"; space1; word1 "juice";];]; space1;
            word1 "needs"; space1; word1 "price"; soft_hyphen1 ; word1 "less"; space1; word1 "fixing.";
          fill;
        ]);

        VertFixedBreakable(paragraph_skip);
        VertParagraph(~% 20., [
          wordK "スペーシングの上"; space; wordK "行分割"; space; wordK "されてるけど，"; space;
          wordK "これでも"; space; wordK "和文フォントが"; space; wordK "埋め込まれた"; space;
          wordK "立派な"; space; wordK "PDF"; space; wordK "です。"; space;
          wordK "←"; space; wordK "しかし"; space; wordK "見ての通り"; space;
          wordK "メトリック情報の"; space; wordK "埋め込みに"; space; wordK "関しては"; space; wordK "不完全。";
          iframed [
            word1 "My"; space1; word1 "quiz"; space1; word1 "above"; space1; word1 "the"; space1; iframed[word1 "kiwi"; space1; word1 "juice";];]; space1;
            word1 "needs"; space1; word1 "price"; soft_hyphen1 ; word1 "less"; space1; word1 "fixing.";
          fill;
        ]);

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

        VertFixedBreakable(paragraph_skip);
        VertParagraph(~% 24., [
          indentation;
          bframed [
            word1 "Lorem"; space; word1 "ipsum"; space; word "dolor"; space; word "sit"; space; word "amet,"; space;
            word "consectetur"; space; word "adipiscing"; space; word "elit,"; space;
            word "sed"; space; word "do"; space; word "eiusmod"; space; word "tempor"; space; word "incididunt"; space;
            word "ut"; space; word "labore"; space; word "et"; space; word "dolore"; space; word "magna"; space; word "aliqua.";
            word "Ut"; space; word "enim"; space; word "ad"; space; word "minim"; space; word "veniam,"; space;
            word " quis"; space; word "nostrud"; space; word "exercitation"; space; word "ullamco"; space;
            word "laboris"; space; word "nisi"; space; word "ut"; space; word "aliquip"; space;
            word "ex"; space; word "ea"; space; word "commodo"; space; word "consequat."; space;
          ]; fill;
        ]);

      ] @ repeat 8 [
        VertFixedBreakable(paragraph_skip);
        VertParagraph(~% 24., [
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
