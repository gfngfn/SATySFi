(* Page Labels. 1-based. All functions assume input lists of page labels are
well-formed. *)
open Pdfutil

type labelstyle =
  | DecimalArabic
  | UppercaseRoman
  | LowercaseRoman
  | UppercaseLetters
  | LowercaseLetters
  | NoLabelPrefixOnly

type t =
  {labelstyle : labelstyle;
   labelprefix : string option;
   startpage : int;
   startvalue : int} 

(* For debug only... *)
let string_of_labelstyle = function
  | DecimalArabic -> "DecimalArabic"
  | UppercaseRoman -> "UppercaseRoman"
  | LowercaseRoman -> "LowercaseRoman"
  | UppercaseLetters -> "UppercaseLetters"
  | LowercaseLetters -> "LowercaseLetters"
  | NoLabelPrefixOnly -> "NoLabelPrefixOnly"

let string_of_pagelabel l =
  (Printf.sprintf "labelstyle: %s\n" (string_of_labelstyle l.labelstyle)) ^
  (Printf.sprintf "labelprefix: %s\n"
     (match l.labelprefix with None -> "None" | Some s -> s)) ^
  (Printf.sprintf "startpage: %i\n" l.startpage) ^
  (Printf.sprintf "startvalue: %s\n" (string_of_int l.startvalue))

let label_of_range pdf (startpage, thing) =
  let startpage =
    match startpage with
    | Pdf.Integer i -> i + 1
    | _ -> raise (Pdf.PDFError "Bad Number Tree")
  and labelstyle =
    match Pdf.lookup_direct pdf "/S" thing with
    | Some (Pdf.Name "/D") -> DecimalArabic
    | Some (Pdf.Name "/R") -> UppercaseRoman
    | Some (Pdf.Name "/r") -> LowercaseRoman
    | Some (Pdf.Name "/A") -> UppercaseLetters
    | Some (Pdf.Name "/a") -> LowercaseLetters
    | _ -> NoLabelPrefixOnly
  and labelprefix =
    match Pdf.lookup_direct pdf "/P" thing with
    | Some (Pdf.String s) -> Some s
    | _ -> None
  and startvalue =
    match Pdf.lookup_direct pdf "/St" thing with
    | Some (Pdf.Integer i) -> i
    | _ -> 1
  in
    {labelstyle = labelstyle;
     labelprefix = labelprefix;
     startpage = startpage;
     startvalue = startvalue}

let read pdf =
  match
    Pdf.lookup_direct pdf "/PageLabels" (Pdf.lookup_obj pdf pdf.Pdf.root)
  with
  | None -> []
  | Some labeltree ->
      let labelranges = Pdf.contents_of_nametree pdf labeltree in
        map (label_of_range pdf) labelranges

(** Add a label, rearranging existing labels. *)
let add_label endpage ls l e =
  let beforeorduringorequal, after =
    List.partition (function x -> x.startpage <= e) ls
  in
    let beforeorduring =
      lose (function x -> x.startpage = l.startpage) beforeorduringorequal
    in
      let replica =
        match after with
        | _ when e = endpage -> []
        | x::_ when x.startpage = e + 1 -> []
        | _ ->
            match beforeorduringorequal with [] -> [] | _ ->
              let lst = last beforeorduringorequal in
                [{lst with
                    startpage = e + 1;
                    startvalue = e + 1 + (lst.startvalue - lst.startpage)}]
      and before =
        lose
          (function x -> x.startpage > l.startpage && x.startpage <= e)
          beforeorduring
      in
        before @ [l] @ replica @ after

let basic =
  {labelstyle = DecimalArabic;
   labelprefix = None;
   startpage = 1;
   startvalue = 1}

(** Make a complete set, so that each page has a number *)
let complete = function
  | [] -> [basic]
  | x::xs when x.startpage > 1 -> basic::x::xs
  | ls -> ls

let letter_string n =
  implode (many (char_of_int ((n - 1) mod 26 + 65)) (((n - 1) / 26) + 1))

(* Make a page label string *)
let string_of_pagenumber n = function
  | NoLabelPrefixOnly -> ""
  | DecimalArabic -> string_of_int n
  | UppercaseRoman -> roman_upper n
  | LowercaseRoman -> roman_lower n
  | UppercaseLetters -> letter_string n
  | LowercaseLetters -> String.lowercase (letter_string n)

let pagelabeltext_of_single n l =
  let realnumber =
    n - (l.startpage - l.startvalue)
  in
    begin match l.labelprefix with None -> "" | Some s -> s end ^
    string_of_pagenumber realnumber l.labelstyle

let rec pagelabeltext_of_pagenumber n = function
  | [] -> raise Not_found
  | [x] -> pagelabeltext_of_single n x
  | x::y::_ when n < y.startpage -> pagelabeltext_of_single n x
  | _::r -> pagelabeltext_of_pagenumber n r

(* Just make a page label for a single one *)
let pagelabel_of_single n l =
  let realnumber =
    n - (l.startpage - l.startvalue)
  in
    {l with startpage = realnumber; startvalue = realnumber}

let rec pagelabel_of_pagenumber n = function
  | [] -> raise Not_found
  | [x] -> pagelabel_of_single n x
  | x::y::_ when n < y.startpage -> pagelabel_of_single n x
  | _::r -> pagelabel_of_pagenumber n r

let rec coalesce prev = function
  | [] -> rev prev
  | [x] -> rev (x::prev)
  | x::y::r
      when x.labelstyle = y.labelstyle &&
           x.labelprefix = y.labelprefix &&
           y.startpage - y.startvalue = x.startpage - x.startvalue ->
      coalesce prev (x::r)
  | x::r -> coalesce (x::prev) r

let coalesce ls = coalesce [] ls

(* Merging page labels. Requires that pdfs, ranges non-empty and same length. *)
let merge_pagelabels pdfs ranges =
  let completed = map complete (map read pdfs) in
    let new_labels =
      map2
        (fun labels range ->
           map (fun p -> pagelabel_of_pagenumber p labels) range)
        completed
        ranges
    in
      let change_labels ls =
        map2
          (fun l sp -> {l with startpage = sp})
          ls
          (indx (flatten ranges))
      in
        coalesce (change_labels (flatten new_labels))

(* For now, just a flat number tree. Doesn't check ranges are well-formed (i.e
contiguous / nonoverlapping) *)
let write pdf labels =
  if labels <> [] then
    let arr =
      flatten
        (map
          (function label ->
             [Pdf.Integer (label.startpage - 1);
              Pdf.Dictionary
                ((match label.labelstyle with
                 | NoLabelPrefixOnly -> []
                 | DecimalArabic -> [("/S", Pdf.Name "/D")]
                 | UppercaseRoman -> [("/S", Pdf.Name "/R")]
                 | LowercaseRoman -> [("/S", Pdf.Name "/r")]
                 | UppercaseLetters -> [("/S", Pdf.Name "/A")]
                 | LowercaseLetters -> [("/S", Pdf.Name "/a")])
                @
                (match label.labelprefix with
                 | None -> []
                 | Some p -> [("/P", Pdf.String p)])
                @
                (match label.startvalue with
                 | 1 -> []
                 | s -> [("/St", Pdf.Integer s)]))])
          labels)
    in
      let root = Pdf.lookup_obj pdf pdf.Pdf.root in
        let rootnum =
          Pdf.addobj pdf
            (Pdf.add_dict_entry
              root "/PageLabels" (Pdf.Dictionary ["/Nums", Pdf.Array arr]))
        in
          pdf.Pdf.root <- rootnum;
          pdf.Pdf.trailerdict <-
            Pdf.add_dict_entry
              pdf.Pdf.trailerdict "/Root" (Pdf.Indirect rootnum)

let remove pdf =
  let root = Pdf.lookup_obj pdf pdf.Pdf.root in
    let rootnum =
      Pdf.addobj pdf (Pdf.remove_dict_entry root "/PageLabels")
    in
      pdf.Pdf.root <- rootnum;
      pdf.Pdf.trailerdict <-
        Pdf.add_dict_entry pdf.Pdf.trailerdict "/Root" (Pdf.Indirect rootnum)
  
