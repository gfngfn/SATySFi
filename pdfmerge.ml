open Pdfutil

(* We read all the files, read their pages and concatenate them, dealing with
clashing object numbers. We then build a new page tree, and build the output
PDF document, with a new root and trailer dictionary. We then remove any
unreferenced objects, and write to file.

The [names] argument contains the initial filenames, so that a file referenced
several times (and only loaded once, of course) is not inadvertantly renumbered
to form several separate PDFs, which would increase output size. *)

(* Equality on PDF streams *)
let streameq pdf x y =
  let x = Pdf.lookup_obj pdf x 
  and y = Pdf.lookup_obj pdf y in
    Pdf.getstream x;
    Pdf.getstream y;
    match x, y with
    | Pdf.Stream {contents = (dict, Pdf.Got (bytes))},
      Pdf.Stream {contents = (dict', Pdf.Got (bytes'))} ->
        compare (dict, bytes) (dict', bytes')
    | _ -> raise (Pdf.PDFError "streameq")

let remove_duplicate_fonts pdf =
  let streamobjs = ref [] in
    Pdf.objiter
      (fun objnum obj ->
         match obj with Pdf.Stream _ -> streamobjs := objnum::!streamobjs | _ -> ())
      pdf;
    let toprocess =
      keep (fun x -> length x > 1) (collate (streameq pdf) (sort (streameq pdf) !streamobjs))
    in
      let pdfr = ref pdf in
        iter
          (function [] -> assert false | h::t ->
             let changetable = Hashtbl.create 100 in
               iter (fun e -> Hashtbl.add changetable e h) t;
               pdfr := Pdf.renumber changetable !pdfr)
          toprocess;
        pdf.Pdf.root <- !pdfr.Pdf.root;
        pdf.Pdf.objects <- !pdfr.Pdf.objects;
        pdf.Pdf.trailerdict <- !pdfr.Pdf.trailerdict

(* Merge the bookmarks in the pdfs and ranges, adding to the new pdf. changes
is oldpageobjnum, newpageobjnum pairs. *)
let merge_bookmarks changes pdfs ranges pdf =
  try
    let process_mark oldnums changes mark = 
      let pageobjectnumber_of_target = function
        | Pdfdest.NullDestination -> 0
        | Pdfdest.XYZ (t, _, _, _) | Pdfdest.Fit t | Pdfdest.FitH (t, _) | Pdfdest.FitV (t, _)
        | Pdfdest.FitR (t, _, _, _, _) | Pdfdest.FitB t | Pdfdest.FitBH (t, _) | Pdfdest.FitBV (t, _) ->
            match t with
            | Pdfdest.OtherDocPageNumber _ -> 0
            | Pdfdest.PageObject i -> i
      in
        let objnum = pageobjectnumber_of_target mark.Pdfmarks.target in
          (*Printf.printf "Considering objnum %i for inclusion...\n" objnum;*)
          if mem objnum oldnums || mark.Pdfmarks.target = Pdfdest.NullDestination (* If this bookmark is to be included... *)
            then
              let change_target_destinationpage target n =
                let change_targetpage = function
                  | Pdfdest.OtherDocPageNumber a -> Pdfdest.OtherDocPageNumber a
                  | Pdfdest.PageObject _ -> Pdfdest.PageObject n 
                in
                  match target with
                  | Pdfdest.NullDestination -> Pdfdest.NullDestination
                  | Pdfdest.XYZ (t, a, b, c) -> Pdfdest.XYZ (change_targetpage t, a, b, c)
                  | Pdfdest.Fit t -> Pdfdest.Fit (change_targetpage t)
                  | Pdfdest.FitH (t, a) -> Pdfdest.FitH (change_targetpage t, a)
                  | Pdfdest.FitV (t, a) -> Pdfdest.FitV (change_targetpage t, a)
                  | Pdfdest.FitR (t, a, b, c, d) -> Pdfdest.FitR (change_targetpage t, a, b, c, d)
                  | Pdfdest.FitB t -> Pdfdest.FitB (change_targetpage t)
                  | Pdfdest.FitBH (t, a) -> Pdfdest.FitBH (change_targetpage t, a)
                  | Pdfdest.FitBV (t, a) -> Pdfdest.FitBV (change_targetpage t, a)
              in
                Some
                  {mark with Pdfmarks.target =
                     if mark.Pdfmarks.target = Pdfdest.NullDestination
                       then Pdfdest.NullDestination
                       else change_target_destinationpage mark.Pdfmarks.target (lookup_failnull objnum changes)}
           else
             None
      in
        let bookmarks' =
          let oldnums = ref (fst (split changes))
          and changes = ref changes in
            let call_process_mark marks range =
              let r =
                (* Pass just the oldnums / changes in question. This is a fix
                for when a single file is multiply included without renumbering
                for efficiency. *)
                option_map
                  (process_mark (take !oldnums (length range)) (take !changes (length range)))
                  marks
              in
                (* Remove (length range) things from the beginning of !oldnums
                / !changes.  This allows the function to work properly when a
                single file is included unrenumbered multiple times due to
                being included twice or more in the merge! *)
                oldnums := drop !oldnums (length range);
                changes := drop !changes (length range);
                r
            in
              flatten (map2 call_process_mark (map Pdfmarks.read_bookmarks pdfs) ranges)
        in
          Pdfmarks.add_bookmarks bookmarks' pdf
  with
    e -> Printf.eprintf "failure in merge_bookmarks %s\n" (Printexc.to_string e); pdf

let debug_pagelabels ls =
  iter (Printf.printf "%s\n") (map Pdfpagelabels.string_of_pagelabel ls)

let debug_collection_of_pagelabels =
  iter (fun ls -> debug_pagelabels ls; flprint "\n")

(* Merging /Dests (Named destinations) in the catalog (PDF v1.1 style, rather
 * than in the PDF 1.3 style in the name tree). Since the new /Dests must be an
 * indirect reference, we add the new object to the pdf, returning the new pdf
 * and the reference.
FIXME: merging a v1.1 file with a v1.2 file will result in both sets of dests, confusing the reader...*)
let new_dests pdf pdfs =
  let dests =
    option_map
      (function pdf ->
        let catalog = Pdf.catalog_of_pdf pdf in
          match Pdf.lookup_direct pdf "/Dests" catalog with
          | Some (Pdf.Dictionary d) -> Some d
          | _ -> None)
      pdfs
  in
    if dests = [] then None else
      let new_dests =
        Pdf.Dictionary (flatten dests)
      in
        Some (Pdf.addobj pdf new_dests)

(* Names distinguish PDFs which are actually the same. So we only use the first
of each group of same ones. Then renumber them. and return. *)
let merge_pdfs_renumber names pdfs =  
  (* Can't use setify due to functional values in PDFs, so use a hash table *)
  let h = Hashtbl.create 20 in
    iter2 (Hashtbl.replace h) names pdfs;
    let first_names, first_pdfs =
      let ns = ref [] and ps = ref [] in
        Hashtbl.iter
          (fun name pdf ->
          ns := name::!ns; ps := pdf::!ps) h;
        (!ns, !ps)
      in
        let table = combine first_names (Pdf.renumber_pdfs first_pdfs) in
          map (function k -> lookup_failnull k table) names
      
(* Reading a name tree, flattened. *)
let rec read_name_tree pdf tree =
  let names =
    match Pdf.lookup_direct pdf "/Names" tree with
    | Some (Pdf.Array elts) ->
        if odd (length elts)
          then raise (Pdf.PDFError "Bad /Names")
          else pairs_of_list elts
    | _ -> []
  in
    match Pdf.lookup_direct pdf "/Kids" tree with
    | Some (Pdf.Array kids) ->
        names @ flatten (map (read_name_tree pdf) kids)
    | _ -> names

(* Build a name tree from a flattened list, FIXME: Inefficient: we should build
a proper tree. *)
let build_name_tree _ ls =
  let ls = sort (fun (k, _) (k', _) -> compare k k') ls in
    let list_of_pair (k, v) = [k; v] in
      let arr = flatten (map list_of_pair ls) in
        Pdf.Dictionary ["/Names", Pdf.Array arr]

(* Merge name trees *)
let merge_name_trees pdf trees =
  build_name_tree pdf (flatten (map (read_name_tree pdf) trees))

(* Merging entries in the Name Dictionary *)
let merge_namedicts pdf pdfs =
  let names =
    ["/Dests"; "/AP"; "/JavaScript"; "/Pages"; "/Templates"; "/IDS";
     "/URLS"; "/EmbeddedFiles"; "/AlternatePresentations"; "/Renditions"]
  in
    let gettree name pdf =
      let catalog = Pdf.catalog_of_pdf pdf in
        match Pdf.lookup_direct pdf "/Names" catalog with
        | Some d -> Pdf.lookup_direct pdf name d
        | None -> None
    in
    (* Build a list of the lists of number trees for each name *)
    let trees_in_each =
      map (fun name -> option_map (gettree name) pdfs) names
    in
      (* Combine with the names, and lose nulls *)
      let with_names =
        lose
          (function (_, []) -> true | _ -> false)
          (combine names trees_in_each)
      in
        let new_trees =
          map
            (fun (name, trees) -> name, merge_name_trees pdf trees)
            with_names
        in
          (* Add all the trees as indirect references to the pdf *)
          let nums = ref [] in
            iter
              (function (_, obj) ->
                let num = Pdf.addobj pdf obj in
                  nums =| num)
              new_trees;
          (* Build the new name dictionary *)
          let newdict =
            Pdf.Dictionary
              (map2
                (fun (name, _) n -> name, Pdf.Indirect n)
                new_trees
                (rev !nums))
          in
            (* Return the new pdf, and the new dictionary. *)
            Pdf.addobj pdf newdict

(* Merge catalog items from the PDFs, taking an abitrary instance of any one we
 * find. Items we know how to merge properly, like /Dests, /Names, /PageLabels,
 * /Outlines, will be overwritten, so we don't worry about them here. *)
let catalog_items_from_original_documents pdfs =
  let catalog_entries =
    flatten
      (map
        (fun pdf ->
           match Pdf.catalog_of_pdf pdf with
             Pdf.Dictionary es -> es
           | _ -> failwith "catalog_items_from_original_documents")
        pdfs)
  in
    fold_left (fun d (k, v) -> add k v d) [] catalog_entries

let merge_pdfs retain_numbering do_remove_duplicate_fonts names pdfs ranges =
  let pdfs = merge_pdfs_renumber names pdfs in
    let minor' = fold_left max 0 (map (fun p -> p.Pdf.minor) pdfs) in
      let pagelists = map Pdfpage.pages_of_pagetree pdfs
      in let pdf = Pdf.empty () in
        let select_pages range pagelist =
          let pages = ref [] in
            iter (fun n -> pages =| select n pagelist) range;
            rev !pages
        in
  let pages = flatten (map2 select_pages ranges pagelists) in
    iter (Pdf.objiter (fun k v -> ignore (Pdf.addobj_given_num pdf (k, v)))) pdfs;
    let pdf, pagetree_num = Pdfpage.add_pagetree pages pdf in
      let page_labels =
        if retain_numbering
         then Pdfpagelabels.merge_pagelabels pdfs ranges
         else []
      in
        let dests = new_dests pdf pdfs in
          let namedict = merge_namedicts pdf pdfs in
            let extra_catalog_entries =
              let with_names =
                (add  "/Names" (Pdf.Indirect namedict)
                  (catalog_items_from_original_documents pdfs))
              in
                match dests with
                  None -> with_names
                | Some dests ->
                    add "/Dests" (Pdf.Indirect dests) with_names
            in
   let pdf = Pdfpage.add_root pagetree_num extra_catalog_entries pdf in
      (* To sort out annotations etc. *)
      let old_page_numbers =
        let select_page_numbers range pageobjnums =
          let pages = ref [] in
            iter (fun n -> pages =| select n pageobjnums) range;
            rev !pages
        in
          flatten (map2 select_page_numbers ranges (map Pdf.page_reference_numbers pdfs))
      in let new_page_numbers =
        Pdf.page_reference_numbers pdf
      in
        let changes = combine old_page_numbers new_page_numbers in
          Pdf.objselfmap
          (Pdf.renumber_object_parsed pdf (hashtable_of_dictionary changes))
          pdf;
   let pdf = {pdf with Pdf.major = 1; Pdf.minor = minor'} in
     let pdf = merge_bookmarks changes pdfs ranges pdf in
       Pdfpagelabels.write pdf page_labels;
       if do_remove_duplicate_fonts then remove_duplicate_fonts pdf;
       pdf

