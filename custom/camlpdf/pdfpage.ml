open Pdfutil

(* The type of the four rotations of pages. This defines how a viewing
application (e.g Acrobat) displays the page. *)
type rotation =
  | Rotate0
  | Rotate90
  | Rotate180
  | Rotate270

(* A type representing a page. content is the list of objects
containing the graphical content stream (see the Pdfops module), mediabox
the page size, resources the page's resource dictionary, rotate its rotation
and rest any other entries to reside in the page dictionary. *)
type t =
  {content : Pdf.pdfobject list; (*FIXME Change this to int list, to ensure sharing? *)
   mediabox : Pdf.pdfobject;
   resources : Pdf.pdfobject;
   rotate : rotation;
   rest : Pdf.pdfobject} (* A dictionary of the other records in the page. *)

(* Make a PDF rectangle from a Paper.papersize. *)
let rectangle_of_paper paper =
  let u = Pdfpaper.unit paper
  in let w = Pdfpaper.width paper
  in let h = Pdfpaper.height paper in
    let w', h' =
      let f = Pdfunits.convert 100. u Pdfunits.PdfPoint in
        f w, f h
    in
      Pdf.Array [Pdf.Real 0.; Pdf.Real 0.; Pdf.Real w'; Pdf.Real h']

(* Create a page with empty content, media box from the given paper size,
empty resources, zero rotation and no extra dictionary entries. *)
let custompage rectangle =
  {content = [];
   mediabox = rectangle;
   resources = Pdf.Dictionary [];
   rotate = Rotate0;
   rest = Pdf.Dictionary []}

let blankpage papersize =
  custompage (rectangle_of_paper papersize)

(* Utility function to convert from rotation to integers. *)
let int_of_rotation = function
  | Rotate0 -> 0
  | Rotate90 -> 90
  | Rotate180 -> 180
  | Rotate270 -> 270

(* The reverse. raises [Pdf.PDFError] if its input modulo 360 is not 0, 90, 
180, 270, -90, -180 or -270. *)
let rotation_of_int i =
  match i mod 360 with
  | 0 -> Rotate0
  | 90 | -270 -> Rotate90
  | 180 | -180 -> Rotate180
  | 270 | -90 -> Rotate270
  | _ -> raise (Pdf.PDFError "Bad /Rotate")

(* Extracting the page tree *)

let rec remove_dict_entries e = function
  | (("/Resources" | "/Contents" | "/MediaBox" | "/Rotate" | "/Parent" | "/Type"), v)::t -> remove_dict_entries e t
  | h::t -> remove_dict_entries (h::e) t
  | [] -> e

(* Given a page tree, find the first page resources, contents and
mediabox.  The resources and mediabox may be inherited from any node above in
the page tree. *)
let rec find_pages pages pdf resources mediabox rotate =
  match Pdf.lookup_direct pdf "/Type" pages with
  | Some (Pdf.Name "/Pages") | None ->
      begin match
        Pdf.lookup_fail "No /Kids in page tree" pdf "/Kids" pages
      with
      | Pdf.Array kids ->
          let kids =
            map
              (function
               | Pdf.Indirect k ->
                   (try Pdf.lookup_obj pdf k with
                     Not_found -> raise (Pdf.PDFError "missing kid\n"))
               | _ -> raise (Pdf.PDFError "malformed kid\n"))
              kids
          in
            let resources =
              match Pdf.lookup_direct pdf "/Resources" pages with
              | Some x -> Some x
              | None -> resources
            in let mediabox =
              match Pdf.lookup_direct pdf "/MediaBox" pages with
              | Some x -> Some x
              | None -> mediabox
            in let rotate =
              match Pdf.lookup_direct pdf "/Rotate" pages with
              | Some (Pdf.Integer r) -> rotation_of_int r
              | _ -> rotate
            in
              flatten
                (map
                  (fun k -> find_pages k pdf resources mediabox rotate)
                  kids)
      | _ -> raise (Pdf.PDFError "Malformed /Kids in page tree node")
      end
  | Some (Pdf.Name "/Page") ->
      let resources =
        match Pdf.lookup_direct pdf "/Resources" pages with
        | Some x -> Some x
        | None -> resources
      in let mediabox =
        match Pdf.lookup_direct pdf "/MediaBox" pages with
        | Some x -> Some x
        | None -> mediabox
      in let contents =
        (* 28th March 2016. We modify this to always create an array of one
        indirect if just a single contents stream, rather than following
        through to the actual object. Other code can then preserve the
        sharing. *)
        begin match pages with
          Pdf.Dictionary d ->
            begin match lookup "/Contents" d with
              Some (Pdf.Indirect i) ->
                (* A single content stream, or indirect to array *)
                begin match Pdf.lookup_obj pdf i with
                | Pdf.Array a -> Some (Pdf.Array a)
                | _ -> Some (Pdf.Array [Pdf.Indirect i])
                end
            | _ ->
                (* An array of indirects. Just return it *)
                Pdf.lookup_direct pdf "/Contents" pages
            end
        | _ -> assert false
        end
      in let rotate =
        match Pdf.lookup_direct pdf "/Rotate" pages with
        | Some (Pdf.Integer r) -> rotation_of_int r
        | _ -> rotate
      in
        [{resources =
            (match resources with
            | Some r -> r
            | None -> Pdf.Dictionary []);
          content =
            (* 15th Feb 2012. We now preserve indirect references in /Contents
            to preserve sharing, at least on operations which don't modify the
            /Contents. *)
            (match contents with
            | None -> []
            | Some (Pdf.Array cs) ->
                map
                  (function x ->
                     match Pdf.direct pdf x with
                     | Pdf.Stream _ -> x
                     | x ->
                         raise (Pdf.PDFError ("Bad /Contents 1")))
                  cs;
            | Some pdfobject ->
                begin match Pdf.direct pdf pdfobject with
                | Pdf.Stream _ -> [pdfobject]
                | _ -> raise (Pdf.PDFError "Bad /Contents 2")
                end);
          mediabox =
            (match mediabox with
            | Some m -> m
            | None -> raise (Pdf.PDFError "Bad /MediaBox"));
          rotate = rotate;
          rest =
            (match pages with
            | Pdf.Dictionary d -> Pdf.Dictionary (remove_dict_entries [] d)
            | _ -> raise (Pdf.PDFError "Bad /Pages"))
        }]
  | _ -> raise (Pdf.PDFError "find_pages: Not a page tree node or page object")

(* Given a pdf, return a list of (resources, contents, mediabox) triples. *)
let pages_of_pagetree pdf =
  let document_catalog =
    try Pdf.lookup_obj pdf pdf.Pdf.root with
      Not_found -> raise (Pdf.PDFError "/Root entry is incorrect")
  in 
    let pages =
      Pdf.lookup_fail "No or malformed /Pages" pdf "/Pages" document_catalog
    in
      find_pages pages pdf None None Rotate0

let rec find_pages_quick pages pdf =
  match Pdf.lookup_direct pdf "/Type" pages with
  | Some (Pdf.Name "/Pages") | None ->
      begin match
        Pdf.lookup_fail "No /Kids in page tree" pdf "/Kids" pages
      with
      | Pdf.Array kids ->
          let kids =
            map
              (function
               | Pdf.Indirect k ->
                   (try Pdf.lookup_obj pdf k with
                     Not_found -> raise (Pdf.PDFError "missing kid\n"))
               | _ -> raise (Pdf.PDFError "malformed kid\n"))
              kids
          in
            fold_left ( + ) 0 (map (fun k -> find_pages_quick k pdf) kids)
      | _ -> raise (Pdf.PDFError "Malformed /Kids in page tree node")
      end
  | Some (Pdf.Name "/Page") -> 1
  | _ -> raise (Pdf.PDFError "find_pages: Not a page tree node or page object")

let pages_of_pagetree_quick pdf =
  let document_catalog =
    try Pdf.lookup_obj pdf pdf.Pdf.root with
      Not_found -> raise (Pdf.PDFError "/Root entry is incorrect")
  in 
    let pages =
      Pdf.lookup_fail "No or malformed /Pages" pdf "/Pages" document_catalog
    in
      find_pages_quick pages pdf

let endpage pdf =
  pages_of_pagetree_quick pdf

(* Make a collection of pages capable of being merged -- in other words rename
their resources so as not to clash. *)
let source k =
  let k = ref k in (fun () -> incr k; !k)

let freshname source =
  "/r" ^ string_of_int (source ())

let resource_keys =
  ["/Font"; "/ExtGState"; "/ColorSpace";
   "/Pattern"; "/Shading"; "/XObject"; "/Properties"]

let make_changes pdf pages =
  let src = source 0 in
    let entries_of_page entry pageseq page =
      let entries =
        match Pdf.lookup_direct pdf entry page.resources with
        | Some (Pdf.Dictionary es) -> es
        | _ -> []
      in
        map (fun (k, v) -> entry, pageseq, k, freshname src) entries
    in
      let pagenums = ilist 1 (length pages) in
        let entries name =
          map2 (entries_of_page name) pagenums pages
        in
          let entries = flatten <| flatten (map entries resource_keys) in
            let table = Hashtbl.create 1000 in
              iter
                (fun (entry, pageseq, k, name) ->
                   Hashtbl.add table (entry, pageseq, k) name)
                entries;
              table

let change_operator pdf lookup lookup_option seqnum = function
  | Pdfops.Op_Tf (f, s) ->
      Pdfops.Op_Tf (lookup "/Font" seqnum f, s)
  | Pdfops.Op_gs n ->
      Pdfops.Op_gs (lookup "/ExtGState" seqnum n)
  | Pdfops.Op_CS n ->
      begin match lookup_option "/ColorSpace" seqnum n with
      | Some x -> Pdfops.Op_CS x
      | None -> Pdfops.Op_CS n
      end
  | Pdfops.Op_cs n ->
      begin match lookup_option "/ColorSpace" seqnum n with
      | Some x -> Pdfops.Op_cs x
      | None -> Pdfops.Op_cs n
      end
  | Pdfops.Op_SCNName (s, ns) ->
      Pdfops.Op_SCNName (lookup "/Pattern" seqnum s, ns)
  | Pdfops.Op_scnName (s, ns) ->
      Pdfops.Op_scnName (lookup "/Pattern" seqnum s, ns)
  | Pdfops.Op_sh s ->
      Pdfops.Op_sh (lookup "/Shading" seqnum s)
  | Pdfops.Op_Do x ->
      Pdfops.Op_Do (lookup "/XObject" seqnum x)
  | Pdfops.Op_DP (n, Pdf.Name p) ->
      Pdfops.Op_DP (n, Pdf.Name (lookup "/Properties" seqnum p))
  | Pdfops.Op_BDC (n, Pdf.Name p) ->
      begin match lookup_option "/Properties" seqnum p with
        | Some x ->
            Pdfops.Op_BDC (n, Pdf.Name x)
        | None ->
            Printf.eprintf "Warning: Missing Op_BDC /Properties entry\n";
            Pdfops.Op_BDC (n, Pdf.Name p)
      end
  | Pdfops.InlineImage (dict, bytes) ->
      (* Replace any indirect "/CS" or "/ColorSpace" with a new "/CS" *)
      let dict' =
        match Pdf.lookup_direct_orelse pdf "/CS" "/ColorSpace" dict with
        | Some (Pdf.Name "/DeviceGray")
        | Some (Pdf.Name "/DeviceRGB")
        | Some (Pdf.Name "/DeviceCMYK")
        | Some (Pdf.Name "/G")
        | Some (Pdf.Name "/RGB")
        | Some (Pdf.Name "/CMYK") -> dict
        | Some (Pdf.Name n) ->
            Pdf.add_dict_entry
              (Pdf.remove_dict_entry
                (Pdf.remove_dict_entry dict "/ColorSpace")
                "/CS")
              "/CS"
              (Pdf.Name (lookup "/ColorSpace" seqnum n))
        | _ -> dict
      in
        Pdfops.InlineImage (dict', bytes)
  | x -> x

(* Only for use with twoup now. FIXME: Can blow up shared content streams. Needs
a cunning new method to preserve sharing. *)
let renumber_pages pdf pages =
  match pages with
  | [] -> []
  | pages ->
      let changes = make_changes pdf pages in
        let rec lookup_option dictname page oldkey =
          tryfind changes (dictname, page, oldkey)
        and lookup dictname page oldkey =
          try
            Hashtbl.find changes (dictname, page, oldkey)
          with
            Not_found -> raise (Pdf.PDFError "Pdfdoc.renumber_pages: Bad key")
        in
        let change_content seqnum resources content =
          let operators = Pdfops.parse_operators pdf resources content in
            let operators' =
              map (change_operator pdf lookup lookup_option seqnum) operators
            in
              [Pdfops.stream_of_ops operators']
        in let change_resources seqnum resources =
          let newdict name =
            match Pdf.lookup_direct pdf name resources with
            | Some (Pdf.Dictionary fonts) ->
                Pdf.Dictionary (map (fun (k, v) -> lookup name seqnum k, v) fonts)
            | _ -> Pdf.Dictionary []
          in
            let newdicts = map newdict resource_keys in
              let resources = ref resources in
                iter2
                  (fun k v ->
                    resources := Pdf.add_dict_entry !resources k v)
                  resource_keys
                  newdicts;
                !resources
        in
          let process_page seqnum page =
            {page with
               content = change_content seqnum page.resources page.content;
               resources = change_resources seqnum page.resources}
          in
            map2 process_page (indx pages) pages

(* New code for better page trees *)

(* Each branch contains a list of pages to go at that branch, and pointers to
two more page tree nodes.  Each leaf contains just a page list. Page lists must
be non-null.

Leaves and branches also hold a parent pointer, and the object number of that
leaf or branch. *) 
type ptree =
  | Lf of t list * int * int
  | Br of t list * ptree * ptree * int * int

(* Split a list into three parts, the middle being of fixed, given, length n,
and the left and right roughly equal in size, but at least of length one. *)
let split3 n l =
  let len = length l in
    if n > len - 2 then raise (Invalid_argument "split3") else
      let leftlen = (len - n) / 2 in
        let left, rest = cleave l leftlen in
          let middle, right = cleave rest n in
            left, middle, right

(* Build the pages *)
let rec pagetree objnumsource pages parent =
  if length pages < 10 then Lf (pages, parent, objnumsource ()) else
    let left, this, right = split3 5 pages in
      let this_num = objnumsource () in
        let left_tree = pagetree objnumsource left this_num
        in let right_tree = pagetree objnumsource right this_num in
          Br (this, left_tree, right_tree, parent, this_num)

let pagetree_flat objnumsource pages parent =
  Lf (pages, parent, objnumsource ())

(* Version for pdf_of_pages where we are using the same object numbers *)
type ptree_objnumbers =
  | OLf of int list * int * int (* object numbers, parent, object number of this leaf *)
  | OBr of int list * ptree_objnumbers * ptree_objnumbers * int * int (* object numbers, left, right, parent, object number of this branch *)

let rec print_ptree = function
  | OLf (is, parent, objnumleaf) ->
      Printf.printf "OLf with object numbers ";
      print_ints is;
      Printf.printf " parent %i and this leaf object number is %i\n" parent objnumleaf
  | OBr (is, l, r, p, thisobjnumbranch) ->
      Printf.printf "OBt with object numbers ";
      print_ints is;
      Printf.printf " parent %i, this object number is %i\n" p thisobjnumbranch;
      Printf.printf "***LEFTS\n";
      print_ptree l;
      Printf.printf "***RIGHTS\n";
      print_ptree r

let rec pagetree_with_objnumbers toplevel old_pagetree_root_num objnumsource objnumbers parent =
  if length objnumbers < 10 then
    OLf (objnumbers, parent, if toplevel then old_pagetree_root_num else objnumsource ())
  else
    let left, this, right = split3 5 objnumbers
    and this_num = if toplevel then old_pagetree_root_num else objnumsource () in
      let left_tree = pagetree_with_objnumbers false old_pagetree_root_num objnumsource left this_num
      and right_tree = pagetree_with_objnumbers false old_pagetree_root_num objnumsource right this_num in
        OBr (this, left_tree, right_tree, parent, this_num)

(* Make a page. Returns, objectnumber, page pdfobject, extra objects to be added. *)
let mkpage getobjnum parent page =
  (*Printf.printf "mkpage with parent %i\n" parent;*)
  let content, extras =
    match page.content with
    | [] -> [], []  (*r Null Contents not allowed. *)
    | cs ->
       let indirects, objects =
          split
            (map
              (function
                 | Pdf.Indirect i -> Pdf.Indirect i, None
                 | c -> let i = getobjnum () in Pdf.Indirect i, Some (i, c))
              cs)
        in
          [("/Contents", Pdf.Array indirects)], losenones objects 
  in
    let page =
      Pdf.Dictionary
        ([("/Type", Pdf.Name "/Page");
          ("/Parent", Pdf.Indirect parent);
          ("/Resources", page.resources);
          ("/MediaBox", page.mediabox);
          ("/Rotate", Pdf.Integer (int_of_rotation page.rotate))]
      @
        (match page.rest with
         | Pdf.Dictionary d -> d
         | _ -> raise (Pdf.PDFError "mkpage"))
      @ 
        content)
    in
      getobjnum (), page, extras

(* Build a list of objnum, pdfobject pairs from the ptree. The pages in the
ptree are just missing their parent entries, so we add those. *)
(* FIXME: Now we rewrote this for pdf_of_pages with nicer properties, can we get rid of this one? But consider linearization (see below) *)
let rec objects_of_ptree getobjnum extras = function
  | Lf (pages, parent, this) ->
      let page_objects =
        map
         (fun (o, p, x) -> extras =@ x; (o, p))
         (map (mkpage getobjnum this) pages)
      in
        let page_tree_node =
          let pdfobject =
            let parent_entry =
              if parent = 0 then [] else ["/Parent", Pdf.Indirect parent]
            in
              Pdf.Dictionary
                (["/Type", Pdf.Name "/Pages";
                  "/Kids",
                     Pdf.Array (
                       map (fun x -> Pdf.Indirect x) (fst <| split page_objects));
                  "/Count", Pdf.Integer (length pages)]
                 @ parent_entry)
          in
           this, pdfobject 
        in
          page_tree_node::page_objects
  | Br (pages, left, right, parent, this) ->
      let objs_left = objects_of_ptree getobjnum extras left
      in let objs_right = objects_of_ptree getobjnum extras right in
        let left_num =
          match objs_left with
          | (n, _)::_ -> n
          | [] -> assert false
        in let right_num =
          match objs_right with
          | (n, _)::_ -> n
          | [] -> assert false
        in let count_left =
          match objs_left with
          | (_, Pdf.Dictionary d)::_ ->
              begin match lookup "/Count" d with
              | Some (Pdf.Integer i) -> i 
              | _ -> assert false
              end
          | _ -> assert false
        in let count_right =
          match objs_right with
          | (_, Pdf.Dictionary d)::_ ->
              begin match lookup "/Count" d with
              | Some (Pdf.Integer i) -> i 
              | _ -> assert false
              end
          | _ -> assert false
        in
          let this_objects =
            let page_objects =
              map
               (fun (o, p, x) -> extras =@ x; (o, p))
               (map (mkpage getobjnum this) pages)
            in
              let page_tree_node =
                let pdfobject =
                  let parent_entry =
                    if parent = 0 then [] else ["/Parent", Pdf.Indirect parent]
                  in
                    let kids = fst <| split page_objects in
                      Pdf.Dictionary
                        (["/Type", Pdf.Name "/Pages";
                          "/Kids",
                             Pdf.Array
                               (map
                                  (fun x -> Pdf.Indirect x)
                                  ([left_num] @ kids @ [right_num]));
                          "/Count", Pdf.Integer (count_left + count_right + length kids)]
                         @ parent_entry)
                in
                 this, pdfobject 
              in
                page_tree_node::page_objects
           in
             this_objects @ objs_left @ objs_right
     
let flat_pagetrees = ref false

(* Flat version *)
let add_pagetree_flat pages pdf =
  let extras = ref [] in
    let getobjnum = source pdf.Pdf.objects.Pdf.maxobjnum in
      let ptree = pagetree_flat getobjnum pages 0 in
        let objects = objects_of_ptree getobjnum extras ptree in
          let topnode = match hd objects with (n, _) -> n in
            iter (fun x -> ignore (Pdf.addobj_given_num pdf x)) (objects @ !extras);
            pdf, topnode

(* Take a list of pages and a PDF. Build a page tree in the PDF, returning
the new pdf and the object number assigned to the top page node. All references
to objects not forming part of the tree nodes themselves are left unchanged. *)
let add_pagetree pages pdf =
  if !flat_pagetrees then add_pagetree_flat pages pdf else
    let extras = ref [] in
      let getobjnum = source pdf.Pdf.objects.Pdf.maxobjnum in
        let ptree = pagetree getobjnum pages 0 in
          let objects = objects_of_ptree getobjnum extras ptree in
            let topnode = match hd objects with (n, _) -> n in
              (*Printf.printf "There were %i objects_of_ptree\n" (List.length objects);
              List.iter (fun (i, x) -> Printf.printf "%i: %s\n" i (Pdfwrite.string_of_pdf x)) objects;
              Printf.printf "There were %i extras\n" (List.length !extras);
              List.iter (fun (i, x) -> Printf.printf "%i: %s\n" i
              (Pdfwrite.string_of_pdf x)) !extras;*)
              iter (fun x -> ignore (Pdf.addobj_given_num pdf x)) (objects @ !extras);
              pdf, topnode

(* Add a root entry, replacing the Type and Pages entry, and any entries in
extras. Preserves any entries in any existing root (e.g Metadata pointer). *)
let add_root pageroot extras pdf =
  let existing_entries =
    try
      match Pdf.lookup_obj pdf pdf.Pdf.root with
      | Pdf.Dictionary d -> d
      | _ -> []
    with
    _ -> []
  in
    let root =
      Pdf.Dictionary
        (fold_right (* Right so that /Type, /Pages overwrite *)
           (fun (k, v) d -> add k v d)
              ([("/Type", Pdf.Name "/Catalog"); ("/Pages", Pdf.Indirect pageroot)] @ existing_entries)
              extras)
    in
      let rootnum = Pdf.addobj pdf root in
        let trailerdict' =
          match pdf.Pdf.trailerdict with
          | Pdf.Dictionary d -> Pdf.Dictionary (add "/Root" (Pdf.Indirect rootnum) d)
          | _ -> raise (Pdf.PDFError "add_root: bad trailer dictionary")
        in
          {pdf with
             Pdf.root = rootnum;
             Pdf.trailerdict = trailerdict'}

(* Return a new PDF containing everything the old one does, but with new pages.

Other objects (e.g destinations in the document outline) may point to the
individual page objects, so we must renumber these. We can only do this if the
number of pages are the same. We do this [if change_references is true]. If the
new and old page lists are of differenct lengths, change_references must be
false, or you must supply the changes (expressed as (from, to) 1-based serial
number pairs) *)
let change_pages ?changes change_references basepdf pages' =
  let pdf = Pdf.empty () in
    Pdf.objiter (fun k v -> ignore (Pdf.addobj_given_num pdf (k, v))) basepdf;
    let old_page_numbers = Pdf.page_reference_numbers basepdf in
    let pdf, pagetree_num = add_pagetree pages' pdf in
      let pdf =
        {pdf with
           Pdf.major = basepdf.Pdf.major;
           Pdf.minor = basepdf.Pdf.minor;
           Pdf.trailerdict = basepdf.Pdf.trailerdict;
           Pdf.saved_encryption = basepdf.Pdf.saved_encryption}
      in
        let existing_root_entries =
          try
            match Pdf.lookup_obj basepdf basepdf.Pdf.root with
            | Pdf.Dictionary d -> d
            | _ -> []
          with
          _ -> []
        in
          let pdf = add_root pagetree_num existing_root_entries pdf in
            let new_page_numbers = Pdf.page_reference_numbers pdf in
              if not change_references then pdf else
                let changes =
                   match changes with
                     None ->
                       if length old_page_numbers = length new_page_numbers then
                         combine old_page_numbers new_page_numbers
                       else
                         begin
                           Printf.printf "change_pages: No change supplied, and lengths differ\n";
                           []
                         end
                   | Some cs ->
                       (* Turn the 1-based serial numbers into page reference numbers *)
                       try
                         List.map
                           (fun (x, y) ->
                            List.nth old_page_numbers (x - 1), List.nth new_page_numbers (y - 1))
                         cs
                       with
                         _ -> raise (Pdf.PDFError "change_pages: bad serial number")
                in
                  Pdf.objselfmap
                    (Pdf.renumber_object_parsed pdf (hashtable_of_dictionary changes))
                    pdf;
                  pdf

(* Return a pdf with a subset of pages, but nothing else changed - exactly the
same page object numbers, so bookmarks etc still work. Also sorts out bookmarks
so only those in the range are kept. *)

(* Make sure to supply refnums to speed it up, if you already have them from a
 * previous call to Pdf.page_reference_numbers *)
let pagenumber_of_target ?fastrefnums pdf = function
 | Pdfdest.NullDestination -> 0
 | Pdfdest.XYZ (t, _, _, _) | Pdfdest.Fit t | Pdfdest.FitH (t, _) | Pdfdest.FitV (t, _)
 | Pdfdest.FitR (t, _, _, _, _) | Pdfdest.FitB t | Pdfdest.FitBH (t, _) | Pdfdest.FitBV (t, _) ->
     match t with
     | Pdfdest.OtherDocPageNumber _ -> 0
     | Pdfdest.PageObject i ->
         match fastrefnums with
         | Some table ->
             begin try Hashtbl.find table i with Not_found -> 0 end 
         | None ->
             match position_1 i (Pdf.page_reference_numbers pdf) with
             | Some n -> n
             | None -> 0

(* Find a page indirect from the page tree of a document, given a page number. *)
(* FIXME speed up by caching *)
let page_object_number pdf destpage =
  try
    Some (select destpage (Pdf.page_reference_numbers pdf))
  with
    (* The page might not exist in the output *)
    Invalid_argument _ (*"select"*) -> None

let target_of_pagenumber pdf i =
  match page_object_number pdf i with
  | None -> Pdfdest.NullDestination
  | Some p -> Pdfdest.Fit (Pdfdest.PageObject p)

(* Build a pagetree using existing object numbers of exisiting pages, adding
any intermediate nodes to the pdf *)
let buildnode kids parent count =
  Pdf.Dictionary
    ([("/Type", Pdf.Name "/Pages");
     ("/Kids", Pdf.Array (map (function i -> Pdf.Indirect i) kids));
     ("/Count", Pdf.Integer count)]
     @
     (if parent = 0 then [] else [("/Parent", Pdf.Indirect parent)]))

let objnumfrom = function
  | OLf (_, _, o) -> o
  | OBr (_, _, _, _, o) -> o

let rec countof = function
  | OLf (os, _, _) -> length os
  | OBr (os, l, r, _, _) -> countof l + countof r + length os

let rec objects_of_ptree_objnumbers pdf = function
  | OLf (objnumbers, parent, objnumofthisleaf) as node ->
      Pdf.addobj_given_num pdf
        (objnumofthisleaf, buildnode objnumbers parent (countof node))
  | OBr (objnumbers, left, right, parent, objnumofthisbranch) as node ->
      Pdf.addobj_given_num pdf
        (objnumofthisbranch, buildnode ([objnumfrom left] @ objnumbers @ [objnumfrom right]) parent (countof node));
      objects_of_ptree_objnumbers pdf left;
      objects_of_ptree_objnumbers pdf right

let pdf_of_pages_build_pagetree thetree objnumbers pdf =
  match thetree with
  | OLf (objnumbers, parent, _) as node ->
      (* Just a leaf. Don't add it, just build the node *)
      buildnode objnumbers parent (countof node)
  | OBr (objnumbers, left, right, parent, _) as node ->
      (* A branch. Return the top level built node, and call main function on left, right *)
      objects_of_ptree_objnumbers pdf left;
      objects_of_ptree_objnumbers pdf right;
      buildnode ([objnumfrom left] @ objnumbers @ [objnumfrom right]) parent (countof node)

(* pdf_of_pages, if it has duplicates in the range, will produce duplicate
items in the page tree, pointing to the same page object. This is bad for
two reasons:

   a) Adobe Reader is broken and crashes in this case

   b) In any event, duplicate references make further document changes
   confusing for most programs.  So, we duplicate the actual page objects, and
   do the minimal renumbering.

*)

(* Given a number n, of a page node, copy it to a new object, and rewrite all
but the first instance in the page tree to that new number. *)
exception RewriteDone

(* Rewrite first instance of an indirect in an array of such. *)
let rec rewrite_first_kid m n = function
    [] -> []
  | Pdf.Indirect x::t when x = m -> Pdf.Indirect n :: t
  | h::t -> h :: rewrite_first_kid m n t

(* Rewrite first instance of m if any, in obj to n at objnum. Raise Rewrite if
we did it. *)
let rewrite_first_indirect pdf objnum obj m n =
  match Pdf.lookup_direct pdf "/Kids" obj with
    Some (Pdf.Array kids) ->
      if mem (Pdf.Indirect m) kids then
        let newobj =
          Pdf.add_dict_entry obj "/Kids" (Pdf.Array (rewrite_first_kid m n kids))
        in
          Pdf.addobj_given_num pdf (objnum, newobj);
          raise RewriteDone
  | _ -> failwith "rewrite_first_indirect"

(* Those page tree nodes which are not pages *)
let page_tree_nodes_not_pages pdf =
  let objs = ref [] in
    Pdf.objiter
      (fun objnum o ->
        match o with
          Pdf.Dictionary d when lookup "/Type" d = Some (Pdf.Name "/Pages") ->
            objs := (objnum, o) :: !objs
        | _ -> ())
      pdf;
    !objs

let rewrite_page_tree_first pdf m =
  let n = Pdf.addobj pdf (Pdf.lookup_obj pdf m)
  and nodes = page_tree_nodes_not_pages pdf in
    try
      iter
        (fun (objnum, obj) -> rewrite_first_indirect pdf objnum obj m n)
        nodes
    with
      RewriteDone -> () 
    | _ -> raise (Pdf.PDFError "rewrite_page_tree_first: malformed page tree")

(* Run this strategy repeatedly, until there are no duplicate page objects *)
let rec fixup_duplicate_pages pdf =
  let pagerefs = Pdf.page_reference_numbers pdf in
    let groups =
      keep
        (fun x -> length x > 1)
        (collate compare (sort compare pagerefs))
    in
      match groups with
        (h::_)::_ ->
          rewrite_page_tree_first pdf h;
          fixup_duplicate_pages pdf
      | _ -> ()

let pdf_of_pages ?(retain_numbering = false) basepdf range =
  let page_labels =
    if List.length (Pdfpagelabels.read basepdf) = 0 then [] else
      if retain_numbering
        then Pdfpagelabels.merge_pagelabels [basepdf] [range]
        else []
  and marks =
    let refnums = Pdf.page_reference_numbers basepdf in
    let fastrefnums = hashtable_of_dictionary (combine refnums (indx refnums)) in
    let table = hashset_of_list range in
      option_map
        (function m -> if Hashtbl.mem table (pagenumber_of_target ~fastrefnums basepdf m.Pdfmarks.target) then Some m else None)
        (Pdfmarks.read_bookmarks basepdf)
  in
    let pdf = Pdf.empty () in
      Pdf.objiter (fun k v -> ignore (Pdf.addobj_given_num pdf (k, v))) basepdf;
      let page_numbers = Pdf.page_reference_numbers basepdf in
        let pdf =
          {pdf with
             Pdf.major = basepdf.Pdf.major;
             Pdf.minor = basepdf.Pdf.minor;
             Pdf.trailerdict = basepdf.Pdf.trailerdict;
             Pdf.saved_encryption = basepdf.Pdf.saved_encryption}
        in
          let existing_root_entries =
            try
              match Pdf.lookup_obj basepdf basepdf.Pdf.root with | Pdf.Dictionary d -> d | _ -> []
            with
              _ -> []
          in
              let objnumbers = map (function i -> select i page_numbers) range in
              let old_pagetree_root_num =
                match Pdf.lookup_direct basepdf "/Root" pdf.Pdf.trailerdict with
                | Some (Pdf.Dictionary d) ->
                    begin match lookup "/Pages" d with
                    | Some (Pdf.Indirect i) -> i
                    | _ -> raise (Pdf.PDFError "pdf_of_pages")
                    end
                | _ -> raise (Pdf.PDFError "pdf_of_pages")
              in
              (* 1. Look through all the page objects to be included, and
              replicate inheritable entries from their parent nodes, since they
              may fail to exist, leaving pages without Media boxes or
              resources! Inheritable entries are /MediaBox /CropBox /Rotate
              /Resources *)
              iter
                (function objnum ->
                   let replace_inherit objnum entry =
                     let obj = Pdf.lookup_obj pdf objnum in
                       (* Find the first parent entry we can which has the correct attribute. *)
                       let rec find_attribute obj =
                         (* Only replace if not there! *)
                         match Pdf.lookup_direct pdf entry obj with
                         | Some _ -> None
                         | _ ->
                           match Pdf.lookup_direct pdf "/Parent" obj with
                           | Some (Pdf.Dictionary parent) ->
                               (* Does this one have the attribute? If yes,
                               return, if no carry on looking... Don't use a
                               direct lookup, because we want to retain the
                               indirect reference to resources, for example. *)
                               begin match lookup entry parent with
                               | Some pdfobj -> Some pdfobj
                               | None -> find_attribute (Pdf.Dictionary parent)
                               end
                           | _ -> None (* Got to top, couldn't find anything *)
                       in
                         match find_attribute obj with
                         | None -> ()
                         | Some replacement_attr ->
                            (* Replace the attribute with replacement_attr, updating the page object in place. *)
                            Pdf.addobj_given_num pdf (objnum, Pdf.add_dict_entry obj entry replacement_attr)
                   in
                     replace_inherit objnum "/MediaBox";
                     replace_inherit objnum "/CropBox";
                     replace_inherit objnum "/Rotate";
                     replace_inherit objnum "/Resources")
                objnumbers;
              let thetree = pagetree_with_objnumbers true old_pagetree_root_num (source pdf.Pdf.objects.Pdf.maxobjnum) objnumbers 0 in
              let rec findparent objnum = function
              | OLf (objnumbers, parent, this) -> if mem objnum objnumbers then Some this else None
              | OBr (objnumbers, left, right, _, this) ->
                  if mem objnum objnumbers then Some this else
                    match findparent objnum left with
                    | Some parent -> Some parent
                    | None -> findparent objnum right
              in
              (* 2. Kill the old page tree, excepting pages which will appear in the new
              PDF. It will link, via /Parent entries etc, to the new page tree. To do
              this, we remove all objects with /Type /Page or /Type /Pages. The other
              places that null can appear, in destinations and so on, are ok, we think.
              Also, rewrite /Parent entries to point directly to the page root.  *)
              Pdf.objiter
                (fun i o ->
                  match o with
                  | Pdf.Dictionary d ->
                      begin match lookup "/Type" d with
                      | Some (Pdf.Name ("/Pages")) -> Pdf.removeobj pdf i
                      | Some (Pdf.Name ("/Page")) ->
                          if mem i objnumbers
                            then
                              begin match findparent i thetree with
                              | Some p ->
                                  Pdf.addobj_given_num pdf (i, (Pdf.Dictionary (add "/Parent" (Pdf.Indirect p) d)))
                              | None -> raise (Pdf.PDFError "pdf_of_pages internal inconsistency")
                              end
                            else
                              Pdf.removeobj pdf i
                      | _ -> ()
                      end
                  | _ -> ())
                pdf;
                (* Now, add the new page tree, with root at the same object
                number, and finish *)
                let new_pagetree = pdf_of_pages_build_pagetree thetree objnumbers pdf in
                  Pdf.addobj_given_num pdf (old_pagetree_root_num, new_pagetree);
                    let pdf = add_root old_pagetree_root_num existing_root_entries pdf in
                    Pdfpagelabels.write pdf page_labels;
                    let pdf = Pdfmarks.add_bookmarks marks pdf in
                      fixup_duplicate_pages pdf;
                      pdf

let prepend_operators pdf ops ?(fast=false) page =
  if fast then
    {page with content =
       Pdfops.stream_of_ops ops :: page.content}
  else
    let old_ops =
      Pdfops.parse_operators pdf page.resources page.content
    in
      {page with content =
        [Pdfops.stream_of_ops (ops @ old_ops)]}

(* Add stack operators to a content stream to ensure it is composeable. *)
let protect pdf resources content =
  let ops = Pdfops.parse_operators pdf resources content in
    let qs = length (keep (eq Pdfops.Op_q) ops)
    and bigqs = length (keep (eq Pdfops.Op_Q) ops) in
    let deficit = if qs > bigqs then qs - bigqs else 0 in
      if deficit <> 0 then Printf.eprintf "Q Deficit was nonzero. Fixing. %i\n" deficit;
      many Pdfops.Op_Q deficit

(* We check for q/Q mismatches in existing section. *)
let postpend_operators pdf ops ?(fast=false) page =
  if fast then
    {page with content =
       [Pdfops.stream_of_ops ([Pdfops.Op_q] @ ops @ [Pdfops.Op_Q])] @ page.content}
  else
    let beforeops =
      [Pdfops.Op_q]
    and afterops =
      protect pdf page.resources page.content @ [Pdfops.Op_Q] @ ops
    in
      {page with content =
         [Pdfops.stream_of_ops (beforeops @ Pdfops.parse_operators pdf page.resources page.content @ afterops)]}

(* Source of possible prefix strings. String is always copied. *)
let next_string s =
  if s = "" then "a" else
    if s.[0] = 'z' then "a" ^ s else
      let s' = String.copy s in
         s'.[0] <- char_of_int (int_of_char s'.[0] + 1);
         s'

(* True if one string [p] is a prefix of another [n] *)
let is_prefix p n =
  String.length p <= String.length n &&
  String.sub n 0 (String.length p) = p

(* a) List every name used in a /Resources in a /Type /Page or
 /Type /Pages (without the leading "/"
   b) Find the shortest lower-case alphabetic string which is not a prefix of any of these
   strings. This prefix can be added to the other PDF's names, and will never
   clash with any of these. *)
let names_used pdf =
  let names = ref [] in
  let unslash x =
    if x = "" then "" else String.sub x 1 (String.length x - 1)
  in
    Pdf.objiter
      (fun n obj ->
        match obj with
          Pdf.Dictionary d | Pdf.Stream {contents = (Pdf.Dictionary d, _)} ->
            begin match lookup "/Type" d with
              Some (Pdf.Name ("/Page" | "/Pages")) ->
                begin match Pdf.lookup_direct pdf "/Resources" obj with
                  Some resources ->
                    List.iter
                      (fun key ->
                         match Pdf.lookup_direct pdf key resources with
                           Some (Pdf.Dictionary d) ->
                             List.iter
                               (fun (k, _) -> names := unslash k::!names)
                               d
                         | _ -> ())
                      resource_keys
                | _ -> ()
                end
            | _ -> ()
            end
        | _ -> ()
      )
      pdf;
    setify !names

let shortest names =
  let rec loop prefix =
    if List.exists (is_prefix prefix) names
      then loop (next_string prefix)
      else prefix
  in
    loop "a"

let shortest_unused_prefix pdf =
  shortest (names_used pdf)

let addp p n =
  if n = "" then raise (Pdf.PDFError "addp: blank name") else
    "/" ^ p ^ String.sub n 1 (String.length n - 1)

let prefix_operator pdf p = function
  | Pdfops.Op_Tf (f, s) -> Pdfops.Op_Tf (addp p f, s)
  | Pdfops.Op_gs n -> Pdfops.Op_gs (addp p n)
  | Pdfops.Op_CS n -> Pdfops.Op_CS (addp p n)
  | Pdfops.Op_cs n -> Pdfops.Op_cs (addp p n)
  | Pdfops.Op_SCNName (s, ns) -> Pdfops.Op_SCNName (addp p s, ns)
  | Pdfops.Op_scnName (s, ns) -> Pdfops.Op_scnName (addp p s, ns)
  | Pdfops.Op_sh s -> Pdfops.Op_sh (addp p s)
  | Pdfops.Op_Do x -> Pdfops.Op_Do (addp p x)
  | Pdfops.Op_DP (n, Pdf.Name x) -> Pdfops.Op_DP (n, Pdf.Name (addp p x))
  | Pdfops.Op_BDC (n, Pdf.Name x) -> Pdfops.Op_BDC (n, Pdf.Name (addp p x))
  | Pdfops.InlineImage (dict, bytes) ->
      (* Replace any indirect "/CS" or "/ColorSpace" with a new "/CS" *)
      let dict' =
        match Pdf.lookup_direct_orelse pdf "/CS" "/ColorSpace" dict with
        | Some (Pdf.Name "/DeviceGray")
        | Some (Pdf.Name "/DeviceRGB")
        | Some (Pdf.Name "/DeviceCMYK")
        | Some (Pdf.Name "/G")
        | Some (Pdf.Name "/RGB")
        | Some (Pdf.Name "/CMYK") -> dict
        | Some (Pdf.Name n) ->
            Pdf.add_dict_entry
              (Pdf.remove_dict_entry
                (Pdf.remove_dict_entry dict "/ColorSpace")
                "/CS")
              "/CS"
              (Pdf.Name (addp p n))
        | _ -> dict
      in
        Pdfops.InlineImage (dict', bytes)
  | x -> x

let change_resources pdf prefix resources =
  let newdict name =
    match Pdf.lookup_direct pdf name resources with
    | Some (Pdf.Dictionary fonts) ->
        Pdf.Dictionary (map (fun (k, v) -> addp prefix k, v) fonts)
    | _ -> Pdf.Dictionary []
  in
    let newdicts = map newdict resource_keys in
      let resources = ref resources in
        iter2
          (fun k v ->
            resources := Pdf.add_dict_entry !resources k v)
          resource_keys
          newdicts;
        !resources

(* For each object in the PDF with /Type /Page or /Type /Pages:
  a) Add the prefix to any name in /Resources
  b) Add the prefix to any name used in any content streams, keeping track of
  the streams we have processed to preserve sharing
  
FIXME: If a non-ISO PDF with content streams which don't end on lexical boundaries
is provided, the parse will fail, and this function will raise an exception.

The long-term solution to this is to explode the PDF with an
-unshare-content-streams option, and then fix squeeze to re-share subcontent
streams *)
let add_prefix pdf prefix =
  let fixed_streams = Hashtbl.create 100 in
  let fix_stream resources i =
    match i with Pdf.Indirect i ->
      (*Printf.eprintf "fixing stream %i\n" i;*)
      if not (Hashtbl.mem fixed_streams i) then
        let operators = Pdfops.parse_operators pdf resources [Pdf.Indirect i] in
          (*Printf.eprintf "calling prefix_operator on stream %i\n" i;*)
          let operators' = map (prefix_operator pdf prefix) operators in
            begin match Pdf.lookup_obj pdf i with
              Pdf.Stream ({contents = (dict, stream)} as s) ->
                begin match Pdfops.stream_of_ops operators' with
                  Pdf.Stream {contents = ncontents} -> s := ncontents
                | _ -> failwith "add_prefix: bad stream"
                end
            | _ -> failwith "add_prefix: bad stream 2"
            end;
            Hashtbl.add fixed_streams i ()
    | _ -> failwith "add_prefix: not indirect"
  in
  Pdf.objselfmap
    (fun obj ->
       match obj with
         Pdf.Dictionary dict as d ->
           begin match Pdf.lookup_direct pdf "/Type" d with
             Some (Pdf.Name ("/Page" | "/Pages")) ->
               let resources, resources' =
                 begin match Pdf.lookup_direct pdf "/Resources" obj with
                   Some resources -> Some resources, Some (change_resources pdf prefix resources)
                 | _ -> None, None
                 end
               in
                 begin match lookup "/Contents" dict with
                   Some (Pdf.Indirect i) ->
                     fix_stream
                       (if resources = None then Pdf.Dictionary [] else unopt resources)
                       (Pdf.Indirect i)
                 | Some (Pdf.Array a) ->
                     List.iter
                       (fix_stream
                         (if resources = None then Pdf.Dictionary [] else unopt resources))
                       a
                 | _ -> ()
                 end;
                 begin match resources' with
                   Some x -> Pdf.add_dict_entry d "/Resources" x
                 | None -> d
                 end
           | _ -> obj
           end
       | _ -> obj)
    pdf(*;
    Printf.eprintf "***add_prefix has concluded\n";*)

