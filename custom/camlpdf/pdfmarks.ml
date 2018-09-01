(* PDF Bookmarks *)
open Pdfutil

type t =
  {level : int;
   text : string;
   target : Pdfdest.t;
   isopen : bool}

let string_of_bookmark m =
   Printf.sprintf "%i %s %s %b\n" m.level m.text (Pdfdest.string_of_destination m.target) m.isopen

let remove_bookmarks pdf =
  match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
  | None -> raise (Pdf.PDFError "remove_boomarks: Bad PDF: no root")
  | Some catalog ->
      let catalog' = Pdf.remove_dict_entry catalog "/Outlines" in
        let newcatalognum = Pdf.addobj pdf catalog' in
          {pdf with
            Pdf.root = newcatalognum;
            Pdf.trailerdict =
              Pdf.add_dict_entry
                pdf.Pdf.trailerdict "/Root" (Pdf.Indirect newcatalognum)}

type ntree =
  Br of int * Pdf.pdfobject * ntree list * bool (* bool is will_be_open *)

let rec print_tree (Br (i, obj, ls, isopen)) =
  Printf.printf "Br (%i, %s, %b\n" i (Pdfwrite.string_of_pdf obj) isopen;
  iter print_tree ls;
  flprint ")\n"

let fresh source pdf =
  incr source; pdf.Pdf.objects.Pdf.maxobjnum + !source

(* True if there are any open nodes in the tree *)
let rec any_open_nodes = function
  | [] -> false
  | (Br (_, _, ls, o))::r ->
       o || any_open_nodes ls || any_open_nodes r

(* Total visible nodes in a tree *)
let rec total_visible = function
  | [] -> 0
  | (Br (_, _, ls, o))::r ->
       1 + (if o then total_visible ls else 0) + total_visible r

(* Flatten a tree and produce a root object for it. Return a list of
(num, pdfobject) pairs with the root first. *)
let flatten_tree source pdf = function
  | [] ->
      let n = fresh source pdf in
        [(n, Pdf.Dictionary [])], n
  | tree ->
      let root_objnum = fresh source pdf in
      (* Add /Parent links to root *)
      let tree =
        let add_root_parent (Br (i, dict, children, isopen)) =
          Br
            (i,
             Pdf.add_dict_entry dict "/Parent" (Pdf.Indirect root_objnum),
             children,
             isopen)
        in
          map add_root_parent tree
      in
        let rec really_flatten = function
          Br (i, pdfobject, children, isopen) ->
            (i, pdfobject) :: flatten (map really_flatten children)
        in
          let all_but_top = flatten (map really_flatten tree)
          in let top, topnum =
            (* Make top level from objects at first level of tree *)
            let Br (first, _, _, _), Br (last, _, _, _) = extremes tree in
              let count =
                if any_open_nodes tree
                  then ["/Count", (Pdf.Integer (total_visible tree))]
                  else []
              in
               (root_objnum, Pdf.Dictionary
                 ([("/First", Pdf.Indirect first); ("/Last", Pdf.Indirect last)] @ count)),
               root_objnum
          in
            top::all_but_top, topnum

(* Add negative /Count entries to an ntree *)
let rec add_counts l = map add_count l

and add_count (Br (i, obj, ls, isopen)) =
  let newobj =
    if ls = [] then obj else
      if isopen then
        (* Calculate sum of the number of visible descendent items *)
        Pdf.add_dict_entry obj "/Count" (Pdf.Integer (total_visible ls))
      else
        (* Negative - abs value is number of descendants which would be visible *) 
        Pdf.add_dict_entry obj "/Count" (Pdf.Integer (~-(total_visible ls)))
  in
    Br (i, newobj, add_counts ls, isopen)

(* Add /Parent entries to an ntree *)
let rec add_parent parent (Br (i, obj, children, isopen)) =
  let obj' =
    match parent with
    | None -> obj
    | Some parent_num ->
        Pdf.add_dict_entry obj "/Parent" (Pdf.Indirect parent_num)
  in
    Br (i, obj', map (add_parent (Some i)) children, isopen)

(* Add /First and /Last entries to an ntree *)
let rec add_firstlast (Br (i, obj, children, isopen)) =
  match children with
  | [] -> (Br (i, obj, children, isopen))
  | c ->
      match extremes c with
        Br (i', _, _, _), Br (i'', _, _, _) ->
          let obj = Pdf.add_dict_entry obj "/First" (Pdf.Indirect i') in
            let obj = Pdf.add_dict_entry obj "/Last" (Pdf.Indirect i'') in
              (Br (i, obj, map add_firstlast children, isopen))
       
(* Add /Next and /Prev entries to an ntree *)
let rec add_next (Br (i, obj, children, isopen)) =
  match children with
  | [] -> Br (i, obj, children, isopen)
  | [_] -> Br (i, obj, map add_next children, isopen)
  | c::cs ->
      let numbers = map (fun (Br (i, _, _, _)) -> i) cs in
        let children' =
          (map2
             (fun (Br (i, obj, children, isopen)) nextnum ->
                Br (i,
                    Pdf.add_dict_entry obj "/Next" (Pdf.Indirect nextnum),
                    children,
                    isopen))
             (all_but_last (c::cs))
             numbers)
          @ [last cs]
        in
          Br (i, obj, map add_next children', isopen)

let rec add_prev (Br (i, obj, children, isopen)) =
  match children with
  | [] -> Br (i, obj, children, isopen)
  | [_] -> Br (i, obj, map add_prev children, isopen)
  | c::cs ->
      let numbers = map (fun (Br (i, _, _, _)) -> i) (all_but_last (c::cs)) in
        let children' =
          c::
            map2
              (fun (Br (i, obj, children, isopen)) prevnum ->
                 Br (i,
                     Pdf.add_dict_entry obj "/Prev" (Pdf.Indirect prevnum),
                     children,
                     isopen))
              cs
              numbers
        in
          Br (i, obj, map add_prev children', isopen)

(* Make a node from a given title, destination page number in a given PDF ond
open flag. *)
let node_of_line pdf title target =
  Pdf.Dictionary
    (("/Title", Pdf.String title)::
     let dest = Pdfdest.pdfobject_of_destination target in
       if dest = Pdf.Null then [] else [("/Dest", dest)])

(* Make an ntree list from a list of parsed bookmark lines. *)
let rec make_outline_ntree source pdf = function
  | [] -> []
  | h::t ->
      let lower, rest = cleavewhile (fun {level = n'} -> n' > h.level) t in
        let node = node_of_line pdf h.text h.target in
          Br (fresh source pdf, node, make_outline_ntree source pdf lower, h.isopen)
            ::make_outline_ntree source pdf rest

(* Add bookmarks. *)
let add_bookmarks parsed pdf =
  if parsed = [] then remove_bookmarks pdf else
    let source = ref 0 in
    let tree = make_outline_ntree source pdf parsed in
      (* Build the (object number, bookmark tree object) pairs. *)
      let pairs, tree_root_num =
        let tree = map add_firstlast tree in
          let tree =
            match add_next (add_prev (Br (0, Pdf.Null, tree, false))) with
              Br (_, _, children, _) -> children
          in
            let tree = add_counts (map (add_parent None) tree) in
              flatten_tree source pdf tree
      in
        (* Add the objects to the pdf *)
        iter (function x -> ignore (Pdf.addobj_given_num pdf x)) pairs;
        (* Replace the /Outlines entry in the document catalog. *)
        match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
        | None -> raise (Pdf.PDFError "Bad PDF: no root")
        | Some catalog ->
            let catalog' =
              Pdf.add_dict_entry catalog "/Outlines" (Pdf.Indirect tree_root_num)
            in
              let newcatalognum = Pdf.addobj pdf catalog' in
                {pdf with
                  Pdf.root = newcatalognum;
                  Pdf.trailerdict =
                    Pdf.add_dict_entry
                      pdf.Pdf.trailerdict "/Root" (Pdf.Indirect newcatalognum)}

(* Read bookmarks *)
let rec traverse_outlines_lb indent_lb pdf outlines output =
  match Pdf.lookup_direct pdf "/First" outlines with
  | None -> ()
  | Some first -> do_until_no_next_lb indent_lb pdf first output

and do_until_no_next_lb indent_lb pdf outline output =
  begin match Pdf.lookup_direct pdf "/Title" outline with
  | Some (Pdf.String s) ->
      let page =
        match Pdf.lookup_direct pdf "/Dest" outline with
        | Some dest -> Pdfdest.read_destination pdf dest
        | None ->
            match Pdf.lookup_direct pdf "/A" outline with
            | None -> Pdfdest.NullDestination
            | Some action ->
                match Pdf.lookup_direct pdf "/D" action with
                | None -> Pdfdest.NullDestination
                | Some dest -> Pdfdest.read_destination pdf dest
      in let opn =
        match Pdf.lookup_direct pdf "/Count" outline with
        | Some (Pdf.Integer i) when i > 0 -> true
        | _ -> false
      in
        output {level = !indent_lb; text = s; target = page; isopen = opn}
    | _ -> ()
    end;
    incr indent_lb;
    traverse_outlines_lb indent_lb pdf outline output;
    if !indent_lb > 0 then decr indent_lb;
    begin match Pdf.lookup_direct pdf "/Next" outline with
    | None -> ()
    | Some outline -> do_until_no_next_lb indent_lb pdf outline output
    end

let read_bookmarks pdf =
  match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
  | None -> raise (Pdf.PDFError "read_bookmarks - Bad PDF: no root")
  | Some catalog ->
      match Pdf.lookup_direct pdf "/Outlines" catalog with
      | None -> []
      | Some outlines ->
          let out = ref [] in
            let output = (function b -> out := b::!out) in
              traverse_outlines_lb (ref 0) pdf outlines output;
              rev !out

