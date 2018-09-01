(* Pdf Optional Content Groups *)
open Pdfutil

type ocgusage (* Nothing for now; expand later *)

type ocg =
  {ocg_name : string;
   ocg_intent : string list;
   ocg_usage : ocgusage option}

type ocgstate = OCG_ON | OCG_OFF | OCG_Unchanged

type ocglistmode = OCG_AllPages | OCG_VisiblePages

type ocgappdict = AppDict (* Nothing for now; expand later *)

type ocgconfig =
  {ocgconfig_name : string option;
   ocgconfig_creator : string option;
   ocgconfig_basestate : ocgstate;
   ocgconfig_on : int list option;
   ocgconfig_off : int list option;
   ocgconfig_intent: string list;
   ocgconfig_usage_application_dictionaries: ocgappdict list option;
   ocgconfig_order : int tree option;
   ocgconfig_listmode : ocglistmode;
   ocgconfig_rbgroups : int list list;
   ocgconfig_locked : int list}

type ocgproperties =
  {ocgs : (int * ocg) list;
   ocg_default_config : ocgconfig;
   ocg_configs : ocgconfig list}

let read_ocgappdict pdf appdict = AppDict

(* Read an OCG from a file, if there is one. None represents a non-existent
/OCProperties, rather than an error. *)
let read_config pdf config =
  let ocgconfig_name =
    match Pdf.lookup_direct pdf "/Name" config with
    | Some (Pdf.String s) -> Some s
    | _ -> None
  and ocgconfig_creator =
    match Pdf.lookup_direct pdf "/Creator" config with
    | Some (Pdf.String s) -> Some s
    | _ -> None
  and ocgconfig_basestate =
    match Pdf.lookup_direct pdf "/BaseState" config with
    | Some (Pdf.Name "/ON") -> OCG_ON
    | Some (Pdf.Name "/OFF") -> OCG_OFF
    | Some (Pdf.Name "/Unchanged") -> OCG_Unchanged
    | _ -> OCG_ON
  and ocgconfig_on =
    match Pdf.lookup_direct pdf "/ON" config with
    | Some (Pdf.Array indirects) -> 
        Some (map (function Pdf.Indirect n -> n | _ -> 0) indirects)
    | _ -> None
  and ocgconfig_off =
    match Pdf.lookup_direct pdf "/OFF" config with
    | Some (Pdf.Array indirects) -> 
        Some (map (function Pdf.Indirect n -> n | _ -> 0) indirects)
    | _ -> None
  and ocgconfig_intent =
    match Pdf.lookup_direct pdf "/Intent" config with
    | Some (Pdf.Name n) -> [n]
    | Some (Pdf.Array a) ->
          (map
            (function x ->
               match Pdf.direct pdf x with | Pdf.Name n -> n | _ -> "")
            a)
    | _ -> ["/View"]
  and ocgconfig_usage_application_dictionaries =
    match Pdf.lookup_direct pdf "/AS" config with
    | Some (Pdf.Array appdicts) -> Some (map (read_ocgappdict pdf) appdicts)
    | _ -> None
  and ocgconfig_order = None (* FIXME *)
  and ocgconfig_listmode =
    match Pdf.lookup_direct pdf "/ListMode" config with
    | Some (Pdf.Name "/VisiblePages") -> OCG_VisiblePages
    | _ -> OCG_AllPages
  and ocgconfig_rbgroups = [] (* FIXME *)
  and ocgconfig_locked = 
    match Pdf.lookup_direct pdf "/Locked" config with
    | Some (Pdf.Array a) ->
          (map
            (function x ->
               match Pdf.direct pdf x with | Pdf.Indirect n -> n | _ -> 0)
            a)
    | _ -> []
  in
    {ocgconfig_name = ocgconfig_name;
     ocgconfig_creator = ocgconfig_creator;
     ocgconfig_basestate = ocgconfig_basestate;
     ocgconfig_on = ocgconfig_on;
     ocgconfig_off = ocgconfig_off;
     ocgconfig_intent = ocgconfig_intent;
     ocgconfig_usage_application_dictionaries = ocgconfig_usage_application_dictionaries;
     ocgconfig_order = ocgconfig_order;
     ocgconfig_listmode = ocgconfig_listmode;
     ocgconfig_rbgroups = ocgconfig_rbgroups;
     ocgconfig_locked = ocgconfig_locked}

let read_ocg_usage pdf usage = None (* FIXME *) 

let read_individual_ocg pdf ocg =
  let ocg_name =
    match Pdf.lookup_direct pdf "/Name" ocg with
    | Some (Pdf.String s) -> s
    | _ -> raise (Pdf.PDFError "No /Name in optional content group")
  and ocg_intent =
    match Pdf.lookup_direct pdf "/Intent" ocg with
    | Some (Pdf.Name n) -> [n]
    | Some (Pdf.Array a) ->
       option_map (function Pdf.Name n -> Some n | _ -> None) (map (Pdf.direct pdf) a)
    | _ -> ["/View"]
  and ocg_usage =
    match Pdf.lookup_direct pdf "/Usage" ocg with
    | None -> None
    | Some usage -> read_ocg_usage pdf usage
  in
    {ocg_name = ocg_name;
     ocg_intent = ocg_intent;
     ocg_usage = ocg_usage}

let read_ocg pdf =
  match Pdf.lookup_direct pdf "/Root" pdf.Pdf.trailerdict with
  | None -> raise (Pdf.PDFError "No /Root")
  | Some r ->
      match Pdf.lookup_direct pdf "/OCProperties" r with
      | None -> None
      | Some ocproperties ->
          let ocgs =
            match Pdf.lookup_direct pdf "/OCGs" ocproperties with
            | Some (Pdf.Array indirects) ->
               combine
                 (map (function Pdf.Indirect n -> n | _ -> 0) indirects)
                 (map (read_individual_ocg pdf) indirects)
            | _ -> []
          and ocg_default_config =
            match Pdf.lookup_direct pdf "/D" ocproperties with
            | None -> raise (Pdf.PDFError "No default config in /OCProperties")
            | Some config -> read_config pdf config
          and ocg_configs =
            match Pdf.lookup_direct pdf "/Configs" ocproperties with
            | Some (Pdf.Array configs) -> map (read_config pdf) configs
            | _ -> []
          in
            Some
              {ocgs = ocgs;
               ocg_default_config = ocg_default_config;
               ocg_configs = ocg_configs}

let write_ocg pdf ocgprops = ()

(* Use things called by write_ocg to get the pdf objects, then can use
Pdfwrite.string_of_pdf to do the work *)
let print_ocg (num, ocg) =
  Printf.printf "OCG %i\n" num;
  Printf.printf "Name %s\n" ocg.ocg_name;
  Printf.printf "Intent(s)";
  iter (Printf.printf "%s ") ocg.ocg_intent;
  Printf.printf "\n"

let print_ocg_config config =
  begin match config.ocgconfig_name with
  | None -> Printf.printf "No config name\n"
  | Some n -> Printf.printf "Config Name: %s\n" n
  end

let print_document_ocg pdf =
  match read_ocg pdf with
  | None -> flprint "There are no optional content groups in this document\n"
  | Some ocgs ->
      Printf.printf "There are %i OCGs, and %i additional configs\n"
      (length ocgs.ocgs) (length ocgs.ocg_configs);
      flprint "OCGs\n-------------\n";
      iter print_ocg ocgs.ocgs;
      flprint "OCG Configs\n------------\n";
      iter print_ocg_config ocgs.ocg_configs


