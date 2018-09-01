(** Optional Content Groups. *)

(** This will eventually be support for optional content group manipulation. It is as-yet unfinished *)

open Pdfutil

type ocgusage (* Nothing for now; expand later *)

type ocg =
  {ocg_name : string;
   ocg_intent : string list;
   ocg_usage : ocgusage option}

type ocgstate = OCG_ON | OCG_OFF | OCG_Unchanged

type ocglistmode = OCG_AllPages | OCG_VisiblePages

type ocgappdict (* Nothing for now; expand later *)

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

(** Read optional content data. *)
val read_ocg : Pdf.t -> ocgproperties option

(** Write optional content data. *)
val write_ocg : Pdf.t -> ocgproperties -> unit

(**/**)

val print_document_ocg : Pdf.t -> unit

