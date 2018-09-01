(** Representing and Parsing PDF Dates *)

(** The type of a date. *)
type t =
  {year : int;
   month : int;
   day : int;
   hour : int;
   minute : int;
   second : int;
   hour_offset : int;
   minute_offset : int}

(** Raised when [date_of_string] fails. *)
exception BadDate

(** Build a date by parsing a PDF date string. Raises [BadDate] on failure. *)
val date_of_string : string -> t

(** Build a string from a date. *)
val string_of_date : t -> string

(**/**)
val test : unit -> unit

