(** Bookmarks *)

(** The type of bookmarks. *)
type t =
  {level : int;
   text : string;
   target : Pdfdest.t;
   isopen : bool}

(** Debug string from a bookmark. *)
val string_of_bookmark : t -> string

(** Read the bookmarks from a document. *)
val read_bookmarks : Pdf.t -> t list

(** Remove the bookmarks from a document. *)
val remove_bookmarks : Pdf.t -> Pdf.t

(** Add bookmarks to a document, replacing any currently there. *)
val add_bookmarks : t list -> Pdf.t -> Pdf.t

