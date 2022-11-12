type t

val to_code : t -> string
(** [to_code t] converts [t] to OCaml representation of [t] *)

val to_string : t -> string
(** [to_string t] converts [t] to human-readable string (SATySFi format) *)

val (@->) : t -> t -> t
(** arrow type. *)

val forall : string -> (t -> t) -> t
(** type variable binder.

    NB: All type variables should be introduced by [forall].
    For example, ['a -> 'a list] should be written as [forall "a" (fun a -> tL a)].
*)

(** {1 predefined types and type aliases} *)

val tU          : t
val tI          : t
val tFL         : t
val tB          : t
val tLN         : t
val tS          : t
val tIT         : t
val tBT         : t
val tMT         : t
val tIB         : t
val tBB         : t
val tMB         : t
val tCTX        : t
val tTCTX       : t
val tPATH       : t
val tPRP        : t
val tDOC        : t
val tGR         : t
val tIMG        : t
val tRE         : t
val tIPOS       : t
val tITMZ       : t
val tSCR        : t
val tLANG       : t
val tCLR        : t
val tPG         : t
val tMATHCLS    : t
val tMCCLS      : t
val tCELL       : t
val tMCSTY      : t
val tPAREN      : t
val tRULESF     : t
val tPBINFO     : t
val tPT         : t
val tPAGECONTF  : t
val tPAGEPARTSF : t
val tPADS       : t
val tDECOSET    : t
val tFONTKEY    : t
val tFONTWR     : t
val tDECO       : t
val tIGR        : t
val tIGRO       : t
val tDASH       : t
val tDOCINFODIC : t

val tPROD : t list -> t

val tL    : t -> t
val tR    : t -> t
val tOPT  : t -> t
val tCODE : t -> t
val tICMD : t -> t

val mckf     : t
