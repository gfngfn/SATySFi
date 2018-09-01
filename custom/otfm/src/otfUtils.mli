
val debugfmt : Format.formatter

val fmtgen : Format.formatter

val fmtGSUB : Format.formatter

val fmtMATH : Format.formatter

module Alist :
  sig
    type 'a t
    val empty : 'a t
    val extend : 'a t -> 'a -> 'a t
    val append : 'a t -> 'a list -> 'a t
    val to_list : 'a t -> 'a list
  end

val err_invalid_tag : string -> string

val unsafe_chr : int -> char

val unsafe_byte : string -> int -> int

val pp : Format.formatter -> ('a, Format.formatter, unit) format -> 'a

val pp_list :
  ?pp_sep:(Format.formatter -> unit -> unit) ->
  (Format.formatter -> 'a -> 'b) -> Format.formatter -> 'a list -> unit

val ( >>= ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result

val return : 'a -> ('a, 'b) result

val err : 'a -> ('b, 'a) result

type byte = char

module WideInt : sig
  type t
  val ( lsl ) : t -> int -> t
  val ( lsr ) : t -> int -> t
  val ( lor ) : t -> t -> t
  val ( land ) : t -> t -> t
  val ( mod ) : t -> t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val of_int : int -> t
  val to_int : t -> int
  val of_int64 : int64 -> t
  val to_int64 : t -> int64
  val of_byte : byte -> t
  val to_byte : t -> byte
  val is_in_int32 : t -> bool
  val is_in_uint32 : t -> bool
  val is_in_int64 : t -> bool
  val is_neg : t -> bool
  val pp : Format.formatter -> t -> unit
end

type wint = WideInt.t

val ( +% ) : wint -> wint -> wint

val ( -% ) : wint -> wint -> wint

val ( !% ) : int -> wint

val ( !%% ) : int64 -> wint

val cut_uint32_unsafe : wint -> byte * byte * byte * byte
