
open MyUtil
open CharBasis

exception InvalidPatternElement of Range.t

type t

type answer =
  | Single    of uchar_segment list
  | Fractions of (uchar_segment list) list

val empty : t

val main : abs_path -> t

val lookup : int -> int -> t -> uchar_segment list -> answer
