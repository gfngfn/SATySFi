
open CharBasis

type dir_path = string
type file_path = string

exception InvalidPatternElement of Range.t

type t

type answer =
  | Single    of uchar_segment list
  | Fractions of (uchar_segment list) list

val empty : t

val main : file_path -> t

val lookup : int -> int -> t -> uchar_segment list -> answer
