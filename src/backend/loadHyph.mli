
open CharBasis

type dir_path = string
type file_path = string

exception InvalidYOJSON               of file_path * string
exception OtherThanDictionary         of file_path
exception NotProvidingExceptionList   of file_path
exception ExceptionListOtherThanArray of file_path
exception InvalidExceptionElement     of file_path
exception NotProvidingPatternList     of file_path
exception PatternListOtherThanArray   of file_path
exception InvalidPatternElement       of file_path

type t

type answer =
  | Single    of uchar_segment list
  | Fractions of (uchar_segment list) list

val empty : t

val main : file_path -> t

val lookup : int -> int -> t -> uchar_segment list -> answer
