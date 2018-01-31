
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
  | Single    of Uchar.t list
  | Fractions of (Uchar.t list) list

val empty : t

val main : dir_path -> file_path -> t

val lookup : t -> Uchar.t list -> answer
