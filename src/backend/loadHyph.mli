
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

type exception_map

type patterns

val main : dir_path -> file_path -> exception_map * patterns
