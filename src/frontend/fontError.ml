
open MyUtil

type font_error =
  | FailedToReadFont              of abs_path * string
  | FailedToDecodeFont            of Otfed.Decode.Error.t
  | NotASingleFont                of abs_path
  | NotAFontCollectionElement     of abs_path * int
  | CannotFindLibraryFileAsToFont of lib_path * abs_path list
  | NoMathTable                   of abs_path
  | PostscriptNameNotFound        of abs_path
  | CannotFindUnicodeCmap         of abs_path
  | CollectionIndexOutOfBounds of {
      path         : abs_path;
      index        : int;
      num_elements : int;
    }
