
open MyUtil
open EnvelopeSystemBase
open Types


type config_error =
  | CyclicFileDependency            of (abs_path * untyped_library_file) cycle
  | CannotReadFileOwingToSystem     of string
  | LibraryContainsWholeReturnValue of abs_path
  | DocumentLacksWholeReturnValue   of abs_path
  | CannotUseHeaderUse              of module_name_chain ranged
  | CannotUseHeaderUseOf            of module_name_chain ranged
  | FailedToParse                   of parse_error
  | MainModuleNameMismatch of {
      expected : module_name;
      got      : module_name;
    }
  | TypeError                 of TypeError.type_error
  | NotALibraryFile           of abs_path
  | FileModuleNotFound        of Range.t * module_name
  | FileModuleNameConflict    of module_name * abs_path * abs_path
  | NotADocumentFile          of abs_path * mono_type
  | NotAStringFile            of abs_path * mono_type
  | NoMainModule              of module_name
  | UnknownPackageDependency  of Range.t * module_name
  | EnvelopeNameConflict      of envelope_name
  | DependencyOnUnknownEnvelope of {
      depending : envelope_name;
      depended  : envelope_name;
    }
  | CyclicEnvelopeDependency of (envelope_name * untyped_envelope) cycle
  | LibraryRootConfigNotFoundIn of lib_path * abs_path list
  | LocalFileNotFound of {
      relative   : string;
      candidates : abs_path list;
    }
