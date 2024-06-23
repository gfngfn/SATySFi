
open MyUtil
open EnvelopeSystemBase
open Types


type yaml_error =
  | ParseError             of string
  | FieldNotFound          of YamlDecoder.context * string
  | NotAFloat              of YamlDecoder.context
  | NotAString             of YamlDecoder.context
  | NotABool               of YamlDecoder.context
  | NotAnArray             of YamlDecoder.context
  | NotAnObject            of YamlDecoder.context
  | BranchNotFound of {
      context       : YamlDecoder.context;
      expected_tags : string list;
      got_tags      : string list;
    }
  | MoreThanOneBranchFound of {
      context       : YamlDecoder.context;
      expected_tags : string list;
      got_tags      : string list;
    }
  | NotACommand of {
      context : YamlDecoder.context;
      prefix  : char;
      string  : string;
    }
[@@deriving show { with_path = false }]

module YamlError = struct
  type t = yaml_error
  let parse_error s = ParseError(s)
  let field_not_found context s = FieldNotFound(context, s)
  let not_a_float context = NotAFloat(context)
  let not_a_string context = NotAString(context)
  let not_a_bool context = NotABool(context)
  let not_an_array context = NotAnArray(context)
  let not_an_object context = NotAnObject(context)
  let branch_not_found context expected_tags got_tags =
    BranchNotFound{ context; expected_tags; got_tags }
  let more_than_one_branch_found context expected_tags got_tags =
    MoreThanOneBranchFound{ context; expected_tags; got_tags }
end

type config_error =
  | UnexpectedExtension             of string
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
  | DepsConfigNotFound        of abs_path
  | DepsConfigError           of abs_path * yaml_error
  | EnvelopeConfigNotFound    of abs_path
  | EnvelopeConfigError       of abs_path * yaml_error
  | DependedEnvelopeNotFound  of envelope_name
  | MarkdownClassNotFound
  | NoMarkdownConversion
  | MarkdownError             of MarkdownParser.error
