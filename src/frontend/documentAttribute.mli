
open Types
open PackageSystemBase


type error =
  | LabelNotFound of {
      record_range : Range.t;
      label        : label;
    }
  | DuplicateLabel of {
      record_range : Range.t;
      label        : label;
    }
  | NoConfigArgument         of Range.t
  | DuplicateConfigAttribute of Range.t * Range.t
  | NotAVersionRequirement   of Range.t * string
  | NotAPackageDependency    of Range.t
  | NotARegistry             of Range.t
  | NotARegistryRemote       of Range.t
  | NotAListLiteral          of Range.t
  | NotAStringLiteral        of Range.t
  | DuplicateRegistryLocalName of {
      list_range          : Range.t;
      registry_local_name : registry_local_name;
    }

type t = {
  registry_specs : registry_remote RegistryLocalNameMap.t;
  dependencies   : package_dependency list;
}

val make : untyped_attribute list -> (t, error) result
