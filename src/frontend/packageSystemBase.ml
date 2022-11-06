
type package_name = string

type package_restriction =
  | CompatibleWith of SemanticVersion.t

type package_dependency =
    | Dependency of {
        role         : package_name;
        restrictions : package_restriction list;
      }
