
open PackageSystemBase


let main (_deps_with_flags : (dependency_flag * package_dependency) list) : (package_dependency list) LocalFixedPackageIdMap.t =
  LocalFixedPackageIdMap.empty
    (* TODO: construct local_fixed_dependencies by traversing local fixed dependencies *)
