
open PackageSystemBase


type implementation_record = {
  version  : SemanticVersion.t;
  requires : package_dependency list;
}


let find (_package_name : package_name) : implementation_record list =
  failwith "TODO: PackageRegistry.find"
