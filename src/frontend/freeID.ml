
module Impl = struct
  type t = {
    mutable level           : Level.t;
    mutable quantifiability : bool;
  }

  let show n _ = string_of_int n
end


include IdScheme.Make(Impl)


let fresh lev q =
  let supp = Impl.{ level = lev; quantifiability = q } in
  generate supp


let get_level fid =
  let supp = get_supplement fid in
  supp.level


let set_level fid lev =
  let supp = get_supplement fid in
  supp.level <- lev


let get_quantifiability fid =
  let supp = get_supplement fid in
  supp.quantifiability


let set_quantifiability fid q =
  let supp = get_supplement fid in
  supp.quantifiability <- q


let pp ppf fid =
  Format.fprintf ppf "%s" (show fid)
