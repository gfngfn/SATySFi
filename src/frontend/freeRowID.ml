
open SyntaxBase


module Impl = struct
  type t = {
    mutable level     : Level.t;
    mutable label_set : LabelSet.t;
  }

  let show n _ = string_of_int n
end


include IdScheme.Make(Impl)


let fresh (lev : Level.t) (labset : LabelSet.t) : t =
  let supp = Impl.{ level = lev; label_set = labset } in
  generate supp


let get_level (frid : t) : Level.t =
  let supp = get_supplement frid in
  supp.level


let set_level (frid : t) (lev : Level.t) : unit =
  let supp = get_supplement frid in
  supp.level <- lev


let get_label_set (frid : t) : LabelSet.t =
  let supp = get_supplement frid in
  supp.label_set


let set_label_set (frid : t) (labset : LabelSet.t) : unit =
  let supp = get_supplement frid in
  supp.label_set <- labset


let pp ppf frid =
  Format.fprintf ppf "%s" (show frid)
