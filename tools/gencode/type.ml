type t =
  | TyCon0 of string * string
  | TyCon1 of string * string * t
  | Product of t list
  | Fun of t * t
  | TyVar of string
  | Forall of string * t

let none_precedence = 0
let fun_precedence = 10
let prod_precedence = 20
let tapply_precedence = 30

let to_s ~arrow ~prod_open ~prod_sep ~prod_close ~tycon0 ~tycon1 ~tyvar ~forall ppf t =
  let maybe_paren ~outer_prec self f =
    if outer_prec > self then begin
      Format.fprintf ppf "("
    end;
    f ();
    if outer_prec > self then begin
      Format.fprintf ppf ")"
    end
  in
  let rec to_s outer_prec ppf = function
    | TyCon0 (cname, sname) ->
      tycon0 ~cname ~sname ppf
    | TyCon1 (cname, sname, t) ->
      maybe_paren ~outer_prec tapply_precedence
        (fun () ->
           tycon1 ~cname ~sname ppf (to_s @@ tapply_precedence + 1) t)
    | Product ts ->
      maybe_paren ~outer_prec prod_precedence
        (fun () ->
           Format.fprintf ppf "%s" prod_open;
           ts |> List.iteri (fun i t ->
               Format.fprintf ppf "%s%a"
                 (if i > 0 then prod_sep else "")
                 (to_s @@ prod_precedence + 1)
                 t);
           Format.fprintf ppf "%s" prod_close)
    | Fun (dom, cod) ->
      maybe_paren ~outer_prec fun_precedence
        (fun () ->
           (Format.fprintf ppf "%a %s %a"
              (to_s @@ fun_precedence + 1)
              dom
              arrow
              (to_s fun_precedence)
              cod))
    | TyVar name -> tyvar ~name ppf
    | Forall (var, t) -> forall ~var ppf (to_s outer_prec) t
  in
  to_s none_precedence ppf t

let to_string t =
  let tycon0 ~cname:_ ~sname ppf = Format.fprintf ppf "%s" sname in
  let tycon1 ~cname:_ ~sname ppf pp t =
    Format.fprintf ppf "%a %s" pp t sname
  in
  let tyvar ~name ppf =
    Format.fprintf ppf "'%s" name
  in
  let forall ~var:_ ppf pp t = pp ppf t in
  Format.asprintf "%a"
    (to_s
       ~arrow:"->"
       ~prod_open:"" ~prod_sep:" * " ~prod_close:""
       ~tycon0 ~tycon1 ~tyvar ~forall) t

let to_code t =
  let tycon0 ~cname ~sname:_ ppf = Format.fprintf ppf "%s" cname in
  let tycon1 ~cname ~sname:_ ppf pp t =
    Format.fprintf ppf "%s %a" cname pp t
  in
  let tyvar ~name ppf =
    Format.fprintf ppf "%s" name
  in
  let forall ~var ppf pp body =
    Format.fprintf ppf "(let %s = ~@@ (PolyBound(BoundID.fresh UniversalKind ())) in (%a))"
      var pp body
  in
  Format.asprintf "~%% (%a)"
    (to_s
       ~arrow:"@->"
       ~prod_open:"tPROD [" ~prod_sep:"; " ~prod_close:"]"
       ~tycon0 ~tycon1 ~tyvar ~forall) t

let (@->) t1 t2 = Fun (t1, t2)

let forall name f =
  Forall (name, f (TyVar name))

let tU           = TyCon0 ("tU", "unit")
let tI           = TyCon0 ("tI", "int")
let tFL          = TyCon0 ("tFL", "float")
let tB           = TyCon0 ("tB", "bool")
let tLN          = TyCon0 ("tLN", "length")
let tS           = TyCon0 ("tS", "string")
let tIT          = TyCon0 ("tIT", "itext")
let tBT          = TyCon0 ("tBT", "btext")
let tIB          = TyCon0 ("tIB", "iboxes")
let tBB          = TyCon0 ("tBB", "bboxes")
let tCTX         = TyCon0 ("tCTX", "context")
let tTCTX        = TyCon0 ("tTCTX", "text-info")
let tPATH        = TyCon0 ("tPATH", "path")
let tPRP         = TyCon0 ("tPRP", "pre-path")
let tDOC         = TyCon0 ("tDOC", "document")
let tMATH        = TyCon0 ("tMATH", "math")
let tGR          = TyCon0 ("tGR", "graphics")
let tIMG         = TyCon0 ("tIMG", "image")
let tRE          = TyCon0 ("tRE", "regexp")
let tIPOS        = TyCon0 ("tIPOS", "input-position")
let tITMZ        = TyCon0 ("tITMZ", "itemize")
let tSCR         = TyCon0 ("tSCR", "script")
let tLANG        = TyCon0 ("tLANG", "language")
let tCLR         = TyCon0 ("tCLR", "color")
let tPG          = TyCon0 ("tPG", "page")
let tMATHCLS     = TyCon0 ("tMATHCLS", "mathcls")
let tMCCLS       = TyCon0 ("tMCCLS", "mccls")
let tCELL        = TyCon0 ("tCELL", "cell")
let tMCSTY       = TyCon0 ("tMCSTY", "math-char-style")
let tPAREN       = TyCon0 ("tPAREN", "paren")
let tPBINFO      = TyCon0 ("tPBINFO", "page-break-info")
let tPAGECONT    = TyCon0 ("tPAGECONT", "page-content-scheme")
let tPAGEPARTS   = TyCon0 ("tPAGEPARTS", "page-parts")
let tPT          = TyCon0 ("tPT", "point")
let tPADS        = TyCon0 ("tPADS", "paddings")
let tDECOSET     = TyCon0 ("tDECOSET", "deco-set")
let tFONT        = TyCon0 ("tFONT", "font")
let tDECO        = TyCon0 ("tDECO", "deco")
let tIGR         = TyCon0 ("tIGR", "graf")
let tIGRO        = TyCon0 ("tIGRO", "igrafo")
let tDOCINFODIC  = TyCon0 ("tDOCINFODIC", "document-information-dictionary")

let tPROD ts = Product ts

let tL t    = TyCon1 ("tL", "list", t)
let tR t    = TyCon1 ("tR", "ref", t)
let tOPT t  = TyCon1 ("tOPT", "option", t)
let tCODE t = TyCon1 ("tCODE", "code", t)
let tICMD t = TyCon1 ("tICMD", "cmd", t)

let tPCINFO      = tPBINFO
let tRULESF      = tL tLN @-> tL tLN @-> tL tGR
let tPAGECONTF   = tPBINFO @-> tPAGECONT
let tPAGEPARTSF  = tPCINFO @-> tPAGEPARTS
let tDASH        = tPROD [tLN; tLN; tLN; tLN]

let mckf = tLN @-> tLN @-> tLN
