type t =
  | TyCon0 of { cname: string; sname: string }
  (* cname = Caml name, sname = SATySFi name *)
  | TyCon1 of { cname: string; sname: string; t: t }
  | Product of { ts: t list }
  | Fun of { dom: t; cod: t }
  | TyVar of { name: string }
  | Forall of { var: string; t: t }

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
    | TyCon0 {cname; sname} ->
      tycon0 ~cname ~sname ppf
    | TyCon1 {cname; sname; t} ->
      maybe_paren ~outer_prec tapply_precedence
        (fun () ->
           tycon1 ~cname ~sname ppf (to_s @@ tapply_precedence + 1) t)
    | Product {ts} ->
      maybe_paren ~outer_prec prod_precedence
        (fun () ->
           Format.fprintf ppf "%s" prod_open;
           ts |> List.iteri (fun i t ->
               Format.fprintf ppf "%s%a"
                 (if i > 0 then prod_sep else "")
                 (to_s @@ prod_precedence + 1)
                 t);
           Format.fprintf ppf "%s" prod_close)
    | Fun {dom; cod} ->
      maybe_paren ~outer_prec fun_precedence
        (fun () ->
           (Format.fprintf ppf "%a %s %a"
              (to_s @@ fun_precedence + 1)
              dom
              arrow
              (to_s fun_precedence)
              cod))
    | TyVar {name} -> tyvar ~name ppf
    | Forall {var; t} -> forall ~var ppf (to_s outer_prec) t
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
    Format.fprintf ppf "(let %s = ~@@ (PolyBound(BoundID.fresh ())) in (%a))"
      var pp body
  in
  Format.asprintf "~%% (%a)"
    (to_s
       ~arrow:"@->"
       ~prod_open:"tPROD [" ~prod_sep:"; " ~prod_close:"]"
       ~tycon0 ~tycon1 ~tyvar ~forall) t

let tPROD ts = Product {ts}

let (@->) dom cod = Fun {dom; cod}

let forall var f =
  Forall {var; t = f (TyVar {name = var})}

let tycon0 cname sname   = TyCon0 {cname; sname}
let tycon1 cname sname t = TyCon1 {cname; sname; t}

let tU          = tycon0 "tU"          "unit"
let tI          = tycon0 "tI"          "int"
let tFL         = tycon0 "tFL"         "float"
let tB          = tycon0 "tB"          "bool"
let tLN         = tycon0 "tLN"         "length"
let tS          = tycon0 "tS"          "string"
let tIT         = tycon0 "tIT"         "inline-text"
let tBT         = tycon0 "tBT"         "block-text"
let tMT         = tycon0 "tMT"         "math-text"
let tIB         = tycon0 "tIB"         "inline-boxes"
let tBB         = tycon0 "tBB"         "block-boxes"
let tMB         = tycon0 "tMB"         "math-boxes"
let tCTX        = tycon0 "tCTX"        "context"
let tTCTX       = tycon0 "tTCTX"       "text-info"
let tPATH       = tycon0 "tPATH"       "path"
let tPRP        = tycon0 "tPRP"        "pre-path"
let tDOC        = tycon0 "tDOC"        "document"
let tGR         = tycon0 "tGR"         "graphics"
let tIMG        = tycon0 "tIMG"        "image"
let tRE         = tycon0 "tRE"         "regexp"
let tIPOS       = tycon0 "tIPOS"       "input-position"
let tITMZ       = tycon0 "tITMZ"       "itemize"
let tSCR        = tycon0 "tSCR"        "script"
let tLANG       = tycon0 "tLANG"       "language"
let tCLR        = tycon0 "tCLR"        "color"
let tPG         = tycon0 "tPG"         "page"
let tMATHCLS    = tycon0 "tMATHCLS"    "math-class"
let tMCCLS      = tycon0 "tMCCLS"      "math-char-class"
let tCELL       = tycon0 "tCELL"       "cell"
let tMCSTY      = tycon0 "tMCSTY"      "math-char-style"
let tPAREN      = tycon0 "tPAREN"      "paren"
let tPBINFO     = tycon0 "tPBINFO"     "page-break-info"
let tPAGECONT   = tycon0 "tPAGECONT"   "page-content-scheme"
let tPAGEPARTS  = tycon0 "tPAGEPARTS"  "page-parts"
let tPT         = tycon0 "tPT"         "point"
let tPADS       = tycon0 "tPADS"       "paddings"
let tDECOSET    = tycon0 "tDECOSET"    "deco-set"
let tFONTKEY    = tycon0 "tFONTKEY"    "font"
let tFONTWR     = tycon0 "tFONTWR"     "font-with-ratio"
let tDECO       = tycon0 "tDECO"       "deco"
let tIGR        = tycon0 "tIGR"        "inline-graphics"
let tIGRO       = tycon0 "tIGRO"       "inline-graphics-outer"
let tDOCINFODIC = tycon0 "tDOCINFODIC" "document-information-dictionary"
let tL          = tycon1 "tL"          "list"
let tR          = tycon1 "tR"          "ref"
let tOPT        = tycon1 "tOPT"        "option"
let tCODE       = tycon1 "tCODE"       "code"
let tICMD       = tycon1 "tICMD"       "cmd"

let tPCINFO     = tPBINFO
let tRULESF     = tL tLN @-> tL tLN @-> tL tGR
let tPAGECONTF  = tPBINFO @-> tPAGECONT
let tPAGEPARTSF = tPCINFO @-> tPAGEPARTS
let tDASH       = tPROD [tLN; tLN; tLN]

let mckf = tLN @-> tLN @-> tLN
