open Types

exception Error of string

type definition_kind   = Data of int | Synonym of int * poly_type | LocalSynonym of module_name * int * poly_type
type defined_type_list = (type_name * definition_kind) list
type constructor_list  = (constructor_name * type_name * poly_type) list

(* public *)
type t = defined_type_list * constructor_list

type fix_mode = InnerMode | OuterMode
type type_argument_mode = StrictMode of untyped_type_argument_cons | FreeMode of (var_name list) ref


(* public *)
let empty = ([], [])


(* public *)
let append_module_name (mdlnm : module_name) (varntnm : type_name) =
  match mdlnm with "" -> varntnm | _  -> mdlnm ^ "." ^ varntnm


(* public *)
let add (varntenv : t) (constrnm : constructor_name) (pty : poly_type) (varntnm : type_name) =
  let (defedtylst, varntenvmain) = varntenv in
  let rec aux varntenvmain constrnm pty varntnm =
    match varntenvmain with
    | []                                   -> (constrnm, varntnm, pty) :: []
    | (c, v, t) :: tail  when c = constrnm -> (constrnm, varntnm, pty) :: tail
    | (c, v, t) :: tail                    -> (c, v, t) :: (aux tail constrnm pty varntnm)
  in
    (defedtylst, aux varntenvmain constrnm pty varntnm)


(* public *)
let rec add_list = List.fold_left (fun ve (c, v, t) -> add ve c v t)

  
let rec find_definition_kind (defedtylst : defined_type_list) (tynm : type_name) =
  match defedtylst with
  | []                             -> raise Not_found
  | (tn, ts) ::  tl when tn = tynm -> ts
  | _ :: tl                        -> find_definition_kind tl tynm


let rec is_defined_type_argument (tyargcons : untyped_type_argument_cons) (tyargnm : var_name) =
  match tyargcons with
  | UTEndOfTypeArgument                 -> false
  | UTTypeArgumentCons(_, nm, tailcons) ->
      if nm = tyargnm then true else is_defined_type_argument tailcons tyargnm


let report_illegal_type_argument_length (rng : Range.t) (tynm : type_name) (len_expected : int) (len : int) =
  raise(Error("at " ^ (Range.to_string rng) ^ ":\n" ^
    "    '" ^ tynm ^ "' is expected to have " ^ (string_of_int len_expected) ^ " type argument(s),\n" ^
    "    but it has " ^ (string_of_int len) ^ " type argument(s) here"))


let rec fix_manual_type_general (mode : fix_mode) (varntenv : t) (tyargmode : type_argument_mode) (tystr : mono_type) =
  let (defedtylst, varntenvmain) = varntenv in
  let (rng, tymain) = tystr in
  let iter = fix_manual_type_general mode varntenv tyargmode in
  let error = report_illegal_type_argument_length rng in
  let tymainnew =
    match tymain with

    | FuncType(tydom, tycod)           -> FuncType(iter tydom, iter tycod)
    | ProductType(tylist)              -> ProductType(List.map iter tylist)
    | RecordType(asc)                  -> RecordType(Assoc.map_value iter asc)

    | VariantType([], "int")           -> IntType
    | VariantType(tyarglist, "int")    -> error "int" 0 (List.length tyarglist)
    | VariantType([], "string")        -> StringType
    | VariantType(tyarglist, "string") -> error "string" 0 (List.length tyarglist)
    | VariantType([], "bool")          -> BoolType
    | VariantType(tyarglist, "bool")   -> error "bool" 0 (List.length tyarglist)
    | VariantType([], "unit")          -> UnitType
    | VariantType(tyarglist, "unit")   -> error "unit" 0 (List.length tyarglist)

    | VariantType(tyarg :: [], "list") -> ListType(tyarg)
    | VariantType(tyarglist, "list")   -> error "list" 1 (List.length tyarglist)
    | VariantType(tyarg :: [], "ref")  -> RefType(tyarg)
    | VariantType(tyarglist, "ref")    -> error "ref" 1 (List.length tyarglist)
    | VariantType(tyarglist, tynm) ->
        begin
          try
            match find_definition_kind defedtylst tynm with
            | Data(argnum) ->
                let len = List.length tyarglist in
                  if argnum <> len then error tynm argnum len else
                    VariantType(List.map iter tyarglist, tynm)

            | Synonym(argnum, pty) ->
                let len = List.length tyarglist in
                  if argnum <> len then error tynm argnum len else
                    TypeSynonym(List.map iter tyarglist, tynm, pty)

            | LocalSynonym(mdlnm, argnum, pty) ->
                let len = List.length tyarglist in
                  if argnum <> len then error tynm argnum len else
                    match mode with
                    | InnerMode -> TypeSynonym(List.map iter tyarglist, tynm, pty)
                    | OuterMode -> VariantType(List.map iter tyarglist, append_module_name mdlnm tynm)
          with
          | Not_found -> raise (Error("at " ^ (Range.to_string rng) ^ ":\n" ^ "    undefined type '" ^ tynm ^ "'"))
        end

    | TypeArgument(tyargnm)        ->
          begin
            match tyargmode with
            | StrictMode(tyargcons) ->
                if is_defined_type_argument tyargcons tyargnm then
                  TypeArgument(tyargnm)
                else
                  raise (Error("at " ^ (Range.to_string rng) ^ ":\n" ^ "    undefined type argument '" ^ tyargnm ^ "'"))

            | FreeMode(reftyarglst) ->
                begin
                  ( if List.mem tyargnm (!reftyarglst) then () else reftyarglst := tyargnm :: (!reftyarglst) ) ;
                  TypeArgument(tyargnm)
                end
          end

    | other                             ->
        begin
          print_endline ("OTHER: " ^ (Display.string_of_mono_type_basic (rng, other))) ;
          assert false
        end
  in
    (rng, tymainnew)


let rec make_type_argument_numbered_mono (var_id : Tyvarid.t) (tyargnm : var_name) ((rng, tymain) : mono_type) =
  let iter = make_type_argument_numbered_mono var_id tyargnm in
  let tymainnew =
    match tymain with
    | TypeArgument(nm)   when nm = tyargnm -> TypeVariable(var_id)
    | FuncType(tydom, tycod)               -> FuncType(iter tydom, iter tycod)
    | ListType(tycont)                     -> ListType(iter tycont)
    | RefType(tycont)                      -> RefType(iter tycont)
    | ProductType(tylist)                  -> ProductType(List.map iter tylist)
(*
    | ForallType(tvid, kdstr, tycont)      -> ForallType(tvid, kdstr, iter tycont)
        (* maybe contains bugs, when tvid = -var_id *)
*)
    | VariantType(tylist, varntnm)         -> VariantType(List.map iter tylist, varntnm)
    | TypeSynonym(tylist, tysynnm, tycont) -> TypeSynonym(List.map iter tylist, tysynnm, tycont)
    | RecordType(asc)                      -> RecordType(Assoc.map_value iter asc)
    | other                                -> other
  in
    (rng, tymainnew)


let make_type_argument_numbered (var_id : Tyvarid.t) (tyargnm : var_name) (pty : poly_type) =
  let rec aux_poly pty =
    match pty with
    | Mono(ty)                 -> Mono(make_type_argument_numbered_mono var_id tyargnm ty)
    | Forall(tvid, kd, ptysub) -> Forall(tvid, kd, aux_poly ptysub)
  in
    aux_poly pty


let fix_manual_type (varntenv : t) tyargcons (tystr : mono_type) =
  fix_manual_type_general InnerMode varntenv (StrictMode(tyargcons)) tystr


let free_type_argument_list : (var_name list) ref = ref []

let rec make_type_argument_into_type_variable qtfbl (tyarglist : var_name list) (ty : mono_type) =
  match tyarglist with
  | []                   -> ty
  | tyargnm :: tyargtail ->
      let ntv = Tyvarid.fresh qtfbl in
      let tynew = make_type_argument_numbered_mono ntv tyargnm ty in
        make_type_argument_into_type_variable qtfbl tyargtail tynew


(* public *)
let fix_manual_type_for_inner_and_outer qtfbl (varntenv : t) (tystr : mono_type) =
  free_type_argument_list := [] ;
  let tystrin  = fix_manual_type_general InnerMode varntenv (FreeMode(free_type_argument_list)) tystr in
  let tystrout = fix_manual_type_general OuterMode varntenv (FreeMode(free_type_argument_list)) tystr in
    let tystrin_result = make_type_argument_into_type_variable qtfbl (!free_type_argument_list) tystrin in
    let tystrout_result = make_type_argument_into_type_variable qtfbl (!free_type_argument_list) tystrout in
      (tystrin_result, tystrout_result)


let rec make_type_argument_quantified (tyargcons : untyped_type_argument_cons) (ty : mono_type) =
  let rec aux tyargcons pty =
    match tyargcons with
    | UTEndOfTypeArgument                        -> pty
    | UTTypeArgumentCons(rng, tyargnm, tailcons) ->
        let tvidqtf = Tyvarid.fresh Tyvarid.Quantifiable in
        let ptynew = Forall(tvidqtf, UniversalKind (* temporary *), make_type_argument_numbered tvidqtf tyargnm pty) in
          aux tailcons ptynew
  in
    aux tyargcons (Mono(ty))

let rec type_argument_length tyargcons =
  match tyargcons with
  | UTEndOfTypeArgument                -> 0
  | UTTypeArgumentCons(_, _, tailcons) -> 1 + (type_argument_length tailcons)


let register_variant (varntenv : t) (len : int) (tynm : type_name) =
  let (defedtypelist, varntenvmain) = varntenv in
    ((tynm, Data(len)) :: defedtypelist, varntenvmain)


let register_variant_list = List.fold_left (fun ve (l, t) -> register_variant ve l t)


let add_synonym (scope : scope) (varntenv : t)
                  (tyargcons : untyped_type_argument_cons) (tysynnm : type_name) (tystr : mono_type) =
  let (defedtypelist, varntenvmain) = varntenv in
  let len = type_argument_length tyargcons in
  let defkind =
    match scope with
    | GlobalScope ->
        let tystr_new    = fix_manual_type varntenv tyargcons tystr in
        let pty = make_type_argument_quantified tyargcons tystr_new in
          Synonym(len, pty)

    | LocalScope(mdlnm) ->
        let tystr_new    = fix_manual_type varntenv tyargcons tystr in
        let pty = make_type_argument_quantified tyargcons tystr_new in
          LocalSynonym(mdlnm, len, pty)
  in
    ((tysynnm, defkind) :: defedtypelist, varntenvmain)


(* public *)
let rec apply_to_type_synonym (tyarglist : mono_type list) (pty : poly_type) =
  match (tyarglist, pty) with
  | (tyarghd :: tyargtl, Forall(tvid, kdstr, ptysub)) ->
      let ptynew = Typeenv.replace_id_poly [(tvid, tyarghd)] pty in
        apply_to_type_synonym tyargtl ptynew
  | ([], Forall(_, _, _))                             -> assert false
  | ([], Mono(ty))                                    -> ty
  | (_ :: _, Mono(_))                                 -> assert false


let rec add_variant_cons (mdlnm : module_name) (varntenv : t)
                           (tyargcons : untyped_type_argument_cons) (varntnm : type_name) (utvc : untyped_variant_cons) =

  let rec aux mdlnm varntenv tyargcons varntnm utvc =
    let (rng, utvcmain) = utvc in
      match utvcmain with
      | UTEndOfVariant                           -> varntenv
      | UTVariantCons(constrnm, ty, tailcons) ->
          let tynew = fix_manual_type varntenv tyargcons ty in
          let pty = make_type_argument_quantified tyargcons tynew in
          let varntenvnew = add varntenv constrnm pty (append_module_name mdlnm varntnm) in
            aux mdlnm varntenvnew tyargcons varntnm tailcons
  in
  let mdlvarntnm = append_module_name mdlnm varntnm in
  let tyarglen   = type_argument_length tyargcons in
    aux mdlnm (register_variant varntenv tyarglen mdlvarntnm) tyargcons varntnm utvc


(* public *)
let rec add_mutual_cons (scope : scope) (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
  let varntenv_mem = memo_variant_name "" varntenv mutvarntcons in
  let varntenv_syn = read_synonym_spec scope varntenv_mem mutvarntcons in
  let varntenv_fin = read_variant_spec varntenv_syn mutvarntcons in
    varntenv_fin

and read_synonym_spec (scope : scope) (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
    match mutvarntcons with
    | UTEndOfMutualVariant                                     -> varntenv
    | UTMutualVariantCons(_, _, _, tailcons)                   -> read_synonym_spec scope varntenv tailcons
    | UTMutualSynonymCons(tyargcons, tysynnm, tystr, tailcons) ->
        let varntenv_new = add_synonym scope varntenv tyargcons tysynnm tystr in
          read_synonym_spec scope varntenv_new tailcons

and read_variant_spec (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
    match mutvarntcons with
    | UTEndOfMutualVariant                                     -> varntenv
    | UTMutualVariantCons(tyargcons, varntnm, utvc, tailcons)  ->
        let varntenv_new = add_variant_cons "" varntenv tyargcons varntnm utvc in
          read_variant_spec varntenv_new tailcons
    | UTMutualSynonymCons(_, _, _, tailcons)                   -> read_variant_spec varntenv tailcons


(* public *)
and add_mutual_cons_hidden (mdlnm : module_name) (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
  memo_all_name mdlnm varntenv mutvarntcons


and memo_variant_name (mdlnm : module_name) (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
  match mutvarntcons with
  | UTEndOfMutualVariant                                 -> varntenv
  | UTMutualVariantCons(tyargcons, varntnm, _, tailcons) ->
      let mdlvarntnm = append_module_name mdlnm varntnm in
      let tyarglen   = type_argument_length tyargcons in
      let varntenv_new = register_variant varntenv tyarglen mdlvarntnm in
        memo_variant_name mdlnm varntenv_new tailcons
  | UTMutualSynonymCons(tyargcons, tysynnm, _, tailcons) -> memo_variant_name mdlnm varntenv tailcons


and memo_all_name (mdlnm : module_name) (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
  match mutvarntcons with
  | UTEndOfMutualVariant                                 -> varntenv
  | UTMutualVariantCons(tyargcons, varntnm, _, tailcons) ->
      let mdlvarntnm = append_module_name mdlnm varntnm in
      let tyarglen   = type_argument_length tyargcons in
      let varntenv_new = register_variant varntenv tyarglen mdlvarntnm in
        memo_all_name mdlnm varntenv_new tailcons
  | UTMutualSynonymCons(tyargcons, tysynnm, _, tailcons) ->
      let mdltysynnm = append_module_name mdlnm tysynnm in
      let tyarglen   = type_argument_length tyargcons in
      let varntenv_new = register_variant varntenv tyarglen mdltysynnm in
        memo_all_name mdlnm varntenv_new tailcons


(* public *)
let rec find (varntenv : t) (constrnm : constructor_name) =
  let (_, varntenvmain) = varntenv in
    let rec f varntenvmain constrnm =
      match varntenvmain with
      | []                -> raise Not_found
      | (c, v, t) :: tail -> if c = constrnm then (v, t) else f tail constrnm
    in
      f varntenvmain constrnm
