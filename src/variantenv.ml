open Types

exception IllegalNumberOfTypeArguments of Range.t * type_name * int * int
exception UndefinedTypeName            of Range.t * type_name
exception UndefinedTypeArgument        of Range.t * var_name

type definition        = Data of int | Synonym of int * poly_type
type defined_type_list = (type_name * Typeid.t * definition) list
type constructor_list  = (constructor_name * Typeid.t * poly_type) list

(* PUBLIC *)
type t = defined_type_list * constructor_list

type type_argument_mode =
  | StrictMode of untyped_type_argument_cons  (* case where all type arguments should be declared; e.g. for type definitions *)
  | FreeMode   of (var_name list) ref         (* case where type arguments do not need to be declared; e.g. for type annotations *)


(* PUBLIC *)
let empty = ([], [])


(* PUBLIC *)
let append_module_name (mdlnm : module_name) (varntnm : type_name) =
  match mdlnm with "" -> varntnm | _  -> mdlnm ^ "." ^ varntnm


let find_definition (defedtylst : defined_type_list) (tynm : type_name) =
  let rec aux lst =
    match lst with
    | []                                            -> raise Not_found
    | (tynmx, tyid, dfn) :: tail  when tynmx = tynm -> (tyid, dfn)
    | _ :: tail                                     -> aux tail
  in
    aux defedtylst


(* PUBLIC *)
let find_type_id ((defedtylst, _) : t) (tynm : type_name) =
  let (tyid, _) = find_definition defedtylst tynm in tyid


(* PUBLIC *)
let find_type_name ((defedtylst, _) : t) (tyid : Typeid.t) =
  let rec aux lst =
    match lst with
    | []                                                  -> raise Not_found
    | (tynm, tyidx, _) :: tail  when Typeid.eq tyidx tyid -> tynm
    | _ :: tail                                           -> aux tail
  in
    aux defedtylst


(* public *)
let add ((defedtylst, varntenvmain) : t) (constrnm : constructor_name) (pty : poly_type) (varntnm : type_name) =
  let (tyid, _) = find_definition defedtylst varntnm in
  let rec aux accrev lst pty =
    match lst with
    | []                                   -> (constrnm, tyid, pty) :: varntenvmain
    | (c, v, t) :: tail  when c = constrnm -> List.rev_append accrev ((constrnm, tyid, pty) :: tail)
    | (c, v, t) :: tail                    -> aux ((c, v, t) :: accrev) tail pty
  in
    (defedtylst, aux [] varntenvmain pty)


(* public *)
let add_list = List.fold_left (fun ve (c, v, t) -> add ve c v t)


let rec is_defined_type_argument (tyargcons : untyped_type_argument_cons) (tyargnm : var_name) =
  match tyargcons with
  | UTEndOfTypeArgument                                    -> false
  | UTTypeArgumentCons(_, nm, tailcons)  when nm = tyargnm -> true
  | UTTypeArgumentCons(_, _, tailcons)                     -> is_defined_type_argument tailcons tyargnm


let rec fix_manual_type_general (varntenv : t) (tyargmode : type_argument_mode) (mnty : manual_type) =
  let (defedtylst, varntenvmain) = varntenv in
  let (rng, mntymain) = mnty in
  let iter = fix_manual_type_general varntenv tyargmode in
  let error tynm lenexp lenerr = raise (IllegalNumberOfTypeArguments(rng, tynm, lenexp, lenerr)) in
  let tymainnew =
    match mntymain with

    | MFuncType(mntydom, mntycod)    -> FuncType(iter mntydom, iter mntycod)
    | MProductType(mntylist)         -> ProductType(List.map iter mntylist)
    | MRecordType(mnasc)             -> RecordType(Assoc.map_value iter mnasc)

    | MTypeName([], "int")             -> IntType
    | MTypeName(mntyarglist, "int")    -> error "int" 0 (List.length mntyarglist)
    | MTypeName([], "string")          -> StringType
    | MTypeName(mntyarglist, "string") -> error "string" 0 (List.length mntyarglist)
    | MTypeName([], "bool")            -> BoolType
    | MTypeName(mntyarglist, "bool")   -> error "bool" 0 (List.length mntyarglist)
    | MTypeName([], "unit")            -> UnitType
    | MTypeName(mntyarglist, "unit")   -> error "unit" 0 (List.length mntyarglist)

    | MTypeName(mntyarg :: [], "list") -> ListType(iter mntyarg)
    | MTypeName(mntyarglist, "list")   -> error "list" 1 (List.length mntyarglist)
    | MTypeName(mntyarg :: [], "ref")  -> RefType(iter mntyarg)
    | MTypeName(mntyarglist, "ref")    -> error "ref" 1 (List.length mntyarglist)
    | MTypeName(mntyarglist, tynm) ->
        let len = List.length mntyarglist in
        begin
          try
            match find_definition defedtylst tynm with
            | (tyid, Data(argnum)) ->
                if argnum <> len then error tynm argnum len else
                  VariantType(List.map iter mntyarglist, tyid)

            | (tyid, Synonym(argnum, pty)) ->
                if argnum <> len then error tynm argnum len else
                  TypeSynonym(List.map iter mntyarglist, tyid, pty)
          with
          | Not_found -> raise (UndefinedTypeName(rng, tynm))
        end

    | MTypeParam(tyargnm) ->
          begin
            match tyargmode with
            | StrictMode(tyargcons) ->
                if is_defined_type_argument tyargcons tyargnm then
                  TypeArgument(tyargnm)
                else
                  raise (UndefinedTypeArgument(rng, tyargnm))

            | FreeMode(reftyarglst) ->
                begin
                  ( if List.mem tyargnm (!reftyarglst) then () else reftyarglst := tyargnm :: (!reftyarglst) ) ;
                  TypeArgument(tyargnm)
                end
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


let fix_manual_type (varntenv : t) tyargcons (mnty : manual_type) =
  fix_manual_type_general varntenv (StrictMode(tyargcons)) mnty


let free_type_argument_list : (var_name list) ref = ref []

let rec make_type_argument_into_type_variable qtfbl (tyarglist : var_name list) (ty : mono_type) =
  match tyarglist with
  | []                   -> ty
  | tyargnm :: tyargtail ->
      let ntv = Tyvarid.fresh qtfbl in
      let tynew = make_type_argument_numbered_mono ntv tyargnm ty in
        make_type_argument_into_type_variable qtfbl tyargtail tynew


(* public *)
let fix_manual_type_for_inner_and_outer qtfbl (varntenv : t) (mnty : manual_type) =
  free_type_argument_list := [] ;
  let tyin  = fix_manual_type_general varntenv (FreeMode(free_type_argument_list)) mnty in
  let tyout = fix_manual_type_general varntenv (FreeMode(free_type_argument_list)) mnty in
    let tyin_result = make_type_argument_into_type_variable qtfbl (!free_type_argument_list) tyin in
    let tyout_result = make_type_argument_into_type_variable qtfbl (!free_type_argument_list) tyout in
      (tyin_result, tyout_result)


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
  let tyid = Typeid.fresh () in
    ((tynm, tyid, Data(len)) :: defedtypelist, varntenvmain)


let register_variant_list = List.fold_left (fun ve (l, t) -> register_variant ve l t)


let add_synonym (varntenv : t)
                  (tyargcons : untyped_type_argument_cons) (tysynnm : type_name) (mnty : manual_type) =
  let (defedtypelist, varntenvmain) = varntenv in
  let len = type_argument_length tyargcons in
  let defkind =
    let tynew = fix_manual_type varntenv tyargcons mnty in
    let pty = make_type_argument_quantified tyargcons tynew in
      Synonym(len, pty)
  in
  let tyid = Typeid.fresh () in
    ((tysynnm, tyid, defkind) :: defedtypelist, varntenvmain)


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
      | UTEndOfVariant                          -> varntenv
      | UTVariantCons(constrnm, mnty, tailcons) ->
          let tynew = fix_manual_type varntenv tyargcons mnty in
          let pty = make_type_argument_quantified tyargcons tynew in
          let varntenvnew = add varntenv constrnm pty (append_module_name mdlnm varntnm) in
            aux mdlnm varntenvnew tyargcons varntnm tailcons
  in
  let mdlvarntnm = append_module_name mdlnm varntnm in
  let tyarglen   = type_argument_length tyargcons in
    aux mdlnm (register_variant varntenv tyarglen mdlvarntnm) tyargcons varntnm utvc


(* public *)
let rec add_mutual_cons (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
  let varntenv_mem = memo_variant_name "" varntenv mutvarntcons in
  let varntenv_syn = read_synonym_spec varntenv_mem mutvarntcons in
  let varntenv_fin = read_variant_spec varntenv_syn mutvarntcons in
    varntenv_fin

and read_synonym_spec (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
    match mutvarntcons with
    | UTEndOfMutualVariant                                     -> varntenv
    | UTMutualVariantCons(_, _, _, tailcons)                   -> read_synonym_spec varntenv tailcons
    | UTMutualSynonymCons(tyargcons, tysynnm, mnty, tailcons) ->
        let varntenv_new = add_synonym varntenv tyargcons tysynnm mnty in
          read_synonym_spec varntenv_new tailcons

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
