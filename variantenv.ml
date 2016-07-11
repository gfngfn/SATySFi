open Types

exception Error of string

type definition_kind   = Data of int | Synonym of int * type_struct | LocalSynonym of module_name * int * type_struct
type defined_type_list = (type_name * definition_kind) list
type constructor_list  = (constructor_name * type_name * type_struct) list

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
let add (varntenv : t) (constrnm : constructor_name) (tystr : type_struct) (varntnm : type_name) =
  let (defedtylst, varntenvmain) = varntenv in
  let rec iter varntenvmain constrnm tystr varntnm =
    match varntenvmain with
    | []                -> (constrnm, varntnm, tystr) :: []
    | (c, v, t) :: tail ->
        if c = constrnm then
          (constrnm, varntnm, tystr) :: tail
        else
          (c, v, t) :: (iter tail constrnm tystr varntnm)
  in
    (defedtylst, iter varntenvmain constrnm tystr varntnm)


(* public *)
let rec add_list (varntenv : t) (lst : (constructor_name * type_struct * type_name) list) =
  match lst with
  | []                -> varntenv
  | (c, v, t) :: tail -> add_list (add varntenv c v t) tail


let rec find_definition_kind (defedtylst : defined_type_list) (tynm : type_name) =
  match defedtylst with
  | []                            -> raise Not_found
  | (tn, ts) :: tl when tn = tynm -> ts
  | _ :: tl                       -> find_definition_kind tl tynm


let rec is_defined_type_argument (tyargcons : untyped_type_argument_cons) (tyargnm : var_name) =
  match tyargcons with
  | UTEndOfTypeArgument                 -> false
  | UTTypeArgumentCons(_, nm, tailcons) ->
      if nm = tyargnm then true else is_defined_type_argument tailcons tyargnm


let report_illegal_type_argument_length (rng : Range.t) (tynm : type_name) (len_expected : int) (len : int) =
  raise(Error("at " ^ (Range.to_string rng) ^ ":\n" ^
    "    '" ^ tynm ^ "' is expected to have " ^ (string_of_int len_expected) ^ " type argument(s),\n" ^
    "    but it has " ^ (string_of_int len) ^ " type argument(s) here"))


let rec fix_manual_type_general (mode : fix_mode) (varntenv : t) (tyargmode : type_argument_mode) (tystr : type_struct) =
  let (defedtylst, varntenvmain) = varntenv in
  let (rng, tymain) = tystr in
  let iter = fix_manual_type_general mode varntenv tyargmode in
  let tymainnew =
    match tymain with

    | FuncType(tydom, tycod)           -> FuncType(iter tydom, iter tycod)
    | ProductType(tylist)              -> ProductType(List.map iter tylist)

    | VariantType([], "int")           -> IntType
    | VariantType(tyarglist, "int")    -> report_illegal_type_argument_length rng "int" 0 (List.length tyarglist)
    | VariantType([], "string")        -> StringType
    | VariantType(tyarglist, "string") -> report_illegal_type_argument_length rng "string" 0 (List.length tyarglist)
    | VariantType([], "bool")          -> BoolType
    | VariantType(tyarglist, "bool")   -> report_illegal_type_argument_length rng "bool" 0 (List.length tyarglist)
    | VariantType([], "unit")          -> UnitType
    | VariantType(tyarglist, "unit")   -> report_illegal_type_argument_length rng "unit" 0 (List.length tyarglist)

    | VariantType(tyarg :: [], "list") -> ListType(tyarg)
    | VariantType(tyarglist, "list")   -> report_illegal_type_argument_length rng "list" 1 (List.length tyarglist)
    | VariantType(tyarg :: [], "ref")  -> RefType(tyarg)
    | VariantType(tyarglist, "ref")    -> report_illegal_type_argument_length rng "ref" 1 (List.length tyarglist)
    | VariantType(tyarglist, tynm) ->
        begin
          try
            match find_definition_kind defedtylst tynm with
            | Data(argnum) ->
                let len = List.length tyarglist in
                  if argnum = len then
                    VariantType(List.map iter tyarglist, tynm)
                  else
                    report_illegal_type_argument_length rng tynm argnum len
            | Synonym(argnum, tystr_forall) ->
                let len = List.length tyarglist in
                  if argnum = len then
                    TypeSynonym(List.map iter tyarglist, tynm, tystr_forall)
                  else
                    report_illegal_type_argument_length rng tynm argnum len
            | LocalSynonym(mdlnm, argnum, tystr_forall) ->
                let len = List.length tyarglist in
                  if argnum = len then
                    match mode with
                    | InnerMode -> TypeSynonym(List.map iter tyarglist, tynm, tystr_forall)
                    | OuterMode -> VariantType(List.map iter tyarglist, append_module_name mdlnm tynm)
                  else
                    report_illegal_type_argument_length rng tynm argnum len
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
          print_endline ("OTHER: " ^ (Display.string_of_type_struct_basic (rng, other))) ;
          assert false
        end
  in
    (rng, tymainnew)


let rec make_type_argument_numbered (var_id : Tyvarid.t) (tyargnm : var_name) (tystr : type_struct) =
  let iter = make_type_argument_numbered var_id tyargnm in
  let (rng, tymain) = tystr in
  let tymainnew =
    match tymain with
    | TypeArgument(nm)   when nm = tyargnm -> TypeVariable(var_id)
    | FuncType(tydom, tycod)               -> FuncType(iter tydom, iter tycod)
    | ListType(tycont)                     -> ListType(iter tycont)
    | RefType(tycont)                      -> RefType(iter tycont)
    | ProductType(tylist)                  -> ProductType(List.map iter tylist)
    | ForallType(tvid, tycont)             -> ForallType(tvid, iter tycont)
        (* maybe contains bugs, when tvid = -var_id *)
    | VariantType(tylist, varntnm)         -> VariantType(List.map iter tylist, varntnm)
    | TypeSynonym(tylist, tysynnm, tycont) -> TypeSynonym(List.map iter tylist, tysynnm, tycont)
    | other                                -> other
  in
    (rng, tymainnew)


let fix_manual_type (varntenv : t) tyargcons (tystr : type_struct) =
  fix_manual_type_general InnerMode varntenv (StrictMode(tyargcons)) tystr


let free_type_argument_list : (var_name list) ref = ref []

let rec make_type_argument_into_type_variable qtfbl tyarglist tystr =
  match tyarglist with
  | []                   -> tystr
  | tyargnm :: tyargtail ->
      let ntv = Tyvarid.fresh qtfbl in
      let tystr_new = make_type_argument_numbered ntv tyargnm tystr in
        make_type_argument_into_type_variable qtfbl tyargtail tystr_new


(* public *)
let fix_manual_type_for_inner_and_outer qtfbl (varntenv : t) (tystr : type_struct) =
  free_type_argument_list := [] ;
  let tystrin  = fix_manual_type_general InnerMode varntenv (FreeMode(free_type_argument_list)) tystr in
  let tystrout = fix_manual_type_general OuterMode varntenv (FreeMode(free_type_argument_list)) tystr in
    let tystrin_result = make_type_argument_into_type_variable qtfbl (!free_type_argument_list) tystrin in
    let tystrout_result = make_type_argument_into_type_variable qtfbl (!free_type_argument_list) tystrout in
      (tystrin_result, tystrout_result)


let rec make_type_argument_quantified (var_id : int) (tyargcons : untyped_type_argument_cons) (tystr : type_struct) =
  match tyargcons with
  | UTEndOfTypeArgument                        -> tystr
  | UTTypeArgumentCons(rng, tyargnm, tailcons) ->
      let tvidqtf = Tyvarid.of_int_for_quantifier var_id in
      let tystr_new = (Range.dummy "make_type_argument_quantified", ForallType(tvidqtf, make_type_argument_numbered tvidqtf tyargnm tystr)) in
        make_type_argument_quantified (var_id + 1) tailcons tystr_new


let rec type_argument_length tyargcons =
  match tyargcons with
  | UTEndOfTypeArgument                -> 0
  | UTTypeArgumentCons(_, _, tailcons) -> 1 + (type_argument_length tailcons)


let register_variant (varntenv : t) (len : int) (tynm : type_name) =
  let (defedtypelist, varntenvmain) = varntenv in
    ((tynm, Data(len)) :: defedtypelist, varntenvmain)


let add_synonym (scope : scope_kind) (varntenv : t)
                  (tyargcons : untyped_type_argument_cons) (tysynnm : type_name) (tystr : type_struct) =
  let (defedtypelist, varntenvmain) = varntenv in
  let len = type_argument_length tyargcons in
  let defkind =
    match scope with
    | GlobalScope ->
        let tystr_new    = fix_manual_type varntenv tyargcons tystr in
        let tystr_forall = make_type_argument_quantified 1 tyargcons tystr_new in
          Synonym(len, tystr_forall)

    | LocalScope(mdlnm) ->
        let tystr_new    = fix_manual_type varntenv tyargcons tystr in
        let tystr_forall = make_type_argument_quantified 1 tyargcons tystr_new in
          LocalSynonym(mdlnm, len, tystr_forall)
  in
    ((tysynnm, defkind) :: defedtypelist, varntenvmain)


(* public *)
let rec apply_to_type_synonym (tyarglist : type_struct list) (tystr_forall : type_struct) =
  match (tyarglist, tystr_forall) with
  | (tyarghd :: tyargtl, (_, ForallType(tvid, tycont))) ->
      let tystr_forall_new = Typeenv.replace_id [(tvid, tyarghd)] tycont in
        apply_to_type_synonym tyargtl tystr_forall_new
  | ([], (_, ForallType(_, _)))                         -> assert false
  | ([], _)                                             -> tystr_forall
  | _                                                   -> assert false


let rec add_variant_cons (mdlnm : module_name) (varntenv : t)
                           (tyargcons : untyped_type_argument_cons) (varntnm : type_name) (utvc : untyped_variant_cons) =

  let rec iter mdlnm varntenv tyargcons varntnm utvc =
    let (rng, utvcmain) = utvc in
      match utvcmain with
      | UTEndOfVariant                           -> varntenv
      | UTVariantCons(constrnm, tystr, tailcons) ->
          let tystr_new    = fix_manual_type varntenv tyargcons tystr in
          let tystr_forall = make_type_argument_quantified 1 tyargcons tystr_new in
          let varntenv_new = add varntenv constrnm tystr_forall (append_module_name mdlnm varntnm) in
            iter mdlnm varntenv_new tyargcons varntnm tailcons
  in
  let mdlvarntnm = append_module_name mdlnm varntnm in
  let tyarglen   = type_argument_length tyargcons in
    iter mdlnm (register_variant varntenv tyarglen mdlvarntnm) tyargcons varntnm utvc


(* public *)
let rec add_mutual_cons (scope : scope_kind) (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
  let varntenv_mem = memo_variant_name "" varntenv mutvarntcons in
  let varntenv_syn = read_synonym_spec scope varntenv_mem mutvarntcons in
  let varntenv_fin = read_variant_spec varntenv_syn mutvarntcons in
    varntenv_fin

and read_synonym_spec (scope : scope_kind) (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
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
