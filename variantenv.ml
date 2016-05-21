open Types

type definition_kind   = Data of int | Synonym of int * type_struct | LocalSynonym of module_name * int * type_struct
type defined_type_list = (type_name * definition_kind) list
type constructor_list  = (constructor_name * type_name * type_struct) list
type t = defined_type_list * constructor_list

type fix_mode = InnerMode | OuterMode
type type_argument_mode = StrictMode of untyped_type_argument_cons | FreeMode of (var_name list) ref

let empty = ([], [])


let append_module_name mdlnm varntnm =
  match mdlnm with
  | "" -> varntnm
  | _  -> mdlnm ^ "." ^ varntnm


(* t -> constructor_name -> type_struct -> type_name -> t *)
let add varntenv constrnm tystr varntnm =
  let (defedtylst, varntenvmain) = varntenv in
  let rec add_main varntenvmain constrnm tystr varntnm =
    match varntenvmain with
    | []                -> (constrnm, varntnm, tystr) :: []
    | (c, v, t) :: tail ->
        if c = constrnm then
          (constrnm, varntnm, tystr) :: tail
        else
          (c, v, t) :: (add_main tail constrnm tystr varntnm)
  in
    (defedtylst, add_main varntenvmain constrnm tystr varntnm)


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


let report_illegal_type_argument_length rng tynm len_expected len =
  Display.report_error_with_range rng [
    "'" ^ tynm ^ "' is expected to have " ^ (string_of_int len_expected) ^ " type argument(s)," ;
    "but it has " ^ (string_of_int len) ^ " type argument(s) here"
  ]


let rec fix_manual_type_general (mode : fix_mode) (varntenv : t) (tyargmode : type_argument_mode) (tystr : type_struct) =
  let (defedtylst, varntenvmain) = varntenv in
  let f = fix_manual_type_general mode varntenv tyargmode in
    match tystr with

    | FuncType(rng, tydom, tycod)           -> FuncType(rng, f tydom, f tycod)
    | ProductType(rng, tylist)              -> ProductType(rng, List.map f tylist)

    | VariantType(rng, [], "int")           -> IntType(rng)
    | VariantType(rng, tyarglist, "int")    -> report_illegal_type_argument_length rng "int" 0 (List.length tyarglist)
    | VariantType(rng, [], "string")        -> StringType(rng)
    | VariantType(rng, tyarglist, "string") -> report_illegal_type_argument_length rng "string" 0 (List.length tyarglist)
    | VariantType(rng, [], "bool")          -> BoolType(rng)
    | VariantType(rng, tyarglist, "bool")   -> report_illegal_type_argument_length rng "bool" 0 (List.length tyarglist)
    | VariantType(rng, [], "unit")          -> UnitType(rng)
    | VariantType(rng, tyarglist, "unit")   -> report_illegal_type_argument_length rng "unit" 0 (List.length tyarglist)

    | VariantType(rng, tyarg :: [], "list") -> ListType(rng, tyarg)
    | VariantType(rng, tyarglist, "list")   -> report_illegal_type_argument_length rng "list" 1 (List.length tyarglist)
    | VariantType(rng, tyarg :: [], "ref")  -> RefType(rng, tyarg)
    | VariantType(rng, tyarglist, "ref")    -> report_illegal_type_argument_length rng "ref" 1 (List.length tyarglist)
    | VariantType(rng, tyarglist, tynm) ->
        begin
          try
            match find_definition_kind defedtylst tynm with
            | Data(argnum) ->
                let len = List.length tyarglist in
                  if argnum = len then
                    VariantType(rng, List.map f tyarglist, tynm)
                  else
                    report_illegal_type_argument_length rng tynm argnum len
            | Synonym(argnum, tystr_forall) ->
                let len = List.length tyarglist in
                  if argnum = len then
                    TypeSynonym(rng, List.map f tyarglist, tynm, tystr_forall)
                  else
                    report_illegal_type_argument_length rng tynm argnum len
            | LocalSynonym(mdlnm, argnum, tystr_forall) ->
                let len = List.length tyarglist in
                  if argnum = len then
                    match mode with
                    | InnerMode -> TypeSynonym(rng, List.map f tyarglist, tynm, tystr_forall)
                    | OuterMode -> VariantType(rng, List.map f tyarglist, append_module_name mdlnm tynm)
                  else
                    report_illegal_type_argument_length rng tynm argnum len
          with
          | Not_found -> Display.report_error_with_range rng ["undefined type '" ^ tynm ^ "'"]
        end

    | TypeArgument(rng, tyargnm)        ->
          begin
            match tyargmode with
            | StrictMode(tyargcons) ->
                if is_defined_type_argument tyargcons tyargnm then
                  TypeArgument(rng, tyargnm)
                else
                  Display.report_error_with_range rng ["undefined type argument '" ^ tyargnm ^ "'"]

            | FreeMode(reftyarglst) ->
                begin
                  ( if List.mem tyargnm (!reftyarglst) then () else reftyarglst := tyargnm :: (!reftyarglst) ) ;
                  TypeArgument(rng, tyargnm)
                end
          end

    | other                             ->
        begin
          print_string ("OTHER: " ^ (Display.string_of_type_struct_basic other) ^ "\n") ;
          assert false
        end


let rec make_type_argument_numbered (var_id : Tyvarid.t) (tyargnm : var_name) (tystr : type_struct) =
  let f = make_type_argument_numbered var_id tyargnm in
    match tystr with
    | TypeArgument(rng, nm)   when nm = tyargnm -> TypeVariable(rng, var_id)
    | FuncType(rng, tydom, tycod)               -> FuncType(rng, f tydom, f tycod)
    | ListType(rng, tycont)                     -> ListType(rng, f tycont)
    | RefType(rng, tycont)                      -> RefType(rng, f tycont)
    | ProductType(rng, tylist)                  -> ProductType(rng, List.map f tylist)
    | ForallType(tvid, tycont)                  -> ForallType(tvid, f tycont)
        (* maybe contains bugs, when tvid = -var_id *)
    | VariantType(rng, tylist, varntnm)         -> VariantType(rng, List.map f tylist, varntnm)
    | TypeSynonym(rng, tylist, tysynnm, tycont) -> TypeSynonym(rng, List.map f tylist, tysynnm, tycont)
    | other                                     -> other


let fix_manual_type varntenv tyargcons tystr = fix_manual_type_general InnerMode varntenv (StrictMode(tyargcons)) tystr


let free_type_argument_list : (var_name list) ref = ref []

let rec make_type_argument_into_type_variable tyarglist tystr =
  match tyarglist with
  | []                   -> tystr
  | tyargnm :: tyargtail ->
      let ntv = Tyvarid.fresh () in
      let tystr_new = make_type_argument_numbered ntv tyargnm tystr in
        make_type_argument_into_type_variable tyargtail tystr_new


let fix_manual_type_for_inner_and_outer (varntenv : t) (tystr : type_struct) =
  free_type_argument_list := [] ;
  let tystrin  = fix_manual_type_general InnerMode varntenv (FreeMode(free_type_argument_list)) tystr in
  let tystrout = fix_manual_type_general OuterMode varntenv (FreeMode(free_type_argument_list)) tystr in
    let tystrin_result = make_type_argument_into_type_variable (!free_type_argument_list) tystrin in
    let tystrout_result = make_type_argument_into_type_variable (!free_type_argument_list) tystrout in
      (tystrin_result, tystrout_result)


let rec make_type_argument_quantified (var_id : int) (tyargcons : untyped_type_argument_cons) (tystr : type_struct) =
  match tyargcons with
  | UTEndOfTypeArgument                        -> tystr
  | UTTypeArgumentCons(rng, tyargnm, tailcons) ->
      let tvidqtf = Tyvarid.of_int_for_quantifier var_id in
      let tystr_new = ForallType(tvidqtf, make_type_argument_numbered tvidqtf tyargnm tystr) in
        make_type_argument_quantified (var_id + 1) tailcons tystr_new


let rec type_argument_length tyargcons =
  match tyargcons with
  | UTEndOfTypeArgument                -> 0
  | UTTypeArgumentCons(_, _, tailcons) -> 1 + (type_argument_length tailcons)


let define_variant (mdlnm : module_name) (varntenv : t) (tyargcons : untyped_type_argument_cons) (tynm : type_name) =
  let len = type_argument_length tyargcons in
  let (defedtypelist, varntenvmain) = varntenv in
    ((append_module_name mdlnm tynm, Data(len)) :: defedtypelist, varntenvmain)


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


let rec apply_to_type_synonym (tyarglist : type_struct list) (tystr_forall : type_struct) =
  match (tyarglist, tystr_forall) with
  | (tyarghd :: tyargtl, ForallType(tvid, tycont)) ->
      let tystr_forall_new = Typeenv.replace_id [(tvid, tyarghd)] tycont in
        apply_to_type_synonym tyargtl tystr_forall_new
  | ([], ForallType(_, _))                         -> assert false
  | ([], _)                                        -> tystr_forall
  | _                                              -> assert false


let rec add_variant_cons (mdlnm : module_name) (varntenv : t)
                           (tyargcons : untyped_type_argument_cons) (varntnm : type_name) (utvc : untyped_variant_cons) =
    add_variant_cons_main mdlnm (define_variant mdlnm varntenv tyargcons varntnm) tyargcons varntnm utvc

and add_variant_cons_main (mdlnm : module_name) (varntenv : t)
                            (tyargcons : untyped_type_argument_cons) (varntnm : type_name) (utvc : untyped_variant_cons) =
  let (rng, utvcmain) = utvc in
    match utvcmain with
    | UTEndOfVariant                           -> varntenv
    | UTVariantCons(constrnm, tystr, tailcons) ->
        let tystr_new    = fix_manual_type varntenv tyargcons tystr in
        let tystr_forall = make_type_argument_quantified 1 tyargcons tystr_new in
        let varntenv_new = add varntenv constrnm tystr_forall (append_module_name mdlnm varntnm) in
          add_variant_cons_main mdlnm varntenv_new tyargcons varntnm tailcons


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


and add_mutual_cons_hidden (mdlnm : module_name) (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
  memo_all_name mdlnm varntenv mutvarntcons


and memo_variant_name (mdlnm : module_name) (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
  match mutvarntcons with
  | UTEndOfMutualVariant                                 -> varntenv
  | UTMutualVariantCons(tyargcons, varntnm, _, tailcons) ->
      let varntenv_new = define_variant mdlnm varntenv tyargcons varntnm in
        memo_variant_name mdlnm varntenv_new tailcons
  | UTMutualSynonymCons(tyargcons, tysynnm, _, tailcons) -> memo_variant_name mdlnm varntenv tailcons


and memo_all_name (mdlnm : module_name) (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
  match mutvarntcons with
  | UTEndOfMutualVariant                                 -> varntenv
  | UTMutualVariantCons(tyargcons, varntnm, _, tailcons) ->
      let varntenv_new = define_variant mdlnm varntenv tyargcons varntnm in
        memo_all_name mdlnm varntenv_new tailcons
  | UTMutualSynonymCons(tyargcons, tysynnm, _, tailcons) ->
      let varntenv_new = define_variant mdlnm varntenv tyargcons tysynnm in
        memo_all_name mdlnm varntenv_new tailcons


let rec find (varntenv : t) (constrnm : constructor_name) =
  let (_, varntenvmain) = varntenv in
    let rec f varntenvmain constrnm =
      match varntenvmain with
      | []                -> raise Not_found
      | (c, v, t) :: tail -> if c = constrnm then (v, t) else f tail constrnm
    in
      f varntenvmain constrnm
