open Types

type definition_kind   = Data of int | Synonym of int * type_struct
type defined_type_list = (type_name * definition_kind) list
type constructor_list  = (constructor_name * type_name * type_struct) list
type t = defined_type_list * constructor_list


let empty = ([], [])


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


let rec check_type_defined (varntenv : t) (tyargcons : untyped_type_argument_cons) (tystr : type_struct) =
  let (defedtylst, varntenvmain) = varntenv in
  let f = check_type_defined varntenv tyargcons in
    match tystr with
    | IntType(rng)                      -> IntType(rng)
    | StringType(rng)                   -> StringType(rng)
    | BoolType(rng)                     -> BoolType(rng)
    | UnitType(rng)                     -> UnitType(rng)
    | FuncType(rng, tydom, tycod)       -> FuncType(rng, f tydom, f tycod)
    | ListType(rng, tycont)             -> ListType(rng, f tycont)
    | RefType(rng, tycont)              -> RefType(rng, f tycont)
    | ProductType(rng, tylist)          -> ProductType(rng, List.map f tylist)
    | VariantType(rng, tyarglist, tynm) ->
        begin
          try
            match find_definition_kind defedtylst tynm with
            | Synonym(argnum, tystr)          ->
                let len = List.length tyarglist in
                  if argnum = len then
                    TypeSynonym(rng, tyarglist, tynm, tystr)
                  else
                    Display.report_error_with_range rng [
                      "type '" ^ tynm ^ "' is expected to have " ^ (string_of_int argnum) ^ " type argument(s)," ;
                      "but it has " ^ (string_of_int len) ^ " type argument(s) here"
                    ]
            | Data(argnum) ->
                let len = List.length tyarglist in
                  if argnum = len then
                    VariantType(rng, tyarglist, tynm)
                  else
                    Display.report_error_with_range rng [
                      "type '" ^ tynm ^ "' is expected to have " ^ (string_of_int argnum) ^ " type argument(s)," ;
                      "but it has " ^ (string_of_int len) ^ " type argument(s) here"
                    ]
          with
          | Not_found -> Display.report_error_with_range rng ["undefined type '" ^ tynm ^ "'"]
        end

    | TypeArgument(rng, tyargnm)        ->
          if is_defined_type_argument tyargcons tyargnm then
            TypeArgument(rng, tyargnm)
          else
            Display.report_error_with_range rng ["undefined type argument '" ^ tyargnm ^ "'"]

    | other                             -> other


let rec make_type_argument_quantified (var_id : int) (tyargcons : untyped_type_argument_cons) (tystr : type_struct) =
  match tyargcons with
  | UTEndOfTypeArgument                        -> tystr
  | UTTypeArgumentCons(rng, tyargnm, tailcons) ->
      let tystr_new = ForallType(-var_id, make_type_argument_numbered var_id tyargnm tystr) in
        make_type_argument_quantified (var_id + 1) tailcons tystr_new

and make_type_argument_numbered (var_id : int) (tyargnm : var_name) (tystr : type_struct) =
  let f = make_type_argument_numbered var_id tyargnm in
    match tystr with
    | TypeArgument(rng, nm)
                              when nm = tyargnm -> TypeVariable(rng, -var_id)
    | FuncType(rng, tydom, tycod)               -> FuncType(rng, f tydom, f tycod)
    | ListType(rng, tycont)                     -> ListType(rng, f tycont)
    | RefType(rng, tycont)                      -> RefType(rng, f tycont)
    | ProductType(rng, tylist)                  -> ProductType(rng, List.map f tylist)
    | ForallType(tvid, tycont)                  -> ForallType(tvid, f tycont)
        (* maybe contains bugs, when tvid = -var_id *)
    | VariantType(rng, tylist, varntnm)         -> VariantType(rng, List.map f tylist, varntnm)
    | TypeSynonym(rng, tylist, tysynnm, tycont) -> TypeSynonym(rng, List.map f tylist, tysynnm, tycont)
    | other                                     -> other


let append_module_name mdlnm varntnm =
  match mdlnm with
  | "" -> varntnm
  | _  -> mdlnm ^ "." ^ varntnm


let rec type_argument_length tyargcons =
  match tyargcons with
  | UTEndOfTypeArgument                -> 0
  | UTTypeArgumentCons(_, _, tailcons) -> 1 + (type_argument_length tailcons)


let define_variant (mdlnm : module_name) (varntenv : t) (tyargcons : untyped_type_argument_cons) (tynm : type_name) =
  let len = type_argument_length tyargcons in
  let (defedtypelist, varntenvmain) = varntenv in
    ((append_module_name mdlnm tynm, Data(len)) :: defedtypelist, varntenvmain)


let add_synonym (mdlnm : module_name) (varntenv : t)
                  (tyargcons : untyped_type_argument_cons) (tysynnm : type_name) (tystr : type_struct) =
  let (defedtypelist, varntenvmain) = varntenv in
  let len = type_argument_length tyargcons in
  let defkind =
    match mdlnm with
    | "" ->
        let tystr_new    = check_type_defined varntenv tyargcons tystr in
        let tystr_forall = make_type_argument_quantified 1 tyargcons tystr_new in
          Synonym(len, tystr_forall)

    | _  -> Data(len)
  in
    ((append_module_name mdlnm tysynnm, defkind) :: defedtypelist, varntenvmain)


let rec apply_to_type_synonym (tyarglist : type_struct list) (tystr : type_struct) =
  match (tystr, tyarglist) with
  | (ForallType(tvid, tycont), tyarghd :: tyargtl) -> apply_to_type_synonym tyargtl (Typeenv.replace_id [(tvid, tyarghd)] tycont)
  | (_, [])                                        -> tystr
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
        let tystr_new    = check_type_defined varntenv tyargcons tystr in
        let tystr_forall = make_type_argument_quantified 1 tyargcons tystr_new in
        let varntenv_new = add varntenv constrnm tystr_forall (append_module_name mdlnm varntnm) in
          add_variant_cons_main mdlnm varntenv_new tyargcons varntnm tailcons


let rec add_mutual_cons (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
  let varntenv_mem = memo_variant_name "" varntenv mutvarntcons in
  let varntenv_syn = read_synonym_spec "" varntenv_mem mutvarntcons in
  let varntenv_fin = read_variant_spec "" varntenv_syn mutvarntcons in
    varntenv_fin

and read_synonym_spec (mdlnm : module_name) (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
    match mutvarntcons with
    | UTEndOfMutualVariant                                     -> varntenv
    | UTMutualVariantCons(_, _, _, tailcons)                   -> read_synonym_spec mdlnm varntenv tailcons
    | UTMutualSynonymCons(tyargcons, tysynnm, tystr, tailcons) ->
        let varntenv_new = add_synonym "" varntenv tyargcons tysynnm tystr in
          read_synonym_spec mdlnm varntenv_new tailcons

and read_variant_spec (mdlnm : module_name) (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
    match mutvarntcons with
    | UTEndOfMutualVariant                                     -> varntenv
    | UTMutualVariantCons(tyargcons, varntnm, utvc, tailcons)  ->
        let varntenv_new = add_variant_cons "" varntenv tyargcons varntnm utvc in
          read_variant_spec mdlnm varntenv_new tailcons
    | UTMutualSynonymCons(_, _, _, tailcons)                   -> read_variant_spec mdlnm varntenv tailcons


and add_mutual_cons_hidden (mdlnm : module_name) (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
  memo_variant_name mdlnm varntenv mutvarntcons


and memo_variant_name (mdlnm : module_name) (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
  match mutvarntcons with
  | UTEndOfMutualVariant                                 -> varntenv
  | UTMutualVariantCons(tyargcons, varntnm, _, tailcons) ->
      let varntenv_new = define_variant mdlnm varntenv tyargcons (append_module_name mdlnm varntnm) in
        memo_variant_name mdlnm varntenv_new tailcons
  | UTMutualSynonymCons(tyargcons, tysynnm, _, tailcons) ->
      memo_variant_name mdlnm varntenv tailcons


let rec find (varntenv : t) (constrnm : constructor_name) =
  let (_, varntenvmain) = varntenv in
    let rec f varntenvmain constrnm =
      match varntenvmain with
      | []                -> raise Not_found
      | (c, v, t) :: tail -> if c = constrnm then (v, t) else f tail constrnm
    in
      f varntenvmain constrnm
