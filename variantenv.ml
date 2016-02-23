open Types

type definition_kind   = Data of int | Hidden of int | Synonym of type_struct
type defined_type_list = (type_name * definition_kind) list
type constructor_list  = (constructor_name * type_name * type_struct) list
type t = defined_type_list * constructor_list


let empty = ([], [])


let rec add (varntenv : t) (constrnm : constructor_name) (tystr : type_struct) (varntnm : type_name) =
  let (defedtylst, varntenvmain) = varntenv in
    (defedtylst, add_main varntenvmain constrnm tystr varntnm)

and add_main (varntenvmain : constructor_list) (constrnm : constructor_name) (tystr : type_struct) (varntnm : type_name) =
  match varntenvmain with
  | []                -> [(constrnm, varntnm, tystr)]
  | (c, v, t) :: tail ->
      if c = constrnm then
        (constrnm, varntnm, tystr) :: tail
      else
        (c, v, t) :: (add_main tail constrnm tystr varntnm)


let rec find_definition_kind (defedtylst : defined_type_list) (tynm : type_name) =
  match defedtylst with
  | []                            -> raise Not_found
  | (tn, ts) :: tl when tn = tynm -> ts
  | _ :: tl                       -> find_definition_kind tl tynm


let rec is_defined_type_argument (tyargcons : untyped_type_argument_cons) (tyargnm : var_name) =
  match tyargcons with
  | UTEndOfTypeArgument                 -> false
  | UTTypeArgumentCons(_, nm, tailcons) ->
      begin                                    (* for debug *)
        print_string ("tyarg: " ^ nm ^ "\n") ; (* for debug *)
        if nm = tyargnm then true else is_defined_type_argument tailcons tyargnm
      end                                      (* for debug *)


let rec check_type_defined (varntenv : t) (tyargcons : untyped_type_argument_cons) (tystr : type_struct) =
	let (defedtylst, varntenvmain) = varntenv in
  	match tystr with
    | VariantType(rng, tyarglist, varntnm) ->
        begin
          try
            match find_definition_kind defedtylst varntnm with
            | (Data(argnum) | Hidden(argnum)) ->
                let len = List.length tyarglist in
                  if argnum = len then
                    VariantType(rng, tyarglist, varntnm)
                  else
                    raise (TypeCheckError(
                        "at " ^ (Display.describe_position rng) ^ ":\n"
                      ^ "    variant type '" ^ varntnm ^ "' is expected to have " ^ (string_of_int argnum) ^ " type argument(s),\n"
                      ^ "    but it has " ^ (string_of_int len) ^ " type argument(s) here."))
            | Synonym(tystr) -> TypeSynonym(rng, varntnm, tystr)
          with
          | Not_found ->
              raise (TypeCheckError(
                  "at " ^ (Display.describe_position rng) ^ ":\n"
                ^ "    undefined type '" ^ varntnm ^ "'"))
        end
  	| FuncType(rng, tydom, tycod) -> FuncType(rng, check_type_defined varntenv tyargcons tydom,
                                                   check_type_defined varntenv tyargcons tycod)
  	| ListType(rng, tycont)       -> ListType(rng, check_type_defined varntenv tyargcons tycont)
  	| RefType(rng, tycont)        -> RefType(rng, check_type_defined varntenv tyargcons tycont)
  	| ProductType(rng, tylist)    -> ProductType(rng, List.map (check_type_defined varntenv tyargcons) tylist)
    | TypeArgument(rng, tyargnm)  ->
          if is_defined_type_argument tyargcons tyargnm then
            TypeArgument(rng, tyargnm)
          else
            raise (TypeCheckError(
                "at " ^ (Display.describe_position rng) ^ ":\n"
              ^ "    undefined type argument '" ^ tyargnm ^ "'"))
  	| other                       -> other


let append_module_name mdlnm varntnm =
  match mdlnm with
  | "" -> varntnm
  | _  -> mdlnm ^ "." ^ varntnm


let rec type_argument_length tyargcons =
  match tyargcons with
  | UTEndOfTypeArgument                -> 0
  | UTTypeArgumentCons(_, _, tailcons) -> 1 + (type_argument_length tailcons)


let add_variant (varntenv :t) (tyargcons : untyped_type_argument_cons) (tynm : type_name) =
  let len = type_argument_length tyargcons in
  let (defedtypelist, varntenvmain) = varntenv in
    ((tynm, Data(len)) :: defedtypelist, varntenvmain)


let add_hidden_type (mdlnm : module_name) (varntenv : t) (tyargcons : untyped_type_argument_cons) (tynm : type_name) =
  let len = type_argument_length tyargcons in
  let (defedtypelist, varntenvmain) = varntenv in
    ((append_module_name mdlnm tynm, Hidden(len)) :: defedtypelist, varntenvmain)


let add_type_synonym (varntenv : t) (tynm : type_name) (tystr : type_struct) =
  let (defedtypelist, varntenvmain) = varntenv in
  let tystr_new = check_type_defined varntenv UTEndOfTypeArgument tystr in
    ((tynm, Synonym(tystr_new)) :: defedtypelist, varntenvmain)


let rec add_cons (mdlnm : module_name) (varntenv : t)
                   (tyargcons : untyped_type_argument_cons) (varntnm : type_name) (utvc : untyped_variant_cons) =
	  add_cons_main mdlnm (add_variant varntenv tyargcons varntnm) tyargcons varntnm utvc

and add_cons_main (mdlnm : module_name) (varntenv : t)
                    (tyargcons : untyped_type_argument_cons) (varntnm : type_name) (utvc : untyped_variant_cons) =
  let (rng, utvcmain) = utvc in
    match utvcmain with
    | UTEndOfVariant                           -> varntenv
    | UTVariantCons(constrnm, tystr, tailcons) ->
        let tystr_new    = check_type_defined varntenv tyargcons tystr in
        let tystr_forall = make_type_argument_quantified 1 tyargcons tystr_new in
        let varntenv_new = add varntenv constrnm tystr_forall (append_module_name mdlnm varntnm) in
          add_cons_main mdlnm varntenv_new tyargcons varntnm tailcons

and make_type_argument_quantified (var_id : int) (tyargcons : untyped_type_argument_cons) (tystr : type_struct) =
  match tyargcons with
  | UTEndOfTypeArgument                        -> tystr
  | UTTypeArgumentCons(rng, tyargnm, tailcons) ->
      let tystr_new = ForallType(-var_id, make_type_argument_numbered var_id tyargnm tystr) in
        make_type_argument_quantified (var_id + 1) tailcons tystr_new

and make_type_argument_numbered (var_id : int) (tyargnm : var_name) (tystr : type_struct) =
  match tystr with
  | TypeArgument(rng, nm) when nm = tyargnm -> TypeVariable(rng, -var_id)
  | FuncType(rng, tydom, tycod) ->
      let tydom_new = make_type_argument_numbered var_id tyargnm tydom in
      let tycod_new = make_type_argument_numbered var_id tyargnm tycod in
        FuncType(rng, tydom_new, tycod_new)
  | ListType(rng, tycont) ->
      let tycont_new = make_type_argument_numbered var_id tyargnm tycont in
        ListType(rng, tycont_new)
  | RefType(rng, tycont) ->
      let tycont_new = make_type_argument_numbered var_id tyargnm tycont in
        RefType(rng, tycont_new)
  | ProductType(rng, tylist) ->
      let tylist_new = List.map (make_type_argument_numbered var_id tyargnm) tylist in
        ProductType(rng, tylist_new)
  | ForallType(tvid, tycont) ->
      let tycont_new = make_type_argument_numbered var_id tyargnm tycont in
        ForallType(tvid, tycont_new)
  | other -> other


let rec add_mutual_cons (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
  let varntenv_new = add_mutual_variant_type "" varntenv mutvarntcons in
    match mutvarntcons with
    | UTEndOfMutualVariant                                    -> varntenv_new
    | UTMutualVariantCons(tyargcons, varntnm, utvc, tailcons) ->
        add_mutual_cons (add_cons "" varntenv_new tyargcons varntnm utvc) tailcons


and add_mutual_cons_hidden (mdlnm : module_name) (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
  let varntenv_new = add_mutual_variant_type mdlnm varntenv mutvarntcons in
    match mutvarntcons with
    | UTEndOfMutualVariant                                 -> varntenv_new
    | UTMutualVariantCons(tyargcons, varntnm, _, tailcons) ->
        add_mutual_cons_hidden mdlnm (add_hidden_type mdlnm varntenv_new tyargcons varntnm) tailcons


and add_mutual_variant_type (mdlnm : module_name) (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
  match mutvarntcons with
  | UTEndOfMutualVariant                                 -> varntenv
  | UTMutualVariantCons(tyargcons, varntnm, _, tailcons) ->
      add_mutual_variant_type mdlnm (add_variant varntenv tyargcons (append_module_name mdlnm varntnm)) tailcons




let rec find (varntenv : t) (constrnm : constructor_name) =
	let (_, varntenvmain) = varntenv in find_main varntenvmain constrnm

and find_main varntenvmain constrnm =
    match varntenvmain with
    | []                -> raise Not_found
    | (c, v, t) :: tail -> if c = constrnm then (v, t) else find_main tail constrnm
