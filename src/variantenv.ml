open Types

let print_for_debug_variantenv msg =

  print_endline msg ;

  ()


exception IllegalNumberOfTypeArguments of Range.t * type_name * int * int
exception UndefinedTypeName            of Range.t * type_name
exception UndefinedTypeArgument        of Range.t * var_name
exception CyclicTypeDefinition         of type_name list


type definition        = Data of int | Alias of (type_variable_info ref) list * poly_type
type defined_type_list = (type_name * Typeid.t * definition) list
type constructor_list  = (constructor_name * Typeid.t * (type_variable_info ref) list * mono_type) list

(* PUBLIC *)
type t = defined_type_list * constructor_list


module MapList
: sig
    type ('a, 'b) t
    val create : unit -> ('a, 'b) t
    val add : ('a, 'b) t -> 'a -> 'b -> unit
    val find : ('a, 'b) t -> 'a -> 'b
    val to_list : ('a, 'b) t -> ('a * 'b) list
  end
= struct
    type ('a, 'b) t = (('a * 'b) list) ref

    let create () = ref []

    let add mapl k v = ( mapl := (k, v) :: !mapl )

    let find mapl k = List.assoc k (!mapl)

    let to_list = (!)

  end


type type_argument_mode =
  | StrictMode of (type_argument_name, type_variable_info ref) MapList.t
  | FreeMode   of (type_argument_name, type_variable_info ref) MapList.t
      (* --
        StrictMode : case where all type arguments should be declared; e.g. for type definitions
        FreeMode   : case where type arguments do not need to be declared; e.g. for type annotations
      -- *)


module DependencyGraph = DirectedGraph.Make(
  struct
    type t = type_name
    let compare = compare
  end)


module InforefHashtbl = Hashtbl.Make(
  struct
    type t = type_variable_info ref
    let equal = (==)
    let hash = Hashtbl.hash
  end)


type type_mode =
  | VariantMode
  | SynonymMode


type dependency_mode =
  | NoDependency
  | DependentMode of type_mode * type_name * (type_mode * Typeid.t * int) DependencyGraph.t * (mono_type list * Typeid.t) InforefHashtbl.t


(* PUBLIC *)
let empty = ([], [])


let find_definition (defedtylst : defined_type_list) (tynm : type_name) : Typeid.t * definition =
  let rec aux lst =
    match lst with
    | []                                            -> raise Not_found
    | (tynmx, tyid, dfn) :: tail  when tynmx = tynm -> (tyid, dfn)
    | _ :: tail                                     -> aux tail
  in
    aux defedtylst


let find_definition_by_id ((defedtylst, _) : t) (tyid : Typeid.t) : definition =
  let rec aux lst =
    match lst with
    | []                                                 -> raise Not_found
    | (_, tyidx, dfn) :: tail  when Typeid.eq tyidx tyid -> dfn
    | _ :: tail                                          -> aux tail
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


(* PUBLIC *)
let add ((defedtylst, varntenvmain) : t) (constrnm : constructor_name) (paramlist : (type_variable_info ref) list) (ty : mono_type) (varntnm : type_name) =
  let (tyid, _) = find_definition defedtylst varntnm in
  let rec aux accrev lst =
    match lst with
    | []                                      -> (constrnm, tyid, paramlist, ty) :: varntenvmain
    | (c, v, p, t) :: tail  when c = constrnm -> List.rev_append accrev ((constrnm, tyid, paramlist, ty) :: tail)
    | (c, v, p, t) :: tail                    -> aux ((c, v, p, t) :: accrev) tail
  in
    (defedtylst, aux [] varntenvmain)


(* PUBLIC *)
let add_list = List.fold_left (fun ve (c, v, p, t) -> add ve c v p t)


let make_real_type (tyarglist : mono_type list) (tvreflist : (type_variable_info ref) list) (Poly(ty) : poly_type) =
  let bid_to_type_ht : mono_type InforefHashtbl.t = InforefHashtbl.create 32 in
  let rec pre tyargs tvrefs =
    match (tyargs, tvrefs) with
    | ([], [])                                 -> ()
    | (tyarg :: tyargtail, tvref :: tvreftail) ->
        begin
          InforefHashtbl.add bid_to_type_ht tvref tyarg ;
          pre tyargtail tvreftail
        end
    | (_, _)                                   -> failwith "variantenv.ml > make_real_type > pre"
  in
  let rec aux (rng, tymain) =
    let tymainres =
      match tymain with
      | TypeVariable({contents= Bound(_)} as tvref) ->
          begin
            try
              let tysubst = InforefHashtbl.find bid_to_type_ht tvref in
              begin
                tvref := Link(tysubst) ;
                tymain
              end
            with
            | Not_found -> failwith "variantenv.ml > make_real_type > aux : bound type variable"
          end
      | TypeVariable(_)                   -> tymain
      | FuncType(tydom, tycod)            -> FuncType(aux tydom, aux tycod)
      | ProductType(tylist)               -> ProductType(List.map aux tylist)
      | RecordType(tyasc)                 -> RecordType(Assoc.map_value aux tyasc)
      | SynonymType(tylist, tyid, tyreal) -> SynonymType(List.map aux tylist, tyid, aux tyreal)
      | VariantType(tylist, tyid)         -> VariantType(List.map aux tylist, tyid)
      | ListType(tysub)                   -> ListType(aux tysub)
      | RefType(tysub)                    -> RefType(aux tysub)
      | ( UnitType
        | BoolType
        | IntType
        | StringType ) -> tymain
    in
      (rng, tymainres)
  in
  begin
    pre tyarglist tvreflist ;
    aux ty
  end
    

let fix_manual_type_general (dpmode : dependency_mode) (varntenv : t) (tyargmode : type_argument_mode) (mnty : manual_type) =
  let rec aux mnty =
    let (defedtylst, varntenvmain) = varntenv in
    let (rng, mntymain) = mnty in
    let iter = aux in
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
          let find_in_variant_environment () =
            try
              match find_definition defedtylst tynm with
              | (tyid, Data(argnum)) ->
                  if argnum <> len then error tynm argnum len else
                    let () = print_for_debug_variantenv ("FV " ^ tynm ^ " -> " ^ Typeid.to_string tyid) in (* for debug *)
                    VariantType(List.map iter mntyarglist, tyid)

              | (tyid, Alias(tvreflist, ptyscheme)) ->
                  let argnum = List.length tvreflist in
                    if argnum  <> len then error tynm argnum len else
                      let tyarglist = List.map iter mntyarglist in
                      let tyreal = make_real_type tyarglist tvreflist ptyscheme in
                      let () = print_for_debug_variantenv ("FS " ^ tynm ^ " -> " ^ Typeid.to_string tyid) in (* for debug *)
                        SynonymType(tyarglist, tyid, tyreal)

            with
            | Not_found -> raise (UndefinedTypeName(rng, tynm))
          in
          begin
            match dpmode with
            | NoDependency -> find_in_variant_environment ()
            | DependentMode(objtymode, objtynm, dg, synonym_ht) ->
                begin
                  try
                    let (tymode, tyid, objlen) = DependencyGraph.find_vertex dg tynm in
                      if len <> objlen then error tynm objlen len else
                      begin
                        begin
                          match objtymode with
                          | VariantMode -> ()
                          | SynonymMode ->
                              let () = print_for_debug_variantenv ("AddE " ^ objtynm ^ " ----> " ^ tynm) in (* for debug*)
                                DependencyGraph.add_edge dg objtynm tynm
                        end ;
                        match tymode with
                        | VariantMode -> VariantType(List.map iter mntyarglist, tyid)
                        | SynonymMode ->
                            let tvid = Tyvarid.fresh UniversalKind Quantifiable () in
                              (* -- type variable for afterward replacement -- *)
                            let tvref = ref (Free(tvid)) in
                            let tyarglist = List.map iter mntyarglist in
                            begin
                              InforefHashtbl.add synonym_ht tvref (tyarglist, tyid) ;
                              TypeVariable(tvref)
                            end
                      end
                  with
                  | DependencyGraph.UndefinedSourceVertex -> find_in_variant_environment ()
                end
          end

      | MTypeParam(tyargnm) ->
            begin
              match tyargmode with
              | StrictMode(tyargmaplist) ->
                  begin
                    try
                      TypeVariable(MapList.find tyargmaplist tyargnm)
                    with
                    | Not_found -> raise (UndefinedTypeArgument(rng, tyargnm))
                  end

              | FreeMode(tyargmaplist) ->
                  begin
                    try
                      TypeVariable(MapList.find tyargmaplist tyargnm)
                    with
                    | Not_found ->
                        let tvid = Tyvarid.fresh UniversalKind Quantifiable () (* temporary *) in
                        let tvref = ref (Free(tvid)) in
                        begin
                          MapList.add tyargmaplist tyargnm tvref ;
                          TypeVariable(tvref)
                        end
                  end
            end
    in
      (rng, tymainnew)
  in
  let ty = aux mnty in
  match tyargmode with
  | ( StrictMode(tyargmaplist)
    | FreeMode(tyargmaplist) ) ->
        (List.map (fun (_, tvref) -> tvref) (MapList.to_list tyargmaplist), ty)


let fix_manual_type (dpmode : dependency_mode) (varntenv : t) (tyargcons : untyped_type_argument_cons) (mnty : manual_type) =
  let tyargmaplist = MapList.create () in
  let rec aux cons =
    match cons with
    | UTEndOfTypeArgument                      -> ()
    | UTTypeArgumentCons(_, tyargnm, tailcons) ->
       let tvid = Tyvarid.fresh UniversalKind Quantifiable () (* temporary *) in
       begin
         MapList.add tyargmaplist tyargnm (ref (Free(tvid))) ;
         aux tailcons
       end
  in
  begin
    aux tyargcons ;
    fix_manual_type_general dpmode varntenv (StrictMode(tyargmaplist)) mnty
  end


let fix_manual_type_for_synonym (dpmode : dependency_mode) (varntenv : t) (tyargcons : untyped_type_argument_cons) (mnty : manual_type) =
  let tyargmaplist = MapList.create () in
  let rec aux cons =
    match cons with
    | UTEndOfTypeArgument                      -> ()
    | UTTypeArgumentCons(_, tyargnm, tailcons) ->
       let bid = Boundid.fresh UniversalKind () (* temporary *) in
       begin
         MapList.add tyargmaplist tyargnm (ref (Bound(bid))) ;
         aux tailcons
       end
  in
  begin
    aux tyargcons ;
    fix_manual_type_general dpmode varntenv (StrictMode(tyargmaplist)) mnty
  end


(* PUBLIC *)
let fix_manual_type_for_inner_and_outer (qtfbl : quantifiability) (varntenv : t) (mnty : manual_type) =
  let tyargmaplist = MapList.create () in
  let tyin  = fix_manual_type_general NoDependency varntenv (FreeMode(tyargmaplist)) mnty in
  let tyout = fix_manual_type_general NoDependency varntenv (FreeMode(tyargmaplist)) mnty in
    (tyin, tyout)


let rec type_argument_length tyargcons =
  match tyargcons with
  | UTEndOfTypeArgument                -> 0
  | UTTypeArgumentCons(_, _, tailcons) -> 1 + (type_argument_length tailcons)


let register_variant_type (dg : (type_mode * Typeid.t * int) DependencyGraph.t) (varntenv : t) (len : int) (tynm : type_name) =
  let (defedtypelist, varntenvmain) = varntenv in
  print_for_debug_variantenv ("RV " ^ tynm) ; (* for debug *)
  try
    let (_, tyid, _) = DependencyGraph.find_vertex dg tynm in
      ((tynm, tyid, Data(len)) :: defedtypelist, varntenvmain)
  with
  | DependencyGraph.UndefinedSourceVertex -> failwith ("'" ^ tynm ^ "' not defined")


let register_synonym_type (dg : (type_mode * Typeid.t * int) DependencyGraph.t) (varntenv : t)
                  (tynm : type_name) (tvreflist : (type_variable_info ref) list) (ty : mono_type) =
  let (defedtypelist, varntenvmain) = varntenv in
  print_for_debug_variantenv ("RS " ^ tynm) ; (* for debug *)
  try
    let (_, tyid, _) = DependencyGraph.find_vertex dg tynm in
      ((tynm, tyid, Alias(tvreflist, Poly(ty))) :: defedtypelist, varntenvmain)
  with
  | DependencyGraph.UndefinedSourceVertex -> failwith ("'" ^ tynm ^ "' not defined")


let rec add_variant_cons (synonym_ht : (mono_type list * Typeid.t) InforefHashtbl.t) (dg : (type_mode * Typeid.t * int) DependencyGraph.t) (varntenv : t)
                           (tyargcons : untyped_type_argument_cons) (varntnm : type_name) (utvc : untyped_variant_cons) =

  let rec aux varntenv tyargcons utvc =
    let (rng, utvcmain) = utvc in
      match utvcmain with
      | UTEndOfVariant                          -> varntenv
      | UTVariantCons(constrnm, mnty, tailcons) ->
          let (paramlist, ty) = fix_manual_type (DependentMode(VariantMode, varntnm, dg, synonym_ht)) varntenv tyargcons mnty in
          let varntenvnew = add varntenv constrnm paramlist ty varntnm in
            aux varntenvnew tyargcons tailcons
  in
  let len = type_argument_length tyargcons in
  let varntenvreg = register_variant_type dg varntenv len varntnm in
    aux varntenvreg tyargcons utvc


(* PUBLIC *)
let rec add_mutual_cons (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =

  let synonym_ht = InforefHashtbl.create 128 in

  let dg = DependencyGraph.create 32 in

  let rec register_each_type_name mutvarntcons =
    let iter = register_each_type_name in
    match mutvarntcons with
    | UTEndOfMutualVariant -> ()
    | UTMutualVariantCons(tyargcons, tynm, _, tailcons) ->
        let () = print_for_debug_variantenv ("AddV " ^ tynm) in (* for debug *)
        let tyid = Typeid.fresh () in
        begin
          DependencyGraph.add_vertex dg tynm (VariantMode, tyid, type_argument_length tyargcons) ;
          iter tailcons
        end

    | UTMutualSynonymCons(tyargcons, tynm, _, tailcons) ->
        let () = print_for_debug_variantenv ("AddS " ^ tynm) in (* for debug *)
        let tyid = Typeid.fresh () in
        begin
          DependencyGraph.add_vertex dg tynm (SynonymMode, tyid, type_argument_length tyargcons) ;
          iter tailcons
        end
  in

  let rec add_dependency_and_each_synonym_type varntenv mutvarntcons =
    let iter = add_dependency_and_each_synonym_type in
    match mutvarntcons with
    | UTEndOfMutualVariant                                 -> varntenv
    | UTMutualVariantCons(tyargcons, tynm, utvc, tailcons) -> iter varntenv tailcons
    | UTMutualSynonymCons(tyargcons, tynm, mnty, tailcons) ->
        let (tvreflist, ty) = fix_manual_type_for_synonym (DependentMode(SynonymMode, tynm, dg, synonym_ht)) varntenv tyargcons mnty in
        let varntenvnew = register_synonym_type dg varntenv tynm tvreflist ty in
          iter varntenvnew tailcons
  in

  let rec add_each_variant_type varntenv mutvarntcons =
    let iter = add_each_variant_type in
      match mutvarntcons with
      | UTEndOfMutualVariant                                     -> varntenv
      | UTMutualSynonymCons(_, _, _, tailcons)                   -> iter varntenv tailcons
      | UTMutualVariantCons(tyargcons, varntnm, utvc, tailcons)  ->
          let varntenvnew = add_variant_cons synonym_ht dg varntenv tyargcons varntnm utvc in
            iter varntenvnew tailcons
  in

  begin
    register_each_type_name mutvarntcons ;
    let varntenvnew = add_dependency_and_each_synonym_type varntenv mutvarntcons in
    let () =
      InforefHashtbl.iter (fun tvref (tyarglist, tyid) ->
        let dfn =
          try find_definition_by_id varntenvnew tyid with Not_found -> failwith "dfn"
        in
          match dfn with
          | Data(_)                     -> failwith "Data"
          | Alias(tvreflist, ptyscheme) ->
              let tyreal = make_real_type tyarglist tvreflist ptyscheme in
                tvref := Link((Range.dummy "register", SynonymType(tyarglist, tyid, tyreal)))
      ) synonym_ht
    in
    let cycleopt = DependencyGraph.find_cycle dg in
      match cycleopt with
      | Some(lst) -> raise (CyclicTypeDefinition(lst))
      | None      -> add_each_variant_type varntenvnew mutvarntcons
  end


(* PUBLIC *)
let rec find (varntenv : t) (constrnm : constructor_name) =
  let (_, varntenvmain) = varntenv in
    let rec aux varntenvmain =
      match varntenvmain with
      | []                                      -> raise Not_found
      | (c, v, p, t) :: tail  when c = constrnm -> (v, p, t)
      | _ :: tail                               -> aux tail
    in
      aux varntenvmain
