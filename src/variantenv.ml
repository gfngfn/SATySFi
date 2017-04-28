open Types

let print_for_debug_variantenv msg =

  print_endline msg ;

  ()


exception IllegalNumberOfTypeArguments of Range.t * type_name * int * int
exception UndefinedTypeName            of Range.t * type_name
exception UndefinedTypeArgument        of Range.t * var_name
exception CyclicTypeDefinition         of type_name list
exception MultipleTypeDefinition       of type_name

type alias             = (type_variable_info ref) list * poly_type
type definition        = Data of int | Alias of alias
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
    let show s = s (* for debug *)
  end)


module InforefHashtbl = Hashtbl.Make(
  struct
    type t = type_variable_info ref
    let equal = (==)
    let hash = Hashtbl.hash
  end)


type vertex_label =
  | VariantVertex of Typeid.t * untyped_type_argument_cons * untyped_variant_cons
  | SynonymVertex of Typeid.t * untyped_type_argument_cons * manual_type * (alias option) ref


type dependency_mode =
  | NoDependency
  | DependentMode of vertex_label DependencyGraph.t


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


let instantiate_synonym_type (tyarglist : mono_type list) (tvreflist : (type_variable_info ref) list) (Poly(ty) : poly_type) =
  let tvref_to_type_ht : mono_type InforefHashtbl.t = InforefHashtbl.create 32 in
  let rec pre tyargs tvrefs =
    match (tyargs, tvrefs) with
    | ([], [])                                 -> ()
    | (tyarg :: tyargtail, tvref :: tvreftail) ->
        begin
          InforefHashtbl.add tvref_to_type_ht tvref tyarg ;
          pre tyargtail tvreftail ;
        end
    | (_, _)                                   -> assert false
  in
  let rec aux (rng, tymain) =
    let tymainres =
      match tymain with
      | TypeVariable({contents= Bound(_)} as tvref) ->
          begin
            try
              let tysubst = InforefHashtbl.find tvref_to_type_ht tvref in
              begin
                tvref := Link(tysubst) ;
                tymain
              end
            with
            | Not_found -> assert false
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
    

let rec type_argument_length tyargcons =
  match tyargcons with
  | UTEndOfTypeArgument                -> 0
  | UTTypeArgumentCons(_, _, tailcons) -> 1 + (type_argument_length tailcons)


let fix_manual_type_general (dpmode : dependency_mode) (varntenv : t) (tyargmode : type_argument_mode) (mnty : manual_type) =
  let rec aux mnty =
    let (defedtylst, varntenvmain) = varntenv in
    let (rng, mntymain) = mnty in
    let iter = aux in
    let error tynm lenexp lenerr = raise (IllegalNumberOfTypeArguments(rng, tynm, lenexp, lenerr)) in
    let tymainnew =
      match mntymain with

      | MFuncType(mntydom, mntycod)      -> FuncType(iter mntydom, iter mntycod)
      | MProductType(mntylist)           -> ProductType(List.map iter mntylist)
      | MRecordType(mnasc)               -> RecordType(Assoc.map_value iter mnasc)

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
          let tyarglist = List.map iter mntyarglist in
          let find_in_variant_environment () =
            try
              match find_definition defedtylst tynm with
              | (tyid, Data(lenexp)) ->
                  if lenexp <> len then error tynm lenexp len else
                    let () = print_for_debug_variantenv ("FV " ^ tynm ^ " -> " ^ Typeid.to_string tyid) in (* for debug *)
                      VariantType(List.map iter mntyarglist, tyid)

              | (tyid, Alias(tvreflist, ptyscheme)) ->
                  let lenexp = List.length tvreflist in
                    if lenexp <> len then error tynm lenexp len else
                      let tyreal = instantiate_synonym_type tyarglist tvreflist ptyscheme in
                      let () = print_for_debug_variantenv ("FS " ^ tynm ^ " -> " ^ Typeid.to_string tyid) in (* for debug *)
                        SynonymType(tyarglist, tyid, tyreal)

            with
            | Not_found -> raise (UndefinedTypeName(rng, tynm))
          in
          begin
            match dpmode with
            | NoDependency -> find_in_variant_environment ()
            | DependentMode(dg) ->
                begin
                  try
                    match DependencyGraph.find_vertex dg tynm with

                    | VariantVertex(tyid, tyargcons, utvarntcons) ->
                        let lenexp = type_argument_length tyargcons in
                          if len <> lenexp then error tynm lenexp len else
                            VariantType(tyarglist, tyid)

                    | SynonymVertex(tyid, tyargcons, mnty, {contents= Some(tvreflist, ptyscheme)}) ->
                        let lenexp = type_argument_length tyargcons in
                          if len <> lenexp then error tynm lenexp len else
                            let tyreal = instantiate_synonym_type tyarglist tvreflist ptyscheme in
                              SynonymType(tyarglist, tyid, tyreal)

                    | SynonymVertex(_, _, _, {contents= None}) -> assert false

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

(*
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
*)

(* PUBLIC *)
let fix_manual_type_for_inner_and_outer (qtfbl : quantifiability) (varntenv : t) (mnty : manual_type) =
  let tyargmaplist = MapList.create () in
  let tyin  = fix_manual_type_general NoDependency varntenv (FreeMode(tyargmaplist)) mnty in
  let tyout = fix_manual_type_general NoDependency varntenv (FreeMode(tyargmaplist)) mnty in
    (tyin, tyout)


let register_type (dg : vertex_label DependencyGraph.t) (varntenv : t) (tynm : type_name) =
  let (defedtypelist, varntenvmain) = varntenv in
  let () = print_for_debug_variantenv ("Register " ^ tynm) in (* for debug *)
  try
    match DependencyGraph.find_vertex dg tynm with

    | VariantVertex(tyid, tyargcons, _) ->
        let len = type_argument_length tyargcons in
          ((tynm, tyid, Data(len)) :: defedtypelist, varntenvmain)

    | SynonymVertex(tyid, _, _, {contents= Some((tvreflist, ptyscheme))}) ->
        ((tynm, tyid, Alias(tvreflist, ptyscheme)) :: defedtypelist, varntenvmain)

    | SynonymVertex(_, _, _, {contents= None}) -> assert false
  with
  | DependencyGraph.UndefinedSourceVertex -> failwith ("'" ^ tynm ^ "' not defined")

(*
let register_synonym_type (dg : (type_mode * Typeid.t * int) DependencyGraph.t) (varntenv : t)
                  (tynm : type_name) (tvreflist : (type_variable_info ref) list) (ty : mono_type) =
  let (defedtypelist, varntenvmain) = varntenv in
  print_for_debug_variantenv ("RS " ^ tynm) ; (* for debug *)
  try
    let (_, tyid, _) = DependencyGraph.find_vertex dg tynm in
      ((tynm, tyid, Alias(tvreflist, Poly(ty))) :: defedtypelist, varntenvmain)
  with
  | DependencyGraph.UndefinedSourceVertex -> failwith ("'" ^ tynm ^ "' not defined")
*)


(* PUBLIC *)
let rec add_mutual_cons (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =

  let dg = DependencyGraph.create 32 in

  let rec add_each_type_as_vertex mutvarntcons =
    let iter = add_each_type_as_vertex in
    match mutvarntcons with
    | UTEndOfMutualVariant -> ()
    | UTMutualVariantCons(tyargcons, tynm, utvarntcons, tailcons) ->
        if DependencyGraph.mem_vertex tynm dg then
          raise (MultipleTypeDefinition(tynm))
        else
          let () = print_for_debug_variantenv ("AddV " ^ tynm) in (* for debug *)
          let tyid = Typeid.fresh () in
          begin
            DependencyGraph.add_vertex dg tynm (VariantVertex(tyid, tyargcons, utvarntcons)) ;
            iter tailcons ;
          end

    | UTMutualSynonymCons(tyargcons, tynm, mnty, tailcons) ->
        if DependencyGraph.mem_vertex tynm dg then
          raise (MultipleTypeDefinition(tynm))
        else
          let () = print_for_debug_variantenv ("AddS " ^ tynm) in (* for debug *)
          let tyid = Typeid.fresh () in
          begin
            DependencyGraph.add_vertex dg tynm (SynonymVertex(tyid, tyargcons, mnty, ref None)) ;
            iter tailcons ;
          end
  in

  let rec add_each_dependency_as_edge mutvarntcons =
    let rec add_dependency tynm1 (rng, mtymain) =
      let iter = add_dependency tynm1 in
        match mtymain with
        | MTypeParam(varnm)           -> ()
        | MFuncType(mtydom, mtycod)   -> begin iter mtydom ; iter mtycod ; end
        | MProductType(mtylist)       -> List.iter iter mtylist
        | MRecordType(mtyasc)         -> Assoc.iter_value iter mtyasc
        | MTypeName(mtyarglist, tynm) ->
            if DependencyGraph.mem_vertex tynm dg then
              begin
                DependencyGraph.add_edge dg tynm1 tynm ;
                print_for_debug_variantenv ("AddE " ^ tynm1 ^ " ---> " ^ tynm) ; (* for debug *)
                List.iter iter mtyarglist ;
              end
            else
              ()
    in
    let iter = add_each_dependency_as_edge in
      match mutvarntcons with
      | UTEndOfMutualVariant                                -> ()
      | UTMutualVariantCons(_, tynm, utvarntcons, tailcons) -> iter tailcons
      | UTMutualSynonymCons(_, tynm, mnty, tailcons)        ->
          begin
            add_dependency tynm mnty ;
            iter tailcons ;
          end

  in
(*
  let rec add_dependency_and_each_synonym_type varntenv mutvarntcons =
    let iter = add_dependency_and_each_synonym_type in
    match mutvarntcons with
    | UTEndOfMutualVariant                                 -> varntenv
    | UTMutualVariantCons(tyargcons, tynm, utvc, tailcons) -> iter varntenv tailcons
    | UTMutualSynonymCons(tyargcons, tynm, mnty, tailcons) ->
        let (tvreflist, ty) = fix_manual_type_for_synonym (DependentMode(dg)) varntenv tyargcons mnty in
        let varntenvnew = register_type dg varntenv tynm in
          iter varntenvnew tailcons
  in

  let rec add_each_variant_type varntenv mutvarntcons =
    let iter = add_each_variant_type in
      match mutvarntcons with
      | UTEndOfMutualVariant                                     -> varntenv
      | UTMutualSynonymCons(_, _, _, tailcons)                   -> iter varntenv tailcons
      | UTMutualVariantCons(tyargcons, varntnm, utvc, tailcons)  ->
          let varntenvnew = add_variant_cons dg varntenv tyargcons varntnm utvc in
            iter varntenvnew tailcons
  in
*)
  let check_cyclic_type_definition () =
    let cycleopt = DependencyGraph.find_cycle dg in
      match cycleopt with
      | Some(lst) -> raise (CyclicTypeDefinition(lst))
      | None      -> ()
  in

  let embody_each_synonym_type_definition () =
    dg |> DependencyGraph.backward_bfs (fun _ label ->
      match label with
      | VariantVertex(_, _, _)                                               -> ()
      | SynonymVertex(tyid, tyargcons, mnty, {contents= Some(_)})            -> assert false
      | SynonymVertex(tyid, tyargcons, mnty, ({contents= None} as tyoptref)) ->
          let (tvreflist, ty) = fix_manual_type (DependentMode(dg)) varntenv tyargcons mnty in
            tyoptref := Some((tvreflist, Poly(ty)))
    )
  in

  let add_each_synonym_type_definition (varntenvold : t) =
    DependencyGraph.fold_vertex (fun tynm label varntenv ->
      match label with
      | VariantVertex(_, _, _)                      -> varntenv
      | SynonymVertex(_, _, _, {contents= None})    -> failwith (tynm ^ " has no embodied definition")
      | SynonymVertex(_, _, _, {contents= Some(_)}) -> register_type dg varntenv tynm
    ) dg varntenvold
  in

  let add_each_variant_type_definition (varntenvold : t) =
    let rec register_each_constructor tyargcons tynm accvarntenv utvarntcons =
      let iter = register_each_constructor tyargcons tynm in
      let (rng, utvcmain) = utvarntcons in
        match utvcmain with
        | UTEndOfVariant                          -> accvarntenv
        | UTVariantCons(constrnm, mnty, tailcons) ->
            let (paramlist, ty) = fix_manual_type (DependentMode(dg)) accvarntenv tyargcons mnty in
              iter (add accvarntenv constrnm paramlist ty tynm) tailcons
    in
      DependencyGraph.fold_vertex (fun tynm label varntenv ->
        match label with
        | SynonymVertex(_, _, _, _)                   -> varntenv
        | VariantVertex(tyid, tyargcons, utvarntcons) ->
            let varntenvreg = register_type dg varntenv tynm in
              register_each_constructor tyargcons tynm varntenvreg utvarntcons
      ) dg varntenvold
  in

  begin
    add_each_type_as_vertex mutvarntcons ;
    add_each_dependency_as_edge mutvarntcons ;
    check_cyclic_type_definition () ;
    embody_each_synonym_type_definition () ;
    let varntenvS = add_each_synonym_type_definition varntenv in
    let varntenvV = add_each_variant_type_definition varntenvS in
      varntenvV
(*
    let varntenvnew = add_dependency_and_each_synonym_type varntenv mutvarntcons in
    let () =
      InforefHashtbl.iter (fun tvref (tyarglist, tyid) ->
        let dfn =
          try find_definition_by_id varntenvnew tyid with Not_found -> failwith "dfn"
        in
          match dfn with
          | Data(_)                     -> failwith "Data"
          | Alias(tvreflist, ptyscheme) ->
              let tyreal = instantiate_synonym_type tyarglist tvreflist ptyscheme in
                tvref := Link((Range.dummy "register", SynonymType(tyarglist, tyid, tyreal)))
      ) synonym_ht
*)
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
