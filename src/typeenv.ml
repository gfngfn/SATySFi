open Types

let print_for_debug_variantenv msg =
(*
  print_endline msg ;
*)
  ()

exception IllegalNumberOfTypeArguments of Range.t * type_name * int * int
exception UndefinedTypeName            of Range.t * type_name
exception UndefinedTypeArgument        of Range.t * var_name
exception CyclicTypeDefinition         of type_name list
exception MultipleTypeDefinition       of type_name


module VarMap = Map.Make(
  struct
    type t = var_name
    let compare = String.compare
  end)

module ModuleID
: sig
    type t
    val compare : t -> t -> int
    val fresh : module_name -> t
    val extract_name : t -> module_name
  end
= struct
    type t = int * module_name
    let compare (i1, _) (i2, _) = i1 - i2
    let current_id = ref 0
    let fresh mdlnm =
      begin incr current_id ; (!current_id, mdlnm) end
    let extract_name (_, mdlnm) = mdlnm
  end

module ModuleTree = HashTree.Make(ModuleID)

module ModuleNameMap = Map.Make(
  struct
    type t = module_name
    let compare = String.compare
  end)

module ConstrMap = Map.Make(
  struct
    type t = constructor_name
    let compare = String.compare
  end)

module TyNameMap = Map.Make(
  struct
    type t = type_name
    let compare = String.compare
  end)

module SigVarMap = Map.Make(
  struct
    type t = sig_var_name
    let compare = String.compare
  end)


type name_to_id_map = ModuleID.t ModuleNameMap.t

type var_to_type_map = poly_type VarMap.t

(*
  type signature = ...
  type sigvar_to_sig_map = signature SigVarMap.t
*)

type type_scheme = Boundid.t list * poly_type
type type_definition = Data of int | Alias of type_scheme

type typename_to_typedef_map = (Typeid.t * type_definition) TyNameMap.t
type constructor_to_def_map = (Typeid.t * type_scheme) ConstrMap.t
type current_address = ModuleID.t list
type single_stage = var_to_type_map * typename_to_typedef_map * constructor_to_def_map (* signature * sigvar_to_sig_map *)
type t = current_address * name_to_id_map * (single_stage ModuleTree.t)


let from_list (lst : (var_name * poly_type) list) =
  let vtmapinit = List.fold_left (fun vmap (varnm, pty) -> VarMap.add varnm pty vmap) VarMap.empty lst in
    ([], ModuleNameMap.empty, ModuleTree.empty (vtmapinit, TyNameMap.empty, ConstrMap.empty))


let update_vt (vtf : var_to_type_map -> var_to_type_map) ((vtmap, tdmap, cdmap) : single_stage) =
  (vtf vtmap, tdmap, cdmap)


let update_td (tdf : typename_to_typedef_map -> typename_to_typedef_map) ((vtmap, tdmap, cdmap) : single_stage) =
  (vtmap, tdf tdmap, cdmap)


let update_cd (cdf : constructor_to_def_map -> constructor_to_def_map) ((vtmap, tdmap, cdmap) : single_stage) =
  (vtmap, tdmap, cdf cdmap)


let add ((addr, nmtoid, mtr) : t) (varnm : var_name) (pty : poly_type) =
  let mtrnew = ModuleTree.update mtr addr (update_vt (VarMap.add varnm pty)) in
    (addr, nmtoid, mtrnew)


let find ((addr, nmtoid, mtr) : t) (mdlnmlst : module_name list) (varnm : var_name) =
  let addrlast = List.map (fun nm -> ModuleNameMap.find nm nmtoid) mdlnmlst in
  let ptyopt =
    ModuleTree.search_backward mtr addr addrlast (fun (vtmap, _, _) ->
      try Some(VarMap.find varnm vtmap) with
      | Not_found -> None
    )
  in
    match ptyopt with
    | None      -> raise Not_found
    | Some(pty) -> pty


let enter_new_module ((addr, nmtoid, mtr) : t) (mdlnm : module_name) =
  let mdlid = ModuleID.fresh mdlnm in
  let addrnew = List.append addr [mdlid] in
  let nmtoidnew = ModuleNameMap.add mdlnm mdlid nmtoid in
  let mtrnew = ModuleTree.add_stage mtr addr mdlid (VarMap.empty, TyNameMap.empty, ConstrMap.empty) in
    (addrnew, nmtoidnew, mtrnew)


let leave_module ((addr, nmtoid, mtr) : t) =
  (List.rev (List.tl (List.rev addr)), nmtoid, mtr)


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


type vertex_label =
  | VariantVertex of Typeid.t * untyped_type_argument_cons * untyped_variant_cons
  | SynonymVertex of Typeid.t * untyped_type_argument_cons * manual_type * (type_scheme option) ref


type dependency_mode =
  | NoDependency
  | DependentMode of vertex_label DependencyGraph.t


let find_definition ((addr, nmtoid, mtr) : t) (tynm : type_name) : Typeid.t * type_definition =
  let opt =
    ModuleTree.search_backward mtr addr [] (fun (_, tdmap, _) ->
      try Some(TyNameMap.find tynm tdmap) with
      | Not_found -> None
    )
  in
    match opt with
    | None              -> raise Not_found
    | Some((tyid, dfn)) -> (tyid, dfn)

(*
let find_definition_by_id ((defedtylst, _) : t) (tyid : Typeid.t) : definition =
  let rec aux lst =
    match lst with
    | []                                                 -> raise Not_found
    | (_, tyidx, dfn) :: tail  when Typeid.eq tyidx tyid -> dfn
    | _ :: tail                                          -> aux tail
  in
    aux defedtylst
*)

(* PUBLIC *)
let find_type_id (tyenv : t) (tynm : type_name) =
  let (tyid, _) = find_definition tyenv tynm in tyid


(* PUBLIC *)
let find_type_name (_ : t) (tyid : Typeid.t) =
  Typeid.extract_name tyid


(* PUBLIC *)
let add_constructor ((addr, nmtoid, mtr) as tyenv : t) (constrnm : constructor_name) (bidlist : Boundid.t list) (pty : poly_type) (varntnm : type_name) : t =

  let () = print_for_debug_variantenv ("C-add " ^ constrnm ^ " of [" ^ (List.fold_left (fun s bid -> "'#" ^ (Boundid.show_direct bid) ^ " " ^ s) "" bidlist) ^ "] " ^ (string_of_poly_type_basic pty)) in (* for debug *)

  let (tyid, _) = find_definition tyenv varntnm in
  let mtrnew = ModuleTree.update mtr addr (update_cd (ConstrMap.add constrnm (tyid, (bidlist, pty)))) in
    (addr, nmtoid, mtrnew)


let instantiate_type_scheme (tyarglist : mono_type list) (bidlist : Boundid.t list) (Poly(ty) : poly_type) =

  let () = print_for_debug_variantenv ("I-input [" ^ (List.fold_left (fun s bid -> "'#" ^ (Boundid.show_direct bid) ^ " " ^ s) "" bidlist) ^ "] " ^ (string_of_mono_type_basic ty)) in (* for debug *)

  let bid_to_type_ht : (type_variable_info ref) BoundidHashtbl.t = BoundidHashtbl.create 32 in

  let rec pre tyargs bids =
    match (tyargs, bids) with
    | ([], [])                             -> ()
    | (tyarg :: tyargtail, bid :: bidtail) ->
        let tvref = ref (Link(tyarg)) in
        begin
          print_for_debug_variantenv ("I-add '#" ^ (Boundid.show_direct bid) ^ " -> " ^ (string_of_mono_type_basic tyarg)) ; (* for debug *)
          BoundidHashtbl.add bid_to_type_ht bid tvref ;
          pre tyargtail bidtail ;
        end
    | (_, _)                                -> assert false
  in

  let rec aux (rng, tymain) =
    let () = print_for_debug_variantenv ("aux " ^ (string_of_mono_type_basic (rng, tymain))) in (* for debug *)
      match tymain with
      | TypeVariable(tvref) ->
          begin
            match !tvref with
            | Link(tysub) -> aux tysub
            | Free(tvid)  -> (rng, tymain)
            | Bound(bid)  ->
                begin
                  try
                    let tvrefsubst = BoundidHashtbl.find bid_to_type_ht bid in
                      (rng, TypeVariable(tvrefsubst))
                  with
                  | Not_found -> assert false
                end
          end

      | FuncType(tydom, tycod)            -> (rng, FuncType(aux tydom, aux tycod))
      | ProductType(tylist)               -> (rng, ProductType(List.map aux tylist))
      | RecordType(tyasc)                 -> (rng, RecordType(Assoc.map_value aux tyasc))
      | SynonymType(tylist, tyid, tyreal) -> (rng, SynonymType(List.map aux tylist, tyid, aux tyreal))
      | VariantType(tylist, tyid)         -> (rng, VariantType(List.map aux tylist, tyid))
      | ListType(tysub)                   -> (rng, ListType(aux tysub))
      | RefType(tysub)                    -> (rng, RefType(aux tysub))
      | ( UnitType
        | BoolType
        | IntType
        | StringType )                    -> (rng, tymain)
  in
  begin
    pre tyarglist bidlist ;
    aux ty
  end


let rec type_argument_length tyargcons =
  match tyargcons with
  | UTEndOfTypeArgument                -> 0
  | UTTypeArgumentCons(_, _, tailcons) -> 1 + (type_argument_length tailcons)


let fix_manual_type_general (dpmode : dependency_mode) (tyenv : t) (lev : Tyvarid.level) (tyargmode : type_argument_mode) (mnty : manual_type) =
  let rec aux mnty =
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
              match find_definition tyenv tynm with
              | (tyid, Data(lenexp)) ->
                  if lenexp <> len then error tynm lenexp len else
                    let () = print_for_debug_variantenv ("FV " ^ tynm ^ " -> " ^ Typeid.to_string tyid) in (* for debug *)
                      VariantType(List.map iter mntyarglist, tyid)

              | (tyid, Alias(bidlist, ptyscheme)) ->
                  let lenexp = List.length bidlist in
                    if lenexp <> len then error tynm lenexp len else
                      let tyreal = instantiate_type_scheme tyarglist bidlist ptyscheme in
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

                    | SynonymVertex(tyid, tyargcons, mnty, {contents= Some(bidlist, ptyscheme)}) ->
                        let lenexp = type_argument_length tyargcons in
                          if len <> lenexp then error tynm lenexp len else
                            let tyreal = instantiate_type_scheme tyarglist bidlist ptyscheme in
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
                        let tvid = Tyvarid.fresh UniversalKind Quantifiable lev () (* temporary *) in
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
        let bidlist =
          (MapList.to_list tyargmaplist) |> List.map (fun (_, tvref) ->
            let bid = Boundid.fresh UniversalKind () in
            begin
              tvref := Bound(bid) ;
              bid
            end
          )
        in
        (bidlist, Poly(ty))


let fix_manual_type (dpmode : dependency_mode) (tyenv : t) (lev : Tyvarid.level) (tyargcons : untyped_type_argument_cons) (mnty : manual_type) =
  let tyargmaplist = MapList.create () in
  let rec aux cons =
    match cons with
    | UTEndOfTypeArgument                      -> ()
    | UTTypeArgumentCons(_, tyargnm, tailcons) ->
       let tvid = Tyvarid.fresh UniversalKind Quantifiable lev () (* temporary *) in
       begin
         MapList.add tyargmaplist tyargnm (ref (Free(tvid))) ;
         aux tailcons
       end
  in
  begin
    aux tyargcons ;
    fix_manual_type_general dpmode tyenv lev (StrictMode(tyargmaplist)) mnty
  end


(* PUBLIC *)
let fix_manual_type_for_inner (qtfbl : quantifiability) (tyenv : t) (lev : Tyvarid.level) (mnty : manual_type) =
  let tyargmaplist = MapList.create () in
  let (bidlist, ptyin) = fix_manual_type_general NoDependency tyenv lev (FreeMode(tyargmaplist)) mnty in
  let tyarglist =
    bidlist |> List.map (fun bid ->
      let tvid = Tyvarid.fresh (Boundid.get_kind bid) qtfbl lev () in
        (Range.dummy "fix_manual_type_for_inner", TypeVariable(ref (Free(tvid))))
    )
  in
    instantiate_type_scheme tyarglist bidlist ptyin


let register_type (dg : vertex_label DependencyGraph.t) ((addr, nmtoid, mtr) : t) (tynm : type_name) : t =

  let () = print_for_debug_variantenv ("Register " ^ tynm) in (* for debug *)

  let mtrnew =
    try
      match DependencyGraph.find_vertex dg tynm with

      | VariantVertex(tyid, tyargcons, _) ->
          let len = type_argument_length tyargcons in
            ModuleTree.update mtr addr (update_td (TyNameMap.add tynm (tyid, Data(len))))

      | SynonymVertex(tyid, _, _, {contents= Some((bidlist, ptyscheme))}) ->
          ModuleTree.update mtr addr (update_td (TyNameMap.add tynm (tyid, Alias(bidlist, ptyscheme))))

      | SynonymVertex(_, _, _, {contents= None}) -> assert false
    with
    | DependencyGraph.UndefinedSourceVertex -> failwith ("'" ^ tynm ^ "' not defined")
  in
    (addr, nmtoid, mtrnew)



let rec find_constructor (qtfbl : quantifiability) ((addr, nmtoid, mtr) : t) (lev : Tyvarid.level) (constrnm : constructor_name) =
  let defopt : (Typeid.t * type_scheme) option =
    ModuleTree.search_backward mtr addr [] (fun (_, _, cdmap) ->
      try Some(ConstrMap.find constrnm cdmap) with
      | Not_found -> None
    )
  in
    match defopt with
    | None                         -> raise Not_found
    | Some((tyid, (bidlist, pty))) ->
        let tyarglist =
          bidlist |> List.map (fun bid ->
            let tvid = Tyvarid.fresh (Boundid.get_kind bid) qtfbl lev () in
              (Range.dummy "tc-constructor", TypeVariable(ref (Free(tvid))))
          )
        in
        let ty = instantiate_type_scheme tyarglist bidlist pty in
          (tyarglist, tyid, ty)



let get_moduled_var_name ((addr, nmtoid, mtr) : t) (varnm : var_name) =
  varnm |> ((List.map ModuleID.extract_name addr) |> List.fold_right (fun s mdlnm -> s ^ "." ^ mdlnm))


(* PUBLIC *)
let rec add_mutual_cons (tyenv : t) (lev : Tyvarid.level) (mutvarntcons : untyped_mutual_variant_cons) =

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
          let tyid = Typeid.fresh (get_moduled_var_name tyenv tynm) in
          begin
            DependencyGraph.add_vertex dg tynm (VariantVertex(tyid, tyargcons, utvarntcons)) ;
            iter tailcons ;
          end

    | UTMutualSynonymCons(tyargcons, tynm, mnty, tailcons) ->
        if DependencyGraph.mem_vertex tynm dg then
          raise (MultipleTypeDefinition(tynm))
        else
          let () = print_for_debug_variantenv ("AddS " ^ tynm) in (* for debug *)
          let tyid = Typeid.fresh tynm in
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
          let (bidlist, pty) = fix_manual_type (DependentMode(dg)) tyenv lev tyargcons mnty in
            tyoptref := Some((bidlist, pty))
    )
  in

  let add_each_synonym_type_definition (tyenvold : t) =
    DependencyGraph.fold_vertex (fun tynm label tyenv ->
      match label with
      | VariantVertex(_, _, _)                      -> tyenv
      | SynonymVertex(_, _, _, {contents= None})    -> failwith (tynm ^ " has no embodied definition")
      | SynonymVertex(_, _, _, {contents= Some(_)}) -> register_type dg tyenv tynm
    ) dg tyenvold
  in

  let add_each_variant_type_definition (tyenvold : t) =
    let rec register_each_constructor tyargcons tynm acctyenv utvarntcons =
      let iter = register_each_constructor tyargcons tynm in
      let (rng, utvcmain) = utvarntcons in
        match utvcmain with
        | UTEndOfVariant                          -> acctyenv
        | UTVariantCons(constrnm, mnty, tailcons) ->
            let (bidlist, pty) = fix_manual_type (DependentMode(dg)) acctyenv lev tyargcons mnty in
              iter (add_constructor acctyenv constrnm bidlist pty tynm) tailcons
    in
      DependencyGraph.fold_vertex (fun tynm label tyenv ->
        match label with
        | SynonymVertex(_, _, _, _)                   -> tyenv
        | VariantVertex(tyid, tyargcons, utvarntcons) ->
            let tyenvreg = register_type dg tyenv tynm in
              register_each_constructor tyargcons tynm tyenvreg utvarntcons
      ) dg tyenvold
  in

  begin
    add_each_type_as_vertex mutvarntcons ;
    add_each_dependency_as_edge mutvarntcons ;
    check_cyclic_type_definition () ;
    embody_each_synonym_type_definition () ;
    let tyenvS = add_each_synonym_type_definition tyenv in
    let tyenvV = add_each_variant_type_definition tyenvS in
      tyenvV
(*
    let tyenvnew = add_dependency_and_each_synonym_type tyenv mutvarntcons in
    let () =
      InforefHashtbl.iter (fun tvref (tyarglist, tyid) ->
        let dfn =
          try find_definition_by_id tyenvnew tyid with Not_found -> failwith "dfn"
        in
          match dfn with
          | Data(_)                     -> failwith "Data"
          | Alias(tvreflist, ptyscheme) ->
              let tyreal = instantiate_type_scheme tyarglist tvreflist ptyscheme in
                tvref := Link((Range.dummy "register", SynonymType(tyarglist, tyid, tyreal)))
      ) synonym_ht
*)
  end
