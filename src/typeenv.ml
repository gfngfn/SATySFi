open Types

let print_for_debug_variantenv msg =
(*
  print_endline msg ;
*)
  ()


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

type type_scheme = Boundid.t list * poly_type
type type_definition = Data of int | Alias of type_scheme

type typename_to_typedef_map = (Typeid.t * type_definition) TyNameMap.t
type constructor_to_def_map = (Typeid.t * type_scheme) ConstrMap.t

type signature = typename_to_typedef_map * var_to_type_map

type sigvar_to_sig_map = signature SigVarMap.t

type current_address = ModuleID.t list
type single_stage = var_to_type_map * typename_to_typedef_map * constructor_to_def_map * signature option (* * sigvar_to_sig_map *)
type t = current_address * name_to_id_map * (single_stage ModuleTree.t)


exception IllegalNumberOfTypeArguments    of Range.t * type_name * int * int
exception UndefinedTypeName               of Range.t * type_name
exception UndefinedTypeArgument           of Range.t * var_name
exception CyclicTypeDefinition            of (Range.t * type_name) list
exception MultipleTypeDefinition          of Range.t * Range.t * type_name
exception NotProvidingValueImplementation of Range.t * var_name
exception NotProvidingTypeImplementation  of Range.t * type_name
exception NotMatchingInterface            of Range.t * var_name * t * poly_type * t * poly_type


let from_list (lst : (var_name * poly_type) list) : t =
  let vtmapinit = List.fold_left (fun vmap (varnm, pty) -> VarMap.add varnm pty vmap) VarMap.empty lst in
    ([], ModuleNameMap.empty, ModuleTree.empty (vtmapinit, TyNameMap.empty, ConstrMap.empty, None))


let update_vt (vtf : var_to_type_map -> var_to_type_map) ((vtmap, tdmap, cdmap, sigopt) : single_stage) : single_stage =
  (vtf vtmap, tdmap, cdmap, sigopt)


let update_td (tdf : typename_to_typedef_map -> typename_to_typedef_map) ((vtmap, tdmap, cdmap, sigopt) : single_stage) : single_stage =
  (vtmap, tdf tdmap, cdmap, sigopt)


let update_cd (cdf : constructor_to_def_map -> constructor_to_def_map) ((vtmap, tdmap, cdmap, sigopt) : single_stage) : single_stage =
  (vtmap, tdmap, cdf cdmap, sigopt)


let update_so (sof : signature option -> signature option) ((vtmap, tdmap, cdmap, sigopt) : single_stage) : single_stage =
  (vtmap, tdmap, cdmap, sof sigopt)


(* PUBLIC *)
let add ((addr, nmtoid, mtr) : t) (varnm : var_name) (pty : poly_type) : t =
  let mtrnew = ModuleTree.update mtr addr (update_vt (VarMap.add varnm pty)) in
    (addr, nmtoid, mtrnew)


(* PUBLIC *)
let find ((addr, nmtoid, mtr) : t) (mdlnmlst : module_name list) (varnm : var_name) : poly_type =
  let addrlast = List.map (fun nm -> ModuleNameMap.find nm nmtoid) mdlnmlst in
  let ptyopt =
    ModuleTree.search_backward mtr addr addrlast (fun (vtmap, _, _, sigopt) ->
      match sigopt with
      | None ->
          begin
            print_for_debug_variantenv ("FVD " ^ varnm ^ " -> no signature") ; (* for debug *)
            try Some(VarMap.find varnm vtmap) with
            | Not_found -> None
          end
      | Some(_, vtmapsig) ->
          begin
            print_for_debug_variantenv ("FVD " ^ varnm ^ " -> signature found") ; (* for debug *)
            try Some(VarMap.find varnm vtmapsig) with
            | Not_found -> None
          end
    )
  in
    match ptyopt with
    | None      -> raise Not_found
    | Some(pty) -> pty


let find_for_inner ((addr, nmtoid, mtr) : t) (varnm : var_name) : poly_type =
  let (vtmap, _, _, _) = ModuleTree.find_stage mtr addr in
    VarMap.find varnm vtmap


let enter_new_module ((addr, nmtoid, mtr) : t) (mdlnm : module_name) : t =
  let mdlid = ModuleID.fresh mdlnm in
  let addrnew = List.append addr [mdlid] in
  let nmtoidnew = ModuleNameMap.add mdlnm mdlid nmtoid in
  let mtrnew = ModuleTree.add_stage mtr addr mdlid (VarMap.empty, TyNameMap.empty, ConstrMap.empty, None) in
    (addrnew, nmtoidnew, mtrnew)


let leave_module ((addr, nmtoid, mtr) : t) : t =
  try
    (List.rev (List.tl (List.rev addr)), nmtoid, mtr)
  with
  | Failure("tl") -> assert false


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
  | VariantVertex of Range.t * Typeid.t * untyped_type_argument_cons * untyped_variant_cons
  | SynonymVertex of Range.t * Typeid.t * untyped_type_argument_cons * manual_type * (type_scheme option) ref


let extract_range_in_vertex_label vtxlabel =
  match vtxlabel with
  | VariantVertex(rng, _, _, _)    -> rng
  | SynonymVertex(rng, _, _, _, _) -> rng


type dependency_mode =
  | NoDependency
  | DependentMode of vertex_label DependencyGraph.t


let add_type_definition ((addr, nmtoid, mtr) : t) (tynm : type_name) ((tyid, dfn) : Typeid.t * type_definition) : t =
  let mtrnew = ModuleTree.update mtr addr (update_td (TyNameMap.add tynm (tyid, dfn))) in
    (addr, nmtoid, mtrnew)


let find_type_definition_for_inner ((addr, nmtoid, mtr) : t) (tynm : type_name) : Typeid.t * type_definition =
  let opt =
    ModuleTree.search_backward mtr addr [] (fun (_, tdmap, _, _) ->
      try Some(TyNameMap.find tynm tdmap) with
      | Not_found -> None
    )
  in
    match opt with
    | None              -> raise Not_found
    | Some((tyid, dfn)) -> (tyid, dfn)


let find_type_definition_for_outer ((addr, nmtoid, mtr) : t) (tynm : type_name) : Typeid.t * type_definition =
  let opt =
    ModuleTree.search_backward mtr addr [] (fun (_, tdmap, _, sigopt) ->
      match sigopt with
      | None ->
          begin
            print_for_debug_variantenv ("FTD " ^ tynm ^ " -> no signature") ; (* for debug *)
            try let (tyid, dfn) = TyNameMap.find tynm tdmap in Some((tyid, dfn)) with
            | Not_found -> None
          end
      | Some((tdmapsig, _)) ->
          begin
            print_for_debug_variantenv ("FTD " ^ tynm ^ " -> signature found") ; (* for debug *)
            try Some(TyNameMap.find tynm tdmapsig) with
            | Not_found -> None
          end
    )
  in
    match opt with
    | None              -> raise Not_found
    | Some((tyid, dfn)) -> (tyid, dfn)


(* PUBLIC *)
let find_type_id (tyenv : t) (tynm : type_name) : Typeid.t =
  let (tyid, _) = find_type_definition_for_outer tyenv tynm in tyid


(* PUBLIC *)
let find_type_name (_ : t) (tyid : Typeid.t) : type_name =
  Typeid.extract_name tyid


(* PUBLIC *)
let add_constructor ((addr, nmtoid, mtr) as tyenv : t) (constrnm : constructor_name) (bidlist : Boundid.t list) (pty : poly_type) (varntnm : type_name) : t =

  let () = print_for_debug_variantenv ("C-add " ^ constrnm ^ " of [" ^ (List.fold_left (fun s bid -> "'#" ^ (Boundid.show_direct (string_of_kind string_of_mono_type_basic) bid) ^ " " ^ s) "" bidlist) ^ "] " ^ (string_of_poly_type_basic pty)) in (* for debug *)

  let (tyid, _) = find_type_definition_for_inner tyenv varntnm in
  let mtrnew = ModuleTree.update mtr addr (update_cd (ConstrMap.add constrnm (tyid, (bidlist, pty)))) in
    (addr, nmtoid, mtrnew)


let instantiate_type_scheme (tyarglist : mono_type list) (bidlist : Boundid.t list) (Poly(ty) : poly_type) =

  let () = print_for_debug_variantenv ("I-input [" ^ (List.fold_left (fun s bid -> "'#" ^ (Boundid.show_direct (string_of_kind string_of_mono_type_basic) bid) ^ " " ^ s) "" bidlist) ^ "] " ^ (string_of_mono_type_basic ty)) in (* for debug *)

  let bid_to_type_ht : (type_variable_info ref) BoundidHashtbl.t = BoundidHashtbl.create 32 in

  let rec pre tyargs bids =
    match (tyargs, bids) with
    | ([], [])                             -> ()
    | (tyarg :: tyargtail, bid :: bidtail) ->
        let tvref = ref (Link(tyarg)) in
        begin
          print_for_debug_variantenv ("I-add '#" ^ (Boundid.show_direct (string_of_kind string_of_mono_type_basic) bid) ^ " -> " ^ (string_of_mono_type_basic tyarg)) ; (* for debug *)
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
  | UTEndOfTypeArgument                   -> 0
  | UTTypeArgumentCons(_, _, _, tailcons) -> 1 + (type_argument_length tailcons)


let rec fix_manual_type_general (dpmode : dependency_mode) (tyenv : t) (lev : Tyvarid.level) (tyargmode : type_argument_mode) (mnty : manual_type) =
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
              match find_type_definition_for_outer tyenv tynm with
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

                    | VariantVertex(_, tyid, tyargcons, utvarntcons) ->
                        let lenexp = type_argument_length tyargcons in
                          if len <> lenexp then error tynm lenexp len else
                            VariantType(tyarglist, tyid)

                    | SynonymVertex(_, tyid, tyargcons, mnty, {contents= Some(bidlist, ptyscheme)}) ->
                        let lenexp = type_argument_length tyargcons in
                          if len <> lenexp then error tynm lenexp len else
                            let tyreal = instantiate_type_scheme tyarglist bidlist ptyscheme in
                              SynonymType(tyarglist, tyid, tyreal)

                    | SynonymVertex(_, _, _, _, {contents= None}) -> assert false

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
            match !tvref with
            | Free(tvid) ->
                let bid = Boundid.fresh (Tyvarid.get_kind tvid) () in
                begin
                  tvref := Bound(bid) ;
                  bid
                end
            | _ -> assert false
          )
        in
        (bidlist, Poly(ty))


and fix_manual_kind_general (dpmode : dependency_mode) (tyenv : t) (lev : Tyvarid.level) (tyargmode : type_argument_mode) (mnkd : manual_kind) : kind =
  match mnkd with
  | MUniversalKind       -> UniversalKind
  | MRecordKind(mntyasc) ->
      let aux asc =
        let (_, Poly(ty)) = fix_manual_type_general dpmode tyenv lev tyargmode asc in
          ty
      in
         RecordKind(Assoc.map_value aux mntyasc)


let fix_manual_type (dpmode : dependency_mode) (tyenv : t) (lev : Tyvarid.level) (tyargcons : untyped_type_argument_cons) (mnty : manual_type) =
  let tyargmaplist = MapList.create () in
  let rec aux cons =
    match cons with
    | UTEndOfTypeArgument                            -> ()
    | UTTypeArgumentCons(_, tyargnm, mnkd, tailcons) ->
       let kd = fix_manual_kind_general dpmode tyenv lev (StrictMode(tyargmaplist)) mnkd in
       let () = print_for_debug_variantenv ("FMT " ^ tyargnm ^ " :: " ^ (string_of_kind string_of_mono_type_basic kd)) in (* for debug *)
       let tvid = Tyvarid.fresh kd Quantifiable lev () in
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
let fix_manual_type_free (qtfbl : quantifiability) (tyenv : t) (lev : Tyvarid.level) (mnty : manual_type) =
  let tyargmaplist = MapList.create () in
  let (bidlist, ptyin) = fix_manual_type_general NoDependency tyenv lev (FreeMode(tyargmaplist)) mnty in
  let tyarglist =
    bidlist |> List.map (fun bid ->
      let tvid = Tyvarid.fresh (Boundid.get_kind bid) qtfbl lev () in
        (Range.dummy "fix_manual_type_free", TypeVariable(ref (Free(tvid))))
    )
  in
    instantiate_type_scheme tyarglist bidlist ptyin


let register_type (dg : vertex_label DependencyGraph.t) ((addr, nmtoid, mtr) : t) (tynm : type_name) : t =

  let () = print_for_debug_variantenv ("Register " ^ tynm) in (* for debug *)

  let mtrnew =
    try
      match DependencyGraph.find_vertex dg tynm with

      | VariantVertex(_, tyid, tyargcons, _) ->
          let len = type_argument_length tyargcons in
            ModuleTree.update mtr addr (update_td (TyNameMap.add tynm (tyid, Data(len))))

      | SynonymVertex(_, tyid, _, _, {contents= Some((bidlist, ptyscheme))}) ->
          ModuleTree.update mtr addr (update_td (TyNameMap.add tynm (tyid, Alias(bidlist, ptyscheme))))

      | SynonymVertex(_, _, _, _, {contents= None}) -> assert false
    with
    | DependencyGraph.UndefinedSourceVertex -> failwith ("'" ^ tynm ^ "' not defined")
  in
    (addr, nmtoid, mtrnew)



let rec find_constructor (qtfbl : quantifiability) ((addr, nmtoid, mtr) : t) (lev : Tyvarid.level) (constrnm : constructor_name) =
  let defopt : (Typeid.t * type_scheme) option =
    ModuleTree.search_backward mtr addr [] (fun (_, _, cdmap, _) ->
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


let get_moduled_type_name ((addr, nmtoid, mtr) : t) (tynm : type_name) =
  tynm |> ((List.map ModuleID.extract_name addr) |> List.fold_right (fun s mdlnm -> s ^ "." ^ mdlnm))


(* PUBLIC *)
let rec add_mutual_cons (tyenv : t) (lev : Tyvarid.level) (mutvarntcons : untyped_mutual_variant_cons) =

  let dg = DependencyGraph.create 32 in

  let rec add_each_type_as_vertex mutvarntcons =
    let iter = add_each_type_as_vertex in
    match mutvarntcons with
    | UTEndOfMutualVariant -> ()
    | UTMutualVariantCons(tyargcons, tynmrng, tynm, utvarntcons, tailcons) ->
        if DependencyGraph.mem_vertex tynm dg then
          let rng = extract_range_in_vertex_label (DependencyGraph.get_vertex dg tynm) in
            raise (MultipleTypeDefinition(rng, tynmrng, tynm))
        else
          let () = print_for_debug_variantenv ("AddV " ^ tynm) in (* for debug *)
          let tyid = Typeid.fresh (get_moduled_var_name tyenv tynm) in
          begin
            DependencyGraph.add_vertex dg tynm (VariantVertex(tynmrng, tyid, tyargcons, utvarntcons)) ;
            iter tailcons ;
          end

    | UTMutualSynonymCons(tyargcons, tynmrng, tynm, mnty, tailcons) ->
        if DependencyGraph.mem_vertex tynm dg then
          let rng = extract_range_in_vertex_label (DependencyGraph.get_vertex dg tynm) in
            raise (MultipleTypeDefinition(rng, tynmrng, tynm))
        else
          let () = print_for_debug_variantenv ("AddS " ^ tynm) in (* for debug *)
          let tyid = Typeid.fresh tynm in
          begin
            DependencyGraph.add_vertex dg tynm (SynonymVertex(tynmrng, tyid, tyargcons, mnty, ref None)) ;
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
      | UTEndOfMutualVariant                                   -> ()
      | UTMutualVariantCons(_, _, tynm, utvarntcons, tailcons) -> iter tailcons
      | UTMutualSynonymCons(_, _, tynm, mnty, tailcons)        ->
          begin
            add_dependency tynm mnty ;
            iter tailcons ;
          end

  in

  let check_cyclic_type_definition () =
    let cycleopt = DependencyGraph.find_cycle dg in
      match cycleopt with
      | None      -> ()
      | Some(lst) ->
          let reslst = lst |> List.map (fun strty -> (extract_range_in_vertex_label (DependencyGraph.get_vertex dg strty), strty)) in
            raise (CyclicTypeDefinition(reslst))
  in

  let embody_each_synonym_type_definition () =
    dg |> DependencyGraph.backward_bfs (fun _ label ->
      match label with
      | VariantVertex(_, _, _, _)                                               -> ()
      | SynonymVertex(_, tyid, tyargcons, mnty, {contents= Some(_)})            -> assert false
      | SynonymVertex(_, tyid, tyargcons, mnty, ({contents= None} as tyoptref)) ->
          let (bidlist, pty) = fix_manual_type (DependentMode(dg)) tyenv lev tyargcons mnty in
            tyoptref := Some((bidlist, pty))
    )
  in

  let add_each_synonym_type_definition (tyenvold : t) =
    DependencyGraph.fold_vertex (fun tynm label tyenv ->
      match label with
      | VariantVertex(_, _, _, _)                      -> tyenv
      | SynonymVertex(_, _, _, _, {contents= None})    -> failwith (tynm ^ " has no embodied definition")
      | SynonymVertex(_, _, _, _, {contents= Some(_)}) -> register_type dg tyenv tynm
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
        | SynonymVertex(_, _, _, _, _)                   -> tyenv
        | VariantVertex(_, tyid, tyargcons, utvarntcons) ->
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
  end


let add_type_to_signature (sigopt : signature option) (tynm : type_name) (tyid : Typeid.t) (len : int) : signature option =
  match sigopt with
  | None               -> Some(TyNameMap.add tynm (tyid, Data(len)) TyNameMap.empty, VarMap.empty)
  | Some(tdmap, vtmap) -> Some(TyNameMap.add tynm (tyid, Data(len)) tdmap, vtmap)


let add_val_to_signature (sigopt : signature option) (varnm : var_name) (pty : poly_type) : signature option =
  match sigopt with
  | None               -> Some(TyNameMap.empty, VarMap.add varnm pty VarMap.empty)
  | Some(tdmap, vtmap) -> Some(tdmap, VarMap.add varnm pty vtmap)


let reflects (Poly(ty1) : poly_type) (Poly(ty2) : poly_type) : bool =

  let current_ht : Boundid.t BoundidHashtbl.t = BoundidHashtbl.create 32 in

  let rec aux ((_, tymain1) as ty1 : mono_type) ((_, tymain2) as ty2 : mono_type) =
    let () = print_for_debug_variantenv ("reflects " ^ (string_of_mono_type_basic ty1) ^ " << " ^ (string_of_mono_type_basic ty2)) in (* fr debug *)
    let aux_list tylistcomb = tylistcomb |> List.fold_left (fun b (ty1, ty2) -> b && aux ty1 ty2) true in
    match (tymain1, tymain2) with
    | (SynonymType(tyl1, tyid1, tyreal1), _)               -> aux tyreal1 ty2
    | (_, SynonymType(tyl2, tyid2, tyreal2))               -> aux ty1 tyreal2
    | (TypeVariable({contents= Link(tysub1)}), _)          -> aux tysub1 ty2
    | (_, TypeVariable({contents= Link(tysub2)}))          -> aux ty1 tysub2

    | (TypeVariable({contents= Bound(bid1)}), TypeVariable({contents= Bound(bid2)})) ->
        begin
          try
            let bidcounterpart = BoundidHashtbl.find current_ht bid1 in
              Boundid.eq bid2 bidcounterpart
          with
          | Not_found -> begin BoundidHashtbl.add current_ht bid1 bid2 ; true end
        end

    | (_, TypeVariable({contents= Bound(_)} as tvref))     -> begin tvref := Link(ty1) ; true end
                                                                (* -- valid substitution of bound type variable -- *)
    | (_, TypeVariable({contents= Free(_)} as tvref))      -> begin tvref := Link(ty1) ; true end

    | (FuncType(tyd1, tyc1), FuncType(tyd2, tyc2))         -> (aux tyd1 tyd2) && (aux tyc1 tyc2)
                                                                (* -- both domain and codomain are covariant -- *)

    | (ProductType(tyl1), ProductType(tyl2))               -> aux_list (List.combine tyl1 tyl2)
    | (RecordType(tyasc1), RecordType(tyasc2))             -> (Assoc.domain_same tyasc1 tyasc2) && aux_list (Assoc.combine_value tyasc1 tyasc2)

    | (VariantType(tyl1, tyid1), VariantType(tyl2, tyid2)) -> (Typeid.eq tyid1 tyid2) && (aux_list (List.combine tyl1 tyl2))
    | (ListType(tysub1), ListType(tysub2))                 -> aux tysub1 tysub2
    | (RefType(tysub1), RefType(tysub2))                   -> aux tysub1 tysub2
    | ( (UnitType, UnitType)
      | (IntType, IntType)
      | (BoolType, BoolType)
      | (StringType, StringType) )                         -> true
    | _                                                    -> false
  in
    aux ty1 ty2


let sigcheck (rng : Range.t) (qtfbl : quantifiability) (lev : Tyvarid.level) (tyenv : t) (tyenvprev : t) (msigopt : manual_signature option) =

  let rec read_manual_signature (tyenvacc : t) (tyenvforsigI : t) (tyenvforsigO : t) (msig : manual_signature) (sigoptacc : signature option) =
    let iter = read_manual_signature in
      match msig with
      | [] -> (sigoptacc, tyenvacc)

      | SigType(tyargcons, tynm) :: tail ->
          let () = print_for_debug_variantenv ("SIGT " ^ tynm) in (* for debug *)
          let (tyid, dfn) =
            try find_type_definition_for_inner tyenv tynm with
            | Not_found -> raise (NotProvidingTypeImplementation(rng, tynm))
          in
          let tyenvforsigInew = add_type_definition tyenvforsigI tynm (tyid, dfn) in
          let len = type_argument_length tyargcons in (* temporary; should check whether len is valid as to dfn *)
          let tyidout = Typeid.fresh (get_moduled_type_name tyenv tynm) in
          let sigoptaccnew = add_type_to_signature sigoptacc tynm tyidout len in
          let tyenvforsigOnew =
            let (addr, nmtoid, mtr) = add_type_definition tyenvforsigO tynm (tyid, dfn) in
            let mtrnew = ModuleTree.update mtr addr (update_so (fun _ -> sigoptaccnew)) in
              (addr, nmtoid, mtrnew)
          in
            iter tyenvacc tyenvforsigInew tyenvforsigOnew tail sigoptaccnew

      | SigValue(varnm, mty) :: tail ->
          let () = print_for_debug_variantenv ("SIGV " ^ varnm) in (* for debug *)
          let tysigI = fix_manual_type_free qtfbl tyenvforsigI (Tyvarid.succ_level lev) mty in
          let ptysigI = generalize lev tysigI in
          let tysigO = fix_manual_type_free qtfbl tyenvforsigO (Tyvarid.succ_level lev) mty in
          let ptysigO = generalize lev tysigO in
          let () = print_for_debug_variantenv ("LEVEL " ^ (Tyvarid.show_direct_level lev) ^ "; " ^ (string_of_mono_type_basic tysigI) ^ " ----> " ^ (string_of_poly_type_basic ptysigI)) in (* for debug *)
          let ptyimp =
            try find_for_inner tyenv varnm with
            | Not_found -> raise (NotProvidingValueImplementation(rng, varnm))
          in
            if reflects ptysigI ptyimp then
              let sigoptaccnew = add_val_to_signature sigoptacc varnm ptysigO in
                iter tyenvacc tyenvforsigI tyenvforsigO tail sigoptaccnew
            else
              raise (NotMatchingInterface(rng, varnm, tyenv, ptyimp, tyenvforsigO, ptysigO))


      | SigDirect(csnm, mty) :: tail ->
          let () = print_for_debug_variantenv ("SIGD " ^ csnm) in (* for debug *)
          let tysigI = fix_manual_type_free qtfbl tyenvforsigI (Tyvarid.succ_level lev) mty in
          let ptysigI = generalize lev tysigI in
          let tysigO = fix_manual_type_free qtfbl tyenvforsigO (Tyvarid.succ_level lev) mty in
          let ptysigO = generalize lev tysigO in
          let () = print_for_debug_variantenv ("LEVEL " ^ (Tyvarid.show_direct_level lev) ^ "; " ^ (string_of_mono_type_basic tysigI) ^ " ----> " ^ (string_of_poly_type_basic ptysigI)) in (* for debug *)
          let ptyimp =
            try find_for_inner tyenv csnm with
            | Not_found -> raise (NotProvidingValueImplementation(rng, csnm))
          in
            if reflects ptysigI ptyimp then
              let sigoptaccnew = add_val_to_signature sigoptacc csnm ptysigO in
              let tyenvaccnew =
                let (addr, nmtoid, mtr) = tyenvacc in
                let mtrnew = ModuleTree.update mtr [] (update_vt (VarMap.add csnm ptysigO)) in
                  (addr, nmtoid, mtrnew)
              in
                iter tyenvaccnew tyenvforsigI tyenvforsigO tail sigoptaccnew
            else
              raise (NotMatchingInterface(rng, csnm, tyenv, ptyimp, tyenvforsigO, ptysigO))

  in

    match msigopt with
    | None       -> tyenv
    | Some(msig) ->
        let (sigopt, tyenvdir) = read_manual_signature tyenv tyenvprev tyenvprev msig None in
        let (addr, nmtoid, mtr) = tyenvdir in
        let mtrnew = ModuleTree.update mtr addr (update_so (fun _ -> sigopt)) in
          (addr, nmtoid, mtrnew)
