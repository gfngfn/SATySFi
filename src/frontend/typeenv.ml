
open Types

let print_for_debug_variantenv msg =
(*
  print_endline msg;
*)
  ()


module VarMap = Map.Make
  (struct
    type t = var_name
    let compare = String.compare
  end)


module ModuleID
: sig
    type t
    val compare : t -> t -> int
    val initialize : unit -> unit
    val fresh : module_name -> t
    val extract_name : t -> module_name
  end
= struct
    type t = int * module_name
    let compare (i1, _) (i2, _) = i1 - i2
    let current_id = ref 0
    let initialize () = ( current_id := 0 )
    let fresh mdlnm =
      begin incr current_id; (!current_id, mdlnm) end
    let extract_name (_, mdlnm) = mdlnm
  end


let initialize_id () =
  ModuleID.initialize ()


module ModuleTree = HashTree.Make(ModuleID)

module ModuleNameMap = Map.Make
  (struct
    type t = module_name
    let compare = String.compare
  end)

module ConstrMap = Map.Make
  (struct
    type t = constructor_name
    let compare = String.compare
  end)

module TyNameMap = Map.Make
  (struct
    type t = type_name
    let compare = String.compare
  end)

module SigVarMap = Map.Make
  (struct
    type t = sig_var_name
    let compare = String.compare
  end)


type name_to_id_map = ModuleID.t ModuleNameMap.t

type var_to_type_map = poly_type VarMap.t

type var_to_vardef_map = (poly_type * EvalVarID.t) VarMap.t

type type_scheme = BoundID.t list * poly_type

type type_definition =
  | Data  of int
  | Alias of type_scheme

type typename_to_typedef_map = (TypeID.t * type_definition) TyNameMap.t

type constructor_to_def_map = (TypeID.t * type_scheme) ConstrMap.t

type signature = typename_to_typedef_map * var_to_type_map

type sigvar_to_sig_map = signature SigVarMap.t

type current_address = ModuleID.t Alist.t

type single_stage = var_to_vardef_map * typename_to_typedef_map * constructor_to_def_map * signature option (* * sigvar_to_sig_map *)

type t = current_address * name_to_id_map * (single_stage ModuleTree.t)


exception IllegalNumberOfTypeArguments    of Range.t * type_name * int * int
exception UndefinedTypeName               of Range.t * type_name
exception UndefinedTypeArgument           of Range.t * var_name
exception CyclicTypeDefinition            of (Range.t * type_name) list
exception MultipleTypeDefinition          of Range.t * Range.t * type_name
exception NotProvidingValueImplementation of Range.t * var_name
exception NotProvidingTypeImplementation  of Range.t * type_name
exception NotMatchingInterface            of Range.t * var_name * t * poly_type * t * poly_type
exception UndefinedModuleName             of Range.t * module_name
exception UndefinedModuleNameList         of module_name list

let empty : t =
  (Alist.empty, ModuleNameMap.empty, ModuleTree.empty (VarMap.empty, TyNameMap.empty, ConstrMap.empty, None))


let update_vt (vdf : var_to_vardef_map -> var_to_vardef_map) ((vdmap, tdmap, cdmap, sigopt) : single_stage) : single_stage =
  (vdf vdmap, tdmap, cdmap, sigopt)


let update_td (tdf : typename_to_typedef_map -> typename_to_typedef_map) ((vdmap, tdmap, cdmap, sigopt) : single_stage) : single_stage =
  (vdmap, tdf tdmap, cdmap, sigopt)


let update_cd (cdf : constructor_to_def_map -> constructor_to_def_map) ((vdmap, tdmap, cdmap, sigopt) : single_stage) : single_stage =
  (vdmap, tdmap, cdf cdmap, sigopt)


let update_so (sof : signature option -> signature option) ((vdmap, tdmap, cdmap, sigopt) : single_stage) : single_stage =
  (vdmap, tdmap, cdmap, sof sigopt)


(* PUBLIC *)
let add ((addr, nmtoid, mtr) : t) (varnm : var_name) ((pty, evid) : poly_type * EvalVarID.t) : t =
  match ModuleTree.update mtr (Alist.to_list addr) (update_vt (VarMap.add varnm (pty, evid))) with
  | None         -> raise (UndefinedModuleNameList(Alist.to_list (addr |> Alist.map ModuleID.extract_name)))
  | Some(mtrnew) -> (addr, nmtoid, mtrnew)


(* PUBLIC *)
let find ((addr, nmtoid, mtr) : t) (mdlnmlst : module_name list) (varnm : var_name) (rng : Range.t) : (poly_type * EvalVarID.t) option =
  let addrlast =
    mdlnmlst |> List.map (fun nm ->
      match nmtoid |> ModuleNameMap.find_opt nm with
      | None        -> raise (UndefinedModuleName(rng, nm))
      | Some(mdlid) -> mdlid
    )
  in
    ModuleTree.search_backward mtr (Alist.to_list addr) addrlast (fun (vdmap, _, _, sigopt) ->
      match sigopt with
      | None ->
          begin
            print_for_debug_variantenv ("FVD " ^ varnm ^ " -> no signature"); (* for debug *)
            VarMap.find_opt varnm vdmap
          end
      | Some((_, vtmapsig)) ->
          begin
            print_for_debug_variantenv ("FVD " ^ varnm ^ " -> signature found"); (* for debug *)
            match VarMap.find_opt varnm vtmapsig with
            | None -> None
            | Some(ptysig) ->
                let (_, evid) = VarMap.find varnm vdmap in
                  Some((ptysig, evid))
          end
    )


let find_for_inner ((addr, nmtoid, mtr) : t) (varnm : var_name) : (poly_type * EvalVarID.t) option =
  let (vdmap, _, _, _) = ModuleTree.find_stage mtr (Alist.to_list addr) in
    VarMap.find_opt varnm vdmap


let enter_new_module ((addr, nmtoid, mtr) : t) (mdlnm : module_name) : t =
  let mdlid = ModuleID.fresh mdlnm in
  let addrnew = Alist.extend addr mdlid in
  let nmtoidnew = ModuleNameMap.add mdlnm mdlid nmtoid in  (* doubtful about nmtoid; shouldn't a module name be added when *leaving* the module instead of when entering it? *)
  let mtrnew = ModuleTree.add_stage mtr (Alist.to_list addr) mdlid (VarMap.empty, TyNameMap.empty, ConstrMap.empty, None) in
    (addrnew, nmtoidnew, mtrnew)

(*
let enter_module_by_id ((addr, nmtoid, mtr) : t) (mdlid : ModuleID.t) : t =
  let addrnew = List.append addr [mdlid] in
  let mtrnew = ModuleTree.add_stage mtr addr mdlid (VarMap.empty, TyNameMap.empty, ConstrMap.empty, None) in
    (addrnew, nmtoid, mtrnew)
*)

let leave_module ((addr, nmtoid, mtr) : t) : t =
  match Alist.chop_last addr with
  | None                  -> assert false
  | Some((addr_outer, _)) -> (addr_outer, nmtoid, mtr)
      (* doubtful about nmtoid; see enter_new_module *)


module MapList
: sig
    type ('a, 'b) t
    val create : unit -> ('a, 'b) t
    val add : ('a, 'b) t -> 'a -> 'b -> unit
    val find_opt : ('a, 'b) t -> 'a -> 'b option
    val to_list : ('a, 'b) t -> ('a * 'b) list
  end
= struct
    type ('a, 'b) t = (('a * 'b) list) ref

    let create () = ref []

    let add mapl k v = begin mapl := (k, v) :: !mapl; end

    let find_opt mapl k = List.assoc_opt k (!mapl)

    let to_list = ( ! )

  end


type type_argument_mode =
  | StrictMode of (type_argument_name, type_variable_info ref) MapList.t
  | FreeMode   of (type_argument_name, type_variable_info ref) MapList.t
      (* --
        StrictMode : case where all type arguments should be declared; e.g. for type definitions
        FreeMode   : case where type arguments do not need to be declared; e.g. for type annotations
      -- *)


module DependencyGraph = DirectedGraph.Make
  (struct
    type t = type_name
    let compare = compare
    let show s = s (* for debug *)
  end)


type vertex_label =
  | VariantVertex of Range.t * TypeID.t * untyped_type_argument_cons * untyped_variant_cons
  | SynonymVertex of Range.t * TypeID.t * untyped_type_argument_cons * manual_type * (type_scheme option) ref


let extract_range_in_vertex_label vtxlabel =
  match vtxlabel with
  | VariantVertex(rng, _, _, _)    -> rng
  | SynonymVertex(rng, _, _, _, _) -> rng


type dependency_mode =
  | NoDependency
  | DependentMode of vertex_label DependencyGraph.t


let add_type_definition ((addr, nmtoid, mtr) : t) (tynm : type_name) ((tyid, dfn) : TypeID.t * type_definition) : t =
  match ModuleTree.update mtr (Alist.to_list addr) (update_td (TyNameMap.add tynm (tyid, dfn))) with
  | None         -> raise (UndefinedModuleNameList(addr |> Alist.map ModuleID.extract_name |> Alist.to_list))
  | Some(mtrnew) -> (addr, nmtoid, mtrnew)


let find_type_definition_for_inner ((addr, nmtoid, mtr) : t) (tynm : type_name) : (TypeID.t * type_definition) option =
  ModuleTree.search_backward mtr (Alist.to_list addr) [] (fun (_, tdmap, _, _) ->
    TyNameMap.find_opt tynm tdmap
  )


let find_type_definition_for_outer ((addr, nmtoid, mtr) : t) (tynm : type_name) : (TypeID.t * type_definition) option =
  let straddr = String.concat "." (Alist.to_list (Alist.map ModuleID.extract_name addr)) in (* for debug *)
    ModuleTree.search_backward mtr (Alist.to_list addr) [] (fun (_, tdmap, _, sigopt) ->
      match sigopt with
      | None ->
          let () = print_for_debug_variantenv ("FTD " ^ straddr ^ ", " ^ tynm ^ " -> no signature") in (* for debug *)
          TyNameMap.find_opt tynm tdmap

      | Some((tdmapsig, _)) ->
          let () = print_for_debug_variantenv ("FTD " ^ straddr ^ ", " ^ tynm ^ " -> signature found") in (* for debug *)
          TyNameMap.find_opt tynm tdmapsig
    )


(* PUBLIC *)
let find_type_id (tyenv : t) (tynm : type_name) : TypeID.t option =
  match find_type_definition_for_outer tyenv tynm with
  | None            -> None
  | Some((tyid, _)) -> Some(tyid)


(* PUBLIC *)
let find_type_name (_ : t) (tyid : TypeID.t) : type_name =
  TypeID.extract_name tyid


let add_constructor (constrnm : constructor_name) ((bidlist, pty) : type_scheme) (tyid : TypeID.t) ((addr, nmtoid, mtr) : t) : t =

  let () = print_for_debug_variantenv ("C-add " ^ constrnm ^ " of [" ^ (List.fold_left (fun s bid -> "'#" ^ (BoundID.show_direct (string_of_kind string_of_mono_type_basic) bid) ^ " " ^ s) "" bidlist) ^ "] " ^ (string_of_poly_type_basic pty)) in (* for debug *)

  match ModuleTree.update mtr (Alist.to_list addr) (update_cd (ConstrMap.add constrnm (tyid, (bidlist, pty)))) with
  | None         -> raise (UndefinedModuleNameList(addr |> Alist.map ModuleID.extract_name |> Alist.to_list))
  | Some(mtrnew) -> (addr, nmtoid, mtrnew)


let instantiate_type_scheme (tyarglist : mono_type list) (bidlist : BoundID.t list) (Poly(ty) : poly_type) =

  let () = print_for_debug_variantenv ("I-input [" ^ (List.fold_left (fun s bid -> "'#" ^ (BoundID.show_direct (string_of_kind string_of_mono_type_basic) bid) ^ " " ^ s) "" bidlist) ^ "] " ^ (string_of_mono_type_basic ty)) in (* for debug *)

  let bid_to_type_ht : (type_variable_info ref) BoundIDHashtbl.t = BoundIDHashtbl.create 32 in

  let rec pre tyargs bids =
    match (tyargs, bids) with
    | ([], [])                             -> ()
    | (tyarg :: tyargtail, bid :: bidtail) ->
        let tvref = ref (Link(tyarg)) in
        begin
          print_for_debug_variantenv ("I-add '#" ^ (BoundID.show_direct (string_of_kind string_of_mono_type_basic) bid) ^ " -> " ^ (string_of_mono_type_basic tyarg)); (* for debug *)
          BoundIDHashtbl.add bid_to_type_ht bid tvref;
          pre tyargtail bidtail;
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
                    let tvrefsubst = BoundIDHashtbl.find bid_to_type_ht bid in
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
      | BaseType(_)                       -> (rng, tymain)
      | HorzCommandType(tylist)           -> (rng, HorzCommandType(List.map aux tylist))
      | VertCommandType(tylist)           -> (rng, VertCommandType(List.map aux tylist))
      | MathCommandType(tylist)           -> (rng, MathCommandType(List.map aux tylist))
      | VertDetailedCommandType(tylist)   -> (rng, VertDetailedCommandType(List.map aux tylist))  (* will be deprecated *)
  in
  begin
    pre tyarglist bidlist;
    aux ty
  end


let rec type_argument_length tyargcons = List.length tyargcons


let rec fix_manual_type_general (dpmode : dependency_mode) (tyenv : t) (lev : FreeID.level) (tyargmode : type_argument_mode) (mnty : manual_type) =
  let rec aux mnty =
    let (rng, mntymain) = mnty in
    let iter = aux in
    let error tynm lenexp lenerr = raise (IllegalNumberOfTypeArguments(rng, tynm, lenexp, lenerr)) in
    let tymainnew =
      match mntymain with

      | MFuncType(mntydom, mntycod)      -> FuncType(iter mntydom, iter mntycod)
      | MProductType(mntylist)           -> ProductType(List.map iter mntylist)
      | MRecordType(mnasc)               -> RecordType(Assoc.map_value iter mnasc)

      | MHorzCommandType(mntylist)       -> HorzCommandType(List.map iter mntylist)
      | MVertCommandType(mntylist)       -> VertCommandType(List.map iter mntylist)
      | MMathCommandType(mntylist)       -> MathCommandType(List.map iter mntylist)

      | MTypeName([], tynm)  when tynm |> Hashtbl.mem base_type_hash_table ->
          begin
            match Hashtbl.find_opt base_type_hash_table tynm with
            | None     -> assert false
            | Some(bt) -> BaseType(bt)
          end

      | MTypeName(mntyarglist, tynm)  when tynm |> Hashtbl.mem base_type_hash_table ->
          error tynm 0 (List.length mntyarglist)

      | MTypeName(mntyarg :: [], "list") -> ListType(iter mntyarg)
      | MTypeName(mntyarglist, "list")   -> error "list" 1 (List.length mntyarglist)
      | MTypeName(mntyarg :: [], "ref")  -> RefType(iter mntyarg)
      | MTypeName(mntyarglist, "ref")    -> error "ref" 1 (List.length mntyarglist)
      | MTypeName(mntyarglist, tynm) ->
          let len = List.length mntyarglist in
          let tyarglist = List.map iter mntyarglist in
          let find_in_variant_environment () =
            match find_type_definition_for_outer tyenv tynm with
            | None -> raise (UndefinedTypeName(rng, tynm))

            | Some((tyid, Data(lenexp))) ->
                if lenexp <> len then error tynm lenexp len else
                  let () = print_for_debug_variantenv ("FV " ^ tynm ^ " -> " ^ TypeID.show_direct tyid) in (* for debug *)
                    VariantType(List.map iter mntyarglist, tyid)

            | Some((tyid, Alias(bidlist, ptyscheme))) ->
                let lenexp = List.length bidlist in
                  if lenexp <> len then error tynm lenexp len else
                    let tyreal = instantiate_type_scheme tyarglist bidlist ptyscheme in
                    let () = print_for_debug_variantenv ("FS " ^ tynm ^ " -> " ^ TypeID.show_direct tyid) in (* for debug *)
                      SynonymType(tyarglist, tyid, tyreal)
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
                    match MapList.find_opt tyargmaplist tyargnm with
                    | None        -> raise (UndefinedTypeArgument(rng, tyargnm))
                    | Some(tvref) -> TypeVariable(tvref)
                  end

              | FreeMode(tyargmaplist) ->
                  begin
                    match MapList.find_opt tyargmaplist tyargnm with
                    | Some(tvref) -> TypeVariable(tvref)
                    | None ->
                        let tvid = FreeID.fresh UniversalKind Quantifiable lev () in
                        let tvref = ref (Free(tvid)) in
                        begin
                          MapList.add tyargmaplist tyargnm tvref;
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
                let bid = BoundID.fresh (FreeID.get_kind tvid) () in
                begin
                  tvref := Bound(bid);
                  bid
                end
            | _ -> assert false
          )
        in
        (bidlist, Poly(ty))


and fix_manual_kind_general (dpmode : dependency_mode) (tyenv : t) (lev : FreeID.level) (tyargmode : type_argument_mode) (mnkd : manual_kind) : kind =
  match mnkd with
  | MUniversalKind       -> UniversalKind
  | MRecordKind(mntyasc) ->
      let aux asc =
        let (_, Poly(ty)) = fix_manual_type_general dpmode tyenv lev tyargmode asc in
          ty
      in
         RecordKind(Assoc.map_value aux mntyasc)


let fix_manual_type (dpmode : dependency_mode) (tyenv : t) (lev : FreeID.level) (tyargcons : untyped_type_argument_cons) (mnty : manual_type) =
  let tyargmaplist = MapList.create () in
  let rec aux cons =
    match cons with
    | []                             -> ()
    | (_, tyargnm, mnkd) :: tailcons ->
       let kd = fix_manual_kind_general dpmode tyenv lev (StrictMode(tyargmaplist)) mnkd in
       let () = print_for_debug_variantenv ("FMT " ^ tyargnm ^ " :: " ^ (string_of_kind string_of_mono_type_basic kd)) in (* for debug *)
       let tvid = FreeID.fresh kd Quantifiable lev () in
       begin
         MapList.add tyargmaplist tyargnm (ref (Free(tvid)));
         aux tailcons
       end
  in
  begin
    aux tyargcons;
    fix_manual_type_general dpmode tyenv lev (StrictMode(tyargmaplist)) mnty
  end


(* PUBLIC *)
let fix_manual_type_free (qtfbl : quantifiability) (tyenv : t) (lev : FreeID.level) (mnty : manual_type) (constrntcons : constraint_cons) =

  let tyargmaplist = MapList.create () in

  let () =
    constrntcons |> List.iter (fun (tyargnm, mkd) ->
      let kd = fix_manual_kind_general NoDependency tyenv lev (FreeMode(tyargmaplist)) mkd in
      let tvid = FreeID.fresh kd qtfbl lev () in
      let tvref = ref (Free(tvid)) in
        MapList.add tyargmaplist tyargnm tvref
    )
  in

  let (bidlist, ptyin) = fix_manual_type_general NoDependency tyenv lev (FreeMode(tyargmaplist)) mnty in
  let tyarglist =
    bidlist |> List.map (fun bid ->
      let tvid = FreeID.fresh (BoundID.get_kind bid) qtfbl lev () in
        (Range.dummy "fix_manual_type_free", TypeVariable(ref (Free(tvid))))
    )
  in
    instantiate_type_scheme tyarglist bidlist ptyin


let register_type (tynm : type_name) (tyid : TypeID.t) (dfn : type_definition) ((addr, nmtoid, mtr) : t) : t =
  match ModuleTree.update mtr (Alist.to_list addr) (update_td (TyNameMap.add tynm (tyid, dfn))) with
  | None         -> raise (UndefinedModuleNameList(addr |> Alist.map ModuleID.extract_name |> Alist.to_list))
  | Some(mtrnew) -> (addr, nmtoid, mtrnew)


let register_type_from_vertex (dg : vertex_label DependencyGraph.t) (tyenv : t) (tynm : type_name) : t =

  let () = print_for_debug_variantenv ("Register " ^ tynm) in (* for debug *)

    try
      match DependencyGraph.find_vertex dg tynm with

      | VariantVertex(_, tyid, tyargcons, _) ->
          let len = type_argument_length tyargcons in
            register_type tynm tyid (Data(len)) tyenv

      | SynonymVertex(_, tyid, _, _, {contents= Some((bidlist, ptyscheme))}) ->
          register_type tynm tyid (Alias(bidlist, ptyscheme)) tyenv

      | SynonymVertex(_, _, _, _, {contents= None}) -> assert false
    with
    | DependencyGraph.UndefinedSourceVertex -> failwith ("'" ^ tynm ^ "' not defined")


let rec find_constructor (qtfbl : quantifiability) ((addr, nmtoid, mtr) : t) (lev : FreeID.level) (constrnm : constructor_name) : (mono_type list * TypeID.t * mono_type) option =
  let defopt =
    ModuleTree.search_backward mtr (Alist.to_list addr) [] (fun (_, _, cdmap, _) -> ConstrMap.find_opt constrnm cdmap)
  in
    match defopt with
    | None                         -> None
    | Some((tyid, (bidlist, pty))) ->
        let tyarglist =
          bidlist |> List.map (fun bid ->
            let tvid = FreeID.fresh (BoundID.get_kind bid) qtfbl lev () in
              (Range.dummy "tc-constructor", TypeVariable(ref (Free(tvid))))
          )
        in
        let ty = instantiate_type_scheme tyarglist bidlist pty in
          Some((tyarglist, tyid, ty))



let get_moduled_var_name ((addr, nmtoid, mtr) : t) (varnm : var_name) =
  varnm |> ((Alist.map ModuleID.extract_name addr) |> Alist.fold_right (fun s mdlnm -> s ^ "." ^ mdlnm))


let get_moduled_type_name ((addr, nmtoid, mtr) : t) (tynm : type_name) =
  tynm |> ((Alist.map ModuleID.extract_name addr) |> Alist.fold_right (fun s mdlnm -> s ^ "." ^ mdlnm))


(* PUBLIC *)
let rec add_mutual_cons (tyenv : t) (lev : FreeID.level) (mutvarntcons : untyped_mutual_variant_cons) =

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
          let tyid = TypeID.fresh (get_moduled_var_name tyenv tynm) in
          begin
            DependencyGraph.add_vertex dg tynm (VariantVertex(tynmrng, tyid, tyargcons, utvarntcons));
            iter tailcons;
          end

    | UTMutualSynonymCons(tyargcons, tynmrng, tynm, mnty, tailcons) ->
        if DependencyGraph.mem_vertex tynm dg then
          let rng = extract_range_in_vertex_label (DependencyGraph.get_vertex dg tynm) in
            raise (MultipleTypeDefinition(rng, tynmrng, tynm))
        else
          let () = print_for_debug_variantenv ("AddS " ^ tynm) in (* for debug *)
          let tyid = TypeID.fresh (get_moduled_var_name tyenv tynm) in
          begin
            DependencyGraph.add_vertex dg tynm (SynonymVertex(tynmrng, tyid, tyargcons, mnty, ref None));
            iter tailcons;
          end
  in

  let rec add_each_dependency_as_edge mutvarntcons =
    let rec add_dependency tynm1 (rng, mtymain) =
      let iter = add_dependency tynm1 in
        match mtymain with
        | MTypeParam(varnm)           -> ()
        | MFuncType(mtydom, mtycod)   -> begin iter mtydom; iter mtycod; end
        | MProductType(mtylist)       -> List.iter iter mtylist
        | MRecordType(mtyasc)         -> Assoc.iter_value iter mtyasc
        | MHorzCommandType(mtylist)   -> List.iter iter mtylist
        | MVertCommandType(mtylist)   -> List.iter iter mtylist
        | MMathCommandType(mtylist)   -> List.iter iter mtylist
        | MTypeName(mtyarglist, tynm) ->
            if DependencyGraph.mem_vertex tynm dg then
              begin
                DependencyGraph.add_edge dg tynm1 tynm;
                print_for_debug_variantenv ("AddE " ^ tynm1 ^ " ---> " ^ tynm); (* for debug *)
                List.iter iter mtyarglist;
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
            add_dependency tynm mnty;
            iter tailcons;
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
      | SynonymVertex(_, _, _, _, {contents= None})    -> assert false
      | SynonymVertex(_, _, _, _, {contents= Some(_)}) -> register_type_from_vertex dg tyenv tynm
    ) dg tyenvold
  in

  let add_each_variant_type_definition (tyenvold : t) =
    let rec register_each_constructor tyargcons tynm acctyenv utvarntcons =
      let iter = register_each_constructor tyargcons tynm in
        match utvarntcons with
        | []                                -> acctyenv
        | (rng, constrnm, mnty) :: tailcons ->
            let tysch = fix_manual_type (DependentMode(dg)) acctyenv lev tyargcons mnty in
              match find_type_definition_for_inner acctyenv tynm with
              | None            -> assert false
              | Some((tyid, _)) ->
                  iter (add_constructor constrnm tysch tyid acctyenv) tailcons
    in
      DependencyGraph.fold_vertex (fun tynm label tyenv ->
        match label with
        | SynonymVertex(_, _, _, _, _)                   -> tyenv
        | VariantVertex(_, tyid, tyargcons, utvarntcons) ->
            let tyenvreg = register_type_from_vertex dg tyenv tynm in
              register_each_constructor tyargcons tynm tyenvreg utvarntcons
      ) dg tyenvold
  in

  begin
    add_each_type_as_vertex mutvarntcons;
    add_each_dependency_as_edge mutvarntcons;
    check_cyclic_type_definition ();
    embody_each_synonym_type_definition ();
    let tyenvS = add_each_synonym_type_definition tyenv in
    let tyenvV = add_each_variant_type_definition tyenvS in
      tyenvV
  end


let add_type_to_signature (sigopt : signature option) (tynm : type_name) (tyid : TypeID.t) (len : int) : signature option =
  match sigopt with
  | None                 -> Some((TyNameMap.add tynm (tyid, Data(len)) TyNameMap.empty, VarMap.empty))
  | Some((tdmap, vtmap)) -> Some((TyNameMap.add tynm (tyid, Data(len)) tdmap, vtmap))


let add_val_to_signature (sigopt : signature option) (varnm : var_name) (pty : poly_type) : signature option =
  match sigopt with
  | None               -> Some(TyNameMap.empty, VarMap.add varnm pty VarMap.empty)
  | Some(tdmap, vtmap) -> Some(tdmap, VarMap.add varnm pty vtmap)


(* -- 'reflects pty1 pty2' returns whether 'pty2' is more general than 'pty1' -- *)
let reflects (Poly(ty1) : poly_type) (Poly(ty2) : poly_type) : bool =
(*
  let current_ht : BoundID.t BoundIDHashtbl.t = BoundIDHashtbl.create 32 in
    (* -- hash table mapping bound IDs in 'pty2' to bound IDs in 'pty1' -- *)
*)
  let current_bid_to_ty : (mono_type * type_variable_info ref) BoundIDHashtbl.t = BoundIDHashtbl.create 32 in
    (* -- hash table mapping bound IDs in 'pty2' to types -- *)

  let rec aux ((_, tymain1) as ty1 : mono_type) ((_, tymain2) as ty2 : mono_type) =
    let () = print_for_debug_variantenv ("reflects " ^ (string_of_mono_type_basic ty1) ^ " << " ^ (string_of_mono_type_basic ty2)) in (* for debug *)
    let aux_list tylistcomb = tylistcomb |> List.fold_left (fun b (ty1, ty2) -> b && aux ty1 ty2) true in
    match (tymain1, tymain2) with
    | (SynonymType(tyl1, tyid1, tyreal1), _)               -> aux tyreal1 ty2
    | (_, SynonymType(tyl2, tyid2, tyreal2))               -> aux ty1 tyreal2
    | (TypeVariable({contents= Link(tysub1)}), _)          -> aux tysub1 ty2
    | (_, TypeVariable({contents= Link(tysub2)}))          -> aux ty1 tysub2

    | (TypeVariable({contents= Bound(bid1)}), TypeVariable({contents= Bound(bid2)} as tyref2)) ->
        begin
          match BoundIDHashtbl.find_opt current_bid_to_ty bid2 with
          | Some(((_, TypeVariable({contents= Bound(bid1old)})), _)) ->
              BoundID.eq bid1 bid1old

          | Some(_) ->
              false

          | None ->
              if is_stronger_kind (BoundID.get_kind bid1) (BoundID.get_kind bid2) then
                begin BoundIDHashtbl.add current_bid_to_ty bid2 (ty1, tyref2); true end
              else
                false
        end

    | (RecordType(tyasc1), TypeVariable({contents= Bound(bid2)} as tvref2)) ->
        let kd2 = BoundID.get_kind bid2 in
        let binc =
          match kd2 with
          | UniversalKind      -> true
          | RecordKind(tyasc2) -> Assoc.domain_included tyasc2 tyasc1
        in
          if not binc then false else
            begin
              match BoundIDHashtbl.find_opt current_bid_to_ty bid2 with
              | None              -> begin BoundIDHashtbl.add current_bid_to_ty bid2 (ty1, tvref2); true end
              | Some((ty1old, _)) -> aux ty1 ty1old
            end
              (* -- valid substitution of bound type variables -- *)

    | (_, TypeVariable({contents= Bound(bid2)} as tvref2)) ->
        let kd2 = BoundID.get_kind bid2 in
        begin
          match kd2 with
          | UniversalKind -> begin BoundIDHashtbl.add current_bid_to_ty bid2 (ty1, tvref2); true end
          | RecordKind(_) -> false
        end
          (* -- valid substitution of bound type variables -- *)

    | (_, TypeVariable({contents= Free(_)} as tvref)) ->
        begin tvref := Link(ty1); true end

    | (FuncType(tyd1, tyc1), FuncType(tyd2, tyc2)) ->
        (aux tyd1 tyd2) && (aux tyc1 tyc2)
          (* -- both domain and codomain are covariant -- *)

    | (HorzCommandType(tyl1), HorzCommandType(tyl2))
    | (VertCommandType(tyl1), VertCommandType(tyl2))
    | (MathCommandType(tyl1), MathCommandType(tyl2))
    | (ProductType(tyl1), ProductType(tyl2))
      ->
        begin
          try aux_list (List.combine tyl1 tyl2) with
          | Invalid_argument(_) -> false
        end

    | (RecordType(tyasc1), RecordType(tyasc2)) ->
        (Assoc.domain_same tyasc1 tyasc2) && aux_list (Assoc.combine_value tyasc1 tyasc2)

    | (VariantType(tyl1, tyid1), VariantType(tyl2, tyid2)) ->
        begin
          try (TypeID.equal tyid1 tyid2) && (aux_list (List.combine tyl1 tyl2)) with
          | Invalid_argument(_) -> false
        end

    | (ListType(tysub1), ListType(tysub2)) -> aux tysub1 tysub2
    | (RefType(tysub1), RefType(tysub2))   -> aux tysub1 tysub2
    | (BaseType(bsty1), BaseType(bsty2))   -> bsty1 = bsty2
    | _                                    -> false


  and is_stronger_kind (kd1 : kind) (kd2 : kind) =
    match (kd1, kd2) with
    | (_, UniversalKind)                       -> true
    | (UniversalKind, _)                       -> false
    | (RecordKind(tyasc1), RecordKind(tyasc2)) ->
        begin
          tyasc2 |> Assoc.fold (fun b k ty2 ->
            match Assoc.find_opt tyasc1 k with
            | Some(ty1) -> b && (aux ty1 ty2)
            | None      -> false
          ) true
        end
  in
  let b = aux ty1 ty2 in
  begin
    if b then
        current_bid_to_ty |> BoundIDHashtbl.iter (fun bid (ty, tyref) ->
          tyref := Link(ty)
        )
    else ()
  end;
  b


let sigcheck (rng : Range.t) (qtfbl : quantifiability) (lev : FreeID.level) (tyenv : t) (tyenvprev : t) (msigopt : manual_signature option) =

  let rec read_manual_signature (tyenvacc : t) (tyenvforsigI : t) (tyenvforsigO : t) (msig : manual_signature) (sigoptacc : signature option) =
    let iter = read_manual_signature in
      match msig with
      | [] -> (sigoptacc, tyenvacc)

      | SigType(tyargcons, tynm) :: tail ->
          let () = print_for_debug_variantenv ("SIGT " ^ tynm) in (* for debug *)
          begin
            match find_type_definition_for_inner tyenv tynm with
            | None              -> raise (NotProvidingTypeImplementation(rng, tynm))
            | Some((tyid, dfn)) ->
                let tyenvforsigInew = add_type_definition tyenvforsigI tynm (tyid, dfn) in
                let len = type_argument_length tyargcons in (* temporary; should check whether len is valid as to dfn *)
                let tyidout = TypeID.fresh (get_moduled_type_name tyenv tynm) in
                let sigoptaccnew = add_type_to_signature sigoptacc tynm tyidout len in
                let tyenvforsigOnew =
                  let (addr, nmtoid, mtr) = add_type_definition tyenvforsigO tynm (tyid, dfn) in
                  match ModuleTree.update mtr (Alist.to_list addr) (update_so (fun _ -> sigoptaccnew)) with
                  | None         -> raise (UndefinedModuleNameList(addr |> Alist.map ModuleID.extract_name |> Alist.to_list))
                  | Some(mtrnew) -> (addr, nmtoid, mtrnew)
                in
                  iter tyenvacc tyenvforsigInew tyenvforsigOnew tail sigoptaccnew
          end

      | SigValue(varnm, mty, constrntcons) :: tail ->
          let () = print_for_debug_variantenv ("SIGV " ^ varnm) in (* for debug *)
          let tysigI = fix_manual_type_free qtfbl tyenvforsigI (FreeID.succ_level lev) mty constrntcons in
          let ptysigI = generalize lev tysigI in
          let tysigO = fix_manual_type_free qtfbl tyenvforsigO (FreeID.succ_level lev) mty constrntcons in
          let ptysigO = generalize lev tysigO in
          let () = print_for_debug_variantenv ("LEVEL " ^ (FreeID.show_direct_level lev) ^ "; " ^ (string_of_mono_type_basic tysigI) ^ " ----> " ^ (string_of_poly_type_basic ptysigI)) in (* for debug *)
          begin
            match find_for_inner tyenv varnm with
            | None              -> raise (NotProvidingValueImplementation(rng, varnm))
            | Some((ptyimp, _)) ->
                let b = reflects ptysigI ptyimp in
                  (* -- 'reflects pty1 pty2' may change 'pty2' -- *)
                if b then
                  let sigoptaccnew = add_val_to_signature sigoptacc varnm ptysigO in
                    iter tyenvacc tyenvforsigI tyenvforsigO tail sigoptaccnew
                else
                  raise (NotMatchingInterface(rng, varnm, tyenv, ptyimp, tyenvforsigO, ptysigO))
          end

      | SigDirect(csnm, mty, constrntcons) :: tail ->
          let () = print_for_debug_variantenv ("SIGD " ^ csnm) in (* for debug *)
          let () = print_for_debug_variantenv ("D-OK0 " ^ (string_of_manual_type mty)) in (* for debug *)
          let tysigI = fix_manual_type_free qtfbl tyenvforsigI (FreeID.succ_level lev) mty constrntcons in
          let () = print_for_debug_variantenv "D-OK1" in (* for debug *)
          let ptysigI = generalize lev tysigI in
          let tysigO = fix_manual_type_free qtfbl tyenvforsigO (FreeID.succ_level lev) mty constrntcons in
          let () = print_for_debug_variantenv "D-OK2" in (* for debug *)
          let ptysigO = generalize lev tysigO in
          let () = print_for_debug_variantenv ("LEVEL " ^ (FreeID.show_direct_level lev) ^ "; " ^ (string_of_mono_type_basic tysigI) ^ " ----> " ^ (string_of_poly_type_basic ptysigI)) in (* for debug *)
          begin
            match find_for_inner tyenv csnm with
            | None                    -> raise (NotProvidingValueImplementation(rng, csnm))
            | Some((ptyimp, evidimp)) ->
                if reflects ptysigI ptyimp then
                  let sigoptaccnew = add_val_to_signature sigoptacc csnm ptysigO in
                  let tyenvaccnew =
                    let (addr, nmtoid, mtr) = tyenvacc in
                    match ModuleTree.update mtr [] (update_vt (VarMap.add csnm (ptysigO, evidimp))) with
                    | None         -> raise (UndefinedModuleNameList(addr |> Alist.map ModuleID.extract_name |> Alist.to_list))
                    | Some(mtrnew) -> (addr, nmtoid, mtrnew)
                  in
                    iter tyenvaccnew tyenvforsigI tyenvforsigO tail sigoptaccnew
                else
                  raise (NotMatchingInterface(rng, csnm, tyenv, ptyimp, tyenvforsigO, ptysigO))
          end
  in

    match msigopt with
    | None       -> tyenv
    | Some(msig) ->
        let sigoptini = Some((TyNameMap.empty, VarMap.empty)) in
        let (tyenvforsigIini, tyenvforsigOini) =
          let (addr, nmtoid, _) = tyenv in
          let (addrprev, _, mtrprev) = tyenvprev in
          let mtrprevInew = ModuleTree.add_stage mtrprev (Alist.to_list addrprev) (List.hd (List.rev (Alist.to_list addr))) (VarMap.empty, TyNameMap.empty, ConstrMap.empty, None) in
          let mtrprevOnew = ModuleTree.add_stage mtrprev (Alist.to_list addrprev) (List.hd (List.rev (Alist.to_list addr))) (VarMap.empty, TyNameMap.empty, ConstrMap.empty, sigoptini) in
            ((addr, nmtoid, mtrprevInew), (addr, nmtoid, mtrprevOnew))
        in
        let (sigopt, tyenvdir) = read_manual_signature tyenv tyenvforsigIini tyenvforsigOini msig sigoptini in
        let (addr, nmtoid, mtr) = tyenvdir in
        match ModuleTree.update mtr (Alist.to_list addr) (update_so (fun _ -> sigopt)) with
        | None         -> raise (UndefinedModuleNameList(addr |> Alist.map ModuleID.extract_name |> Alist.to_list))
        | Some(mtrnew) -> (addr, nmtoid, mtrnew)


module Raw =
  struct
    let fresh_type_id = TypeID.fresh
    let add_constructor = add_constructor
    let register_type = register_type
  end
