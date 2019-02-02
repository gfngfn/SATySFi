
open MyUtil
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

type t =
  {
    current_address : current_address;
    name_to_id_map  : name_to_id_map;
    main_tree       : single_stage ModuleTree.t;
  }


exception IllegalNumberOfTypeArguments    of Range.t * type_name * int * int
exception UndefinedTypeName               of Range.t * module_name list * type_name * type_name list
exception UndefinedTypeArgument           of Range.t * var_name * var_name list
exception CyclicTypeDefinition            of (Range.t * type_name) list
exception MultipleTypeDefinition          of Range.t * Range.t * type_name
exception NotProvidingValueImplementation of Range.t * var_name
exception NotProvidingTypeImplementation  of Range.t * type_name
exception NotMatchingInterface            of Range.t * var_name * t * poly_type * t * poly_type
exception UndefinedModuleName             of Range.t * module_name * module_name list


let empty : t =
  {
    current_address = Alist.empty;
    name_to_id_map  = ModuleNameMap.empty;
    main_tree       = ModuleTree.empty (VarMap.empty, TyNameMap.empty, ConstrMap.empty, None);
  }


let update_vt (vdf : var_to_vardef_map -> var_to_vardef_map) ((vdmap, tdmap, cdmap, sigopt) : single_stage) : single_stage =
  (vdf vdmap, tdmap, cdmap, sigopt)


let update_td (tdf : typename_to_typedef_map -> typename_to_typedef_map) ((vdmap, tdmap, cdmap, sigopt) : single_stage) : single_stage =
  (vdmap, tdf tdmap, cdmap, sigopt)


let update_cd (cdf : constructor_to_def_map -> constructor_to_def_map) ((vdmap, tdmap, cdmap, sigopt) : single_stage) : single_stage =
  (vdmap, tdmap, cdf cdmap, sigopt)


let update_so (sof : signature option -> signature option) ((vdmap, tdmap, cdmap, sigopt) : single_stage) : single_stage =
  (vdmap, tdmap, cdmap, sof sigopt)


let edit_distance s1 s2 mindist =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  if abs (len1 - len2) > mindist then
    mindist + 1
  else
    let d = Array.make_matrix (len1 + 1) (len2 + 1) 0 in
    begin
      for i = 0 to len1 do
        d.(i).(0) <- i
      done;
      for j = 0 to len2 do
        d.(0).(j) <- j
      done;
      for i = 1 to len1 do
        for j = 1 to len2 do
          let replace = if Char.equal (String.get s1 (i - 1)) (String.get s2 (j - 1)) then 0 else 1 in
            d.(i).(j) <-  min (min (d.(i - 1).(j) + 1) (d.(i).(j - 1) + 1)) (d.(i - 1).(j - 1) + replace)
        done
      done;
      d.(len1).(len2)
    end


let initial_candidates nm =
  let maxdist =
    match String.length nm with
    | 1 | 2 -> 0
    | 3 | 4 -> 1
    | 5 | 6 -> 2
    | _     -> 3
  in
    ([], maxdist)


let get_candidates_cont foldf map nm acc =
  foldf (fun k _ (cand, mindist) ->
    let dist = edit_distance nm k mindist in
    if dist < mindist then
      ([k], dist)
    else if dist = mindist then
      (k :: cand, mindist)
    else
      (cand, mindist)
  ) map acc


let get_candidates_first foldf map nm =
  get_candidates_cont foldf map nm (initial_candidates nm)


let get_candidates_last pair =
  fst pair


let get_candidates foldf map nm =
  get_candidates_last @@ get_candidates_first foldf map nm


(* PUBLIC *)
let add (tyenv : t) (varnm : var_name) ((pty, evid) : poly_type * EvalVarID.t) : t =
  let addrlst = Alist.to_list tyenv.current_address in
  let mtr = tyenv.main_tree in
  match ModuleTree.update mtr addrlst (update_vt (VarMap.add varnm (pty, evid))) with
  | None         -> assert false
  | Some(mtrnew) -> { tyenv with main_tree = mtrnew; }


(* PUBLIC *)
let find (tyenv : t) (mdlnmlst : module_name list) (varnm : var_name) (rng : Range.t) : (poly_type * EvalVarID.t) option =
  let open OptionMonad in
  let nmtoid = tyenv.name_to_id_map in
  let mtr = tyenv.main_tree in
  let addrlst = Alist.to_list tyenv.current_address in
  let addrlast =
    mdlnmlst |> List.map (fun nm ->
      match nmtoid |> ModuleNameMap.find_opt nm with
      | None        -> raise (UndefinedModuleName(rng, nm, get_candidates ModuleNameMap.fold nmtoid nm))
      | Some(mdlid) -> mdlid
    )
  in
  let addrstr = String.concat "" (List.map (fun m -> ModuleID.extract_name m ^ ".") addrlast) in  (* for debug *)
    ModuleTree.search_backward mtr addrlst addrlast (fun (vdmap, _, _, sigopt) ->
      match sigopt with
      | None ->
          print_for_debug_variantenv ("FVD " ^ addrstr ^ varnm ^ " -> no signature"); (* for debug *)
          VarMap.find_opt varnm vdmap

      | Some((_, vtmapsig)) ->
          print_for_debug_variantenv ("FVD " ^ addrstr ^ varnm ^ " -> signature found"); (* for debug *)
          VarMap.find_opt varnm vtmapsig >>= fun ptysig ->
          VarMap.find_opt varnm vdmap >>= fun (_, evid) ->
          return (ptysig, evid)
    )


(* PUBLIC *)
let find_candidates (tyenv : t) (mdlnmlst : module_name list) (varnm : var_name) (rng : Range.t) : var_name list =
  let open OptionMonad in
  let nmtoid = tyenv.name_to_id_map in
  let mtr = tyenv.main_tree in
  let addrlst = Alist.to_list tyenv.current_address in
  let addrlast =
    mdlnmlst |> List.map (fun nm ->
      match nmtoid |> ModuleNameMap.find_opt nm with
      | None        -> assert false
      | Some(mdlid) -> mdlid
    )
  in
    get_candidates_last @@ ModuleTree.fold_backward mtr addrlst addrlast (fun acc (vdmap, _, _, sigopt) ->
      get_candidates_cont VarMap.fold vdmap varnm acc
    ) (initial_candidates varnm)


let open_module (tyenv : t) (rng : Range.t) (mdlnm : module_name) =
  let open OptionMonad in
  let mtr = tyenv.main_tree in
  let nmtoid = tyenv.name_to_id_map in
  let addrlst = Alist.to_list tyenv.current_address in
  let mtropt =
    nmtoid |> ModuleNameMap.find_opt mdlnm >>= fun mdlid ->
    ModuleTree.search_backward mtr addrlst [mdlid] (fun (vdmapC, tdmapC, cdmapC, sigopt) ->
      match sigopt with
      | Some((tdmapsig, vtmapsig)) ->
        (* -- if the opened module has a signature -- *)
          ModuleTree.update mtr addrlst (update_td (TyNameMap.fold TyNameMap.add tdmapsig)) >>= fun mtr ->
          ModuleTree.update mtr addrlst (update_vt @@
            VarMap.fold (fun varnm pty vdmapU ->
              match vdmapC |> VarMap.find_opt varnm with
              | None ->
                  assert false
                    (* -- signature must be less general than its corresponding implementation -- *)

              | Some((_, evid)) ->
                  vdmapU |> VarMap.add varnm (pty, evid)
            ) vtmapsig
          )

      | None ->
        (* -- if the opened module does NOT have a signature -- *)
          ModuleTree.update mtr addrlst (update_td (TyNameMap.fold TyNameMap.add tdmapC)) >>= fun mtr ->
          ModuleTree.update mtr addrlst (update_vt (VarMap.fold VarMap.add vdmapC)) >>= fun mtr ->
          ModuleTree.update mtr addrlst (update_cd (ConstrMap.fold ConstrMap.add cdmapC))
    )
  in
    match mtropt with
    | None         -> raise (UndefinedModuleName(rng, mdlnm, get_candidates ModuleNameMap.fold nmtoid mdlnm))
    | Some(mtrnew) -> { tyenv with main_tree = mtrnew; }


let find_for_inner (tyenv : t) (varnm : var_name) : (poly_type * EvalVarID.t) option =
  let open OptionMonad in
  let mtr = tyenv.main_tree in
  let addrlst = Alist.to_list tyenv.current_address in
  ModuleTree.find_stage mtr addrlst >>= fun (vdmap, _, _, _) ->
  VarMap.find_opt varnm vdmap


let enter_new_module (tyenv : t) (mdlnm : module_name) : t =
  let mdlid = ModuleID.fresh mdlnm in
  let mtr = tyenv.main_tree in
  let addr = tyenv.current_address in
  let addrnew = Alist.extend addr mdlid in
    match ModuleTree.add_stage mtr (Alist.to_list addr) mdlid (VarMap.empty, TyNameMap.empty, ConstrMap.empty, None) with
    | None         -> assert false
    | Some(mtrnew) -> { tyenv with current_address = addrnew; main_tree = mtrnew; }


let leave_module (tyenv : t) : t =
  match Alist.chop_last tyenv.current_address with
  | None ->
      assert false

  | Some((addr_outer, mdlid)) ->
      let mdlnm = ModuleID.extract_name mdlid in
      let nmtoidnew = ModuleNameMap.add mdlnm mdlid tyenv.name_to_id_map in
        { tyenv with current_address = addr_outer; name_to_id_map = nmtoidnew; }


module MapList
: sig
    type ('a, 'b) t
    val create : unit -> ('a, 'b) t
    val add : ('a, 'b) t -> 'a -> 'b -> unit
    val find_opt : ('a, 'b) t -> 'a -> 'b option
    val to_list : ('a, 'b) t -> ('a * 'b) list
    val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
  end
= struct
    type ('a, 'b) t = (('a * 'b) Alist.t) ref

    let create () = ref Alist.empty

    let add mapl k v = begin mapl := Alist.extend !mapl (k, v); end

    let find_opt mapl k = List.assoc_opt k (Alist.to_list (!mapl))

    let to_list mapl = Alist.to_list (!mapl)

    let fold f mapl init = Alist.fold_left (fun acc (k, v) -> f k v acc) init (!mapl)

  end


module DependencyGraph = DirectedGraph.Make
  (struct
    type t = type_name
    let compare = compare
    let show s = s (* for debug *)
  end)


type vertex_label =
  | VariantVertex of Range.t * TypeID.t * untyped_type_argument list * untyped_constructor_dec list
  | SynonymVertex of Range.t * TypeID.t * untyped_type_argument list * manual_type * (type_scheme option) ref


let extract_range_in_vertex_label vtxlabel =
  match vtxlabel with
  | VariantVertex(rng, _, _, _)    -> rng
  | SynonymVertex(rng, _, _, _, _) -> rng


type dependency_mode =
  | NoDependency
  | DependentMode of vertex_label DependencyGraph.t


let add_type_definition (tyenv : t) (tynm : type_name) ((tyid, dfn) : TypeID.t * type_definition) : t =
  let addrlst = Alist.to_list tyenv.current_address in
  let mtr = tyenv.main_tree in
    match ModuleTree.update mtr addrlst (update_td (TyNameMap.add tynm (tyid, dfn))) with
    | None         -> assert false
    | Some(mtrnew) -> { tyenv with main_tree = mtrnew; }


let find_type_definition_for_inner (tyenv : t) (tynm : type_name) : (TypeID.t * type_definition) option =
  let addrlst = Alist.to_list tyenv.current_address in
  let mtr = tyenv.main_tree in
    ModuleTree.search_backward mtr addrlst [] (fun (_, tdmap, _, _) ->
      TyNameMap.find_opt tynm tdmap
    )


let find_type_definition_for_outer (tyenv : t) (mdlnmlst : module_name list) (tynm : type_name) (rng : Range.t) : (TypeID.t * type_definition) option =
  let addrlst = Alist.to_list tyenv.current_address in
  let mtr = tyenv.main_tree in
  let nmtoid = tyenv.name_to_id_map in
  let mdlidlst =
    mdlnmlst |> List.map (fun mdlnm ->
      match nmtoid |> ModuleNameMap.find_opt mdlnm with
      | Some(mdlid) -> mdlid
      | None        -> raise (UndefinedModuleName(rng, mdlnm, get_candidates ModuleNameMap.fold nmtoid mdlnm))
    )
  in
  let straddr = String.concat "." (addrlst |> List.map ModuleID.extract_name) in (* for debug *)
    ModuleTree.search_backward mtr addrlst mdlidlst (fun (_, tdmap, _, sigopt) ->
      match sigopt with
      | None ->
          let () = print_for_debug_variantenv ("FTD " ^ straddr ^ ", " ^ tynm ^ " -> no signature") in (* for debug *)
          TyNameMap.find_opt tynm tdmap

      | Some((tdmapsig, _)) ->
          let () = print_for_debug_variantenv ("FTD " ^ straddr ^ ", " ^ tynm ^ " -> signature found") in (* for debug *)
          TyNameMap.find_opt tynm tdmapsig
    )


let find_type_definition_candidates_for_outer (tyenv : t) (mdlnmlst : module_name list) (tynm : type_name) (rng : Range.t) : type_name list =
  let addrlst = Alist.to_list tyenv.current_address in
  let mtr = tyenv.main_tree in
  let nmtoid = tyenv.name_to_id_map in
  let mdlidlst =
    mdlnmlst |> List.map (fun mdlnm ->
      match nmtoid |> ModuleNameMap.find_opt mdlnm with
      | Some(mdlid) -> mdlid
      | None        -> assert false
    )
  in
  let base_type_candidates = get_candidates_first Hashtbl.fold base_type_hash_table tynm in
    get_candidates_last @@ ModuleTree.fold_backward mtr addrlst mdlidlst (fun acc (_, tdmap, _, sigopt) ->
      match sigopt with
      | None                -> get_candidates_cont TyNameMap.fold tdmap tynm acc
      | Some((tdmapsig, _)) -> get_candidates_cont TyNameMap.fold tdmapsig tynm acc
    ) base_type_candidates


(* PUBLIC *)
let find_type_id (tyenv : t) (mdlnmlst : module_name list) (tynm : type_name) (rng : Range.t) : TypeID.t option =
  let open OptionMonad in
  find_type_definition_for_outer tyenv mdlnmlst tynm rng >>= fun (tyid, _) ->
  return tyid


(* PUBLIC *)
let find_type_name (_ : t) (tyid : TypeID.t) : type_name =
  TypeID.extract_name tyid


let add_constructor (constrnm : constructor_name) ((bidlist, pty) : type_scheme) (tyid : TypeID.t) (tyenv : t) : t =
  let addrlst = Alist.to_list tyenv.current_address in
  let mtr = tyenv.main_tree in
    match ModuleTree.update mtr addrlst (update_cd (ConstrMap.add constrnm (tyid, (bidlist, pty)))) with
    | None         -> assert false
    | Some(mtrnew) -> { tyenv with main_tree = mtrnew; }


let instantiate_type_scheme (type a) (type b) (freef : Range.t -> mono_type_variable_info ref -> (a, b) typ) (orfreef : mono_option_row_variable_info ref -> (a, b) option_row) (pairlst : ((a, b) typ * BoundID.t) list) (Poly(pty) : poly_type) : (a, b) typ =
  let bid_to_type_ht : ((a, b) typ) BoundIDHashTable.t = BoundIDHashTable.create 32 in

  let rec aux ((rng, ptymain) : poly_type_body) : (a, b) typ =
    match ptymain with
    | TypeVariable(ptvi) ->
        begin
          match ptvi with
          | PolyFree(tvref) ->
              freef rng tvref

          | PolyBound(bid) ->
              begin
                match BoundIDHashTable.find_opt bid_to_type_ht bid with
                | None        -> assert false
                | Some(tysub) -> tysub
              end
        end

    | FuncType(optrow, tydom, tycod)    -> (rng, FuncType(aux_or optrow, aux tydom, aux tycod))
    | ProductType(tylist)               -> (rng, ProductType(List.map aux tylist))
    | RecordType(tyasc)                 -> (rng, RecordType(Assoc.map_value aux tyasc))
    | SynonymType(tylist, tyid, tyreal) -> (rng, SynonymType(List.map aux tylist, tyid, aux tyreal))
    | VariantType(tylist, tyid)         -> (rng, VariantType(List.map aux tylist, tyid))
    | ListType(tysub)                   -> (rng, ListType(aux tysub))
    | RefType(tysub)                    -> (rng, RefType(aux tysub))
    | BaseType(bt)                      -> (rng, BaseType(bt))
    | HorzCommandType(tylist)           -> (rng, HorzCommandType(List.map (lift_argument_type aux) tylist))
    | VertCommandType(tylist)           -> (rng, VertCommandType(List.map (lift_argument_type aux) tylist))
    | MathCommandType(tylist)           -> (rng, MathCommandType(List.map (lift_argument_type aux) tylist))

  and aux_or optrow =
    match optrow with
    | OptionRowEmpty                         -> OptionRowEmpty
    | OptionRowCons(ty, tail)                -> OptionRowCons(aux ty, aux_or tail)
    | OptionRowVariable(PolyORFree(orviref)) -> orfreef orviref
  in
  begin
    pairlst |> List.iter (fun (tyarg, bid) -> BoundIDHashTable.add bid_to_type_ht bid tyarg);
    aux pty
  end


let rec fix_manual_type_general (type a) (type b) (dpmode : dependency_mode) (tyenv : t) (lev : level) (freef : Range.t -> mono_type_variable_info ref -> (a, b) typ) (orfreef : mono_option_row_variable_info ref -> (a, b) option_row) (typaramf : Range.t -> string -> (a, b) type_main) (mnty : manual_type) : (a, b) typ =

  let rec aux (mnty : manual_type) : (a, b) typ =
    let (rng, mntymain) = mnty in
    let error tynm lenexp lenerr =
      raise (IllegalNumberOfTypeArguments(rng, tynm, lenexp, lenerr))
    in
    let ptymainnew =
      match mntymain with

      | MFuncType(mntyopts, mntydom, mntycod) -> FuncType(aux_or mntyopts, aux mntydom, aux mntycod)
      | MProductType(mntylist)           -> ProductType(List.map aux mntylist)
      | MRecordType(mnasc)               -> RecordType(Assoc.map_value aux mnasc)

      | MHorzCommandType(mncmdargtylist) -> HorzCommandType(List.map aux_cmd mncmdargtylist)
      | MVertCommandType(mncmdargtylist) -> VertCommandType(List.map aux_cmd mncmdargtylist)
      | MMathCommandType(mncmdargtylist) -> MathCommandType(List.map aux_cmd mncmdargtylist)

      | MTypeName([], [], tynm)  when tynm |> Hashtbl.mem base_type_hash_table ->
          begin
            match Hashtbl.find_opt base_type_hash_table tynm with
            | None     -> assert false
            | Some(bt) -> BaseType(bt)
          end

      | MTypeName(mntyarglist, [], tynm)  when tynm |> Hashtbl.mem base_type_hash_table ->
          error tynm 0 (List.length mntyarglist)

      | MTypeName(mntyarg :: [], [], "list") -> ListType(aux mntyarg)
      | MTypeName(mntyarglist, [], "list")   -> error "list" 1 (List.length mntyarglist)
      | MTypeName(mntyarg :: [], [], "ref")  -> RefType(aux mntyarg)
      | MTypeName(mntyarglist, [], "ref")    -> error "ref" 1 (List.length mntyarglist)
      | MTypeName(mntyarglist, mdlnmlst, tynm) ->
          let len = List.length mntyarglist in
          let tyarglist = List.map aux mntyarglist in
          let find_in_variant_environment () =
            match find_type_definition_for_outer tyenv mdlnmlst tynm rng with
            | None ->
                raise (UndefinedTypeName(rng, mdlnmlst, tynm, find_type_definition_candidates_for_outer tyenv mdlnmlst tynm rng))

            | Some((tyid, Data(lenexp))) ->
                if lenexp <> len then error tynm lenexp len else
                  let () = print_for_debug_variantenv ("FV " ^ tynm ^ " -> " ^ TypeID.show_direct tyid) in (* for debug *)
                    VariantType(List.map aux mntyarglist, tyid)

            | Some((tyid, Alias(bidlist, ptyscheme))) ->
                begin
                  try
                    let pairlst = List.combine tyarglist bidlist in
                    let tyreal = instantiate_type_scheme freef orfreef pairlst ptyscheme in
                    let () = print_for_debug_variantenv ("FS " ^ tynm ^ " -> " ^ TypeID.show_direct tyid) in (* for debug *)
                      SynonymType(tyarglist, tyid, tyreal)
                  with
                  | Invalid_argument(_) ->
                      let lenexp = List.length bidlist in
                        error tynm lenexp len
                end

          in
          begin
            match dpmode with
            | NoDependency ->
                find_in_variant_environment ()

            | DependentMode(dg) ->
                begin
                  try
                    match DependencyGraph.find_vertex dg tynm with

                    | VariantVertex(_, tyid, tyargcons, utvarntcons) ->
                        let lenexp = List.length tyargcons in
                          if len <> lenexp then error tynm lenexp len else
                            VariantType(tyarglist, tyid)

                    | SynonymVertex(_, tyid, tyargcons, mnty, {contents= Some(bidlist, ptyscheme)}) ->
                        begin
                          try
                            let pairlst = List.combine tyarglist bidlist in
                              let tyreal = instantiate_type_scheme freef orfreef pairlst ptyscheme in
                                SynonymType(tyarglist, tyid, tyreal)
                          with
                          | Invalid_argument(_) ->
                              let lenexp = List.length tyargcons in
                                error tynm lenexp len
                        end

                    | SynonymVertex(_, _, _, _, {contents= None}) ->
                        assert false

                  with
                  | DependencyGraph.UndefinedSourceVertex -> find_in_variant_environment ()
                end
          end

      | MTypeParam(tyargnm) ->
          typaramf rng tyargnm

    in
      (rng, ptymainnew)


  and aux_cmd = function
    | MMandatoryArgumentType(mnty) -> MandatoryArgumentType(aux mnty)
    | MOptionalArgumentType(mnty)  -> OptionalArgumentType(aux mnty)

  and aux_or mntyopts =
    List.fold_right (fun mnty acc ->
      OptionRowCons(aux mnty, acc)
    ) mntyopts OptionRowEmpty

  in
  aux mnty


and fix_manual_kind_general (dpmode : dependency_mode) (tyenv : t) (lev : level) freef orfreef typaramf (mnkd : manual_kind) =
  match mnkd with
  | MUniversalKind       -> UniversalKind
  | MRecordKind(mntyasc) -> RecordKind(Assoc.map_value (fix_manual_type_general dpmode tyenv lev freef orfreef typaramf) mntyasc)


let fix_manual_type (dpmode : dependency_mode) (tyenv : t) (lev : level) (tyargcons : untyped_type_argument list) (mnty : manual_type) : BoundID.t list * poly_type =
  let bidmaplist = MapList.create () in
  let freef rng tvref =
    (rng, TypeVariable(PolyFree(tvref)))
  in
  let orfreef orviref =
    OptionRowVariable(PolyORFree(orviref))
  in
  let typaramf rng param =
    match MapList.find_opt bidmaplist param with
    | None      -> raise (UndefinedTypeArgument(rng, param, get_candidates MapList.fold bidmaplist param))
    | Some(bid) -> TypeVariable(PolyBound(bid))
  in
  let rec aux cons =
    match cons with
    | [] ->
        ()

    | (_, tyargnm, mnkd) :: tailcons ->
       let kd = fix_manual_kind_general dpmode tyenv lev freef orfreef typaramf mnkd in
(*
       let () = print_for_debug_variantenv ("FMT " ^ tyargnm ^ " :: " ^ (string_of_kind string_of_mono_type_basic kd)) in (* for debug *)
*)
       let bid = BoundID.fresh kd () in
       begin
         MapList.add bidmaplist tyargnm bid;
         aux tailcons
       end
  in
  begin
    aux tyargcons;
    let pty = fix_manual_type_general dpmode tyenv lev freef orfreef typaramf mnty in
    let bidlist = MapList.to_list bidmaplist |> List.map (fun (_, bid) -> bid) in
    (bidlist, Poly(pty))
  end


(* PUBLIC *)
let fix_manual_type_free (qtfbl : quantifiability) (tyenv : t) (lev : level) (mnty : manual_type) (constrnts : constraints) : mono_type =

  let tyargmaplist : (string, mono_type_variable_info ref) MapList.t = MapList.create () in

  let freef rng tvref =
    (rng, TypeVariable(tvref))
  in
  let orfreef orviref =
    OptionRowVariable(orviref)
  in
  let typaramf rng param =
    match MapList.find_opt tyargmaplist param with
    | Some(tvref) ->
        TypeVariable(tvref)

    | None ->
        let tvid = FreeID.fresh UniversalKind qtfbl lev () in
        let tvref = ref (MonoFree(tvid)) in
        begin
          MapList.add tyargmaplist param tvref;
          TypeVariable(tvref)
        end
  in

  let () =
    constrnts |> List.iter (fun (param, mkd) ->
      let kd = fix_manual_kind_general NoDependency tyenv lev freef orfreef typaramf mkd in
      let tvid = FreeID.fresh kd qtfbl lev () in
      let tvref = ref (MonoFree(tvid)) in
        MapList.add tyargmaplist param tvref
    )
  in

  let ty = fix_manual_type_general NoDependency tyenv lev freef orfreef typaramf mnty in
    ty


let register_type (tynm : type_name) (tyid : TypeID.t) (dfn : type_definition) (tyenv : t) : t =
  let addrlst = Alist.to_list tyenv.current_address in
  let mtr = tyenv.main_tree in
    match ModuleTree.update mtr addrlst (update_td (TyNameMap.add tynm (tyid, dfn))) with
    | None         -> assert false  (* raise (UndefinedModuleNameList(addrlst |> List.map ModuleID.extract_name)) *)
    | Some(mtrnew) -> { tyenv with main_tree = mtrnew; }


let register_type_from_vertex (dg : vertex_label DependencyGraph.t) (tyenv : t) (tynm : type_name) : t =

  let () = print_for_debug_variantenv ("Register " ^ tynm) in (* for debug *)

    try
      match DependencyGraph.find_vertex dg tynm with

      | VariantVertex(_, tyid, tyargcons, _) ->
          let len = List.length tyargcons in
            register_type tynm tyid (Data(len)) tyenv

      | SynonymVertex(_, tyid, _, _, {contents= Some((bidlist, ptyscheme))}) ->
          register_type tynm tyid (Alias(bidlist, ptyscheme)) tyenv

      | SynonymVertex(_, _, _, _, {contents= None}) -> assert false
    with
    | DependencyGraph.UndefinedSourceVertex -> failwith ("'" ^ tynm ^ "' not defined")


let rec find_constructor (qtfbl : quantifiability) (tyenv : t) (lev : level) (constrnm : constructor_name) : (mono_type list * TypeID.t * mono_type) option =
  let freef rng tvref =
    (rng, TypeVariable(tvref))
  in
  let orfreef orviref =
    OptionRowVariable(orviref)
  in
  let addrlst = Alist.to_list tyenv.current_address in
  let mtr = tyenv.main_tree in
  let open OptionMonad in
    ModuleTree.search_backward mtr addrlst [] (fun (_, _, cdmap, _) -> ConstrMap.find_opt constrnm cdmap) >>= fun dfn ->
    let (tyid, (bidlist, pty)) = dfn in
    let pairlst =
      bidlist |> List.map (fun bid ->
        let kd = BoundID.get_kind bid in
        let tvid = FreeID.fresh (instantiate_kind lev qtfbl kd) qtfbl lev () in
        let ty = (Range.dummy "tc-constructor", TypeVariable(ref (MonoFree(tvid)))) in
        (ty, bid)
      )
    in
    let ty = instantiate_type_scheme freef orfreef pairlst pty in
    let tyarglst = pairlst |> List.map (fun (ty, _) -> ty) in
      return (tyarglst, tyid, ty)


let rec enumerate_constructors (qtfbl : quantifiability) (tyenv : t) (lev : level) (typeid : TypeID.t)
    : (constructor_name * (mono_type list -> mono_type)) list =
  let freef rng tvref =
    (rng, TypeVariable(tvref))
  in
  let orfreef orviref =
    OptionRowVariable(orviref)
  in
  let addrlst = Alist.to_list tyenv.current_address in
  let mtr = tyenv.main_tree in
  let open OptionMonad in
  let constrs =
    ModuleTree.search_backward mtr addrlst [] (fun (_, _, cdmap, _) ->
      let constrs = ConstrMap.fold (fun constrnm dfn acc ->
        let (tyid, (bidlist, pty)) = dfn in
          if TypeID.equal typeid tyid then
            (constrnm, (fun tyarglist -> instantiate_type_scheme freef orfreef (List.combine tyarglist bidlist) pty)) :: acc
          else
            acc
        ) cdmap []
      in
      match constrs with
      | [] -> None
      | _  -> Some(constrs))
  in
  match constrs with
  | Some(lst) -> lst
  | None      -> []


let rec find_constructor_candidates (qtfbl : quantifiability) (tyenv : t) (lev : level) (constrnm : constructor_name) : constructor_name list =
  let open OptionMonad in
  let addrlst = Alist.to_list tyenv.current_address in
  let mtr = tyenv.main_tree in
    get_candidates_last @@ ModuleTree.fold_backward mtr addrlst [] (fun acc (_, _, cdmap, _) ->
      get_candidates_cont ConstrMap.fold cdmap constrnm acc
    ) (initial_candidates constrnm)


let get_moduled_var_name (tyenv : t) (varnm : var_name) =
  varnm |> (tyenv.current_address |> Alist.map ModuleID.extract_name |> Alist.fold_right (fun s mdlnm -> s ^ "." ^ mdlnm))


let get_moduled_type_name (tyenv : t) (tynm : type_name) =
  tynm |> (tyenv.current_address |> Alist.map ModuleID.extract_name |> Alist.fold_right (fun s mdlnm -> s ^ "." ^ mdlnm))


(* PUBLIC *)
let rec add_mutual_cons (tyenv : t) (lev : level) (mutvarntcons : untyped_mutual_variant_cons) =

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
        | MFuncType(mntyopts, mtydom, mtycod) -> begin List.iter iter mntyopts; iter mtydom; iter mtycod; end
        | MProductType(mtylist)       -> List.iter iter mtylist
        | MRecordType(mtyasc)         -> Assoc.iter_value iter mtyasc
        | MHorzCommandType(mncmdargtylist) -> List.iter (lift_manual_common iter) mncmdargtylist
        | MVertCommandType(mncmdargtylist) -> List.iter (lift_manual_common iter) mncmdargtylist
        | MMathCommandType(mncmdargtylist) -> List.iter (lift_manual_common iter) mncmdargtylist
        | MTypeName(mtyarglist, [], tynm) ->
            if DependencyGraph.mem_vertex tynm dg then
              begin
                DependencyGraph.add_edge dg tynm1 tynm;
                print_for_debug_variantenv ("AddE " ^ tynm1 ^ " ---> " ^ tynm); (* for debug *)
                List.iter iter mtyarglist;
              end
            else
              ()

        | MTypeName(_, mdlnmlst, tynm) ->
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


let rec poly_type_equal (pty1 : poly_type_body) (pty2 : poly_type_body) =

  let combine p lst1 lst2 =
    try let lst = List.combine lst1 lst2 in p lst with Invalid_argument(_) -> false
  in

  let rec iter ((_, ptymain1) as pty1 : poly_type_body) ((_, ptymain2) as pty2 : poly_type_body) =
    match (ptymain1, ptymain2) with
    | (BaseType(bt1), BaseType(bt2)) ->
        bt1 = bt2

    | (SynonymType(tyl1, tyid1, tyreal1), _) ->
        iter tyreal1 pty2

    | (_, SynonymType(tyl2, tyid2, tyreal2)) ->
        iter pty1 tyreal2

    | (TypeVariable(PolyBound(bid1)), TypeVariable(PolyBound(bid2))) ->
        BoundID.eq bid1 bid2

    | (TypeVariable(PolyFree(_)), TypeVariable(PolyFree(_))) ->
        false  (* -- does not handle free variables -- *)

    | (FuncType(optrow1, ty1d, ty1c), FuncType(optrow2, ty2d, ty2c)) ->
        (iter_or optrow1 optrow2) && iter ty1d ty2d && iter ty1c ty2c

    | (ProductType(tylst1), ProductType(tylst2)) ->
        combine iter_list tylst1 tylst2

    | (RecordType(tyasc1), RecordType(tyasc2)) ->
        (Assoc.domain_same tyasc1 tyasc2) && iter_list (Assoc.combine_value tyasc1 tyasc2)

    | (ListType(ty1sub), ListType(ty2sub))
    | (RefType(ty1sub), RefType(ty2sub)) ->
        iter ty1sub ty2sub

    | (VariantType(tylst1, tyid1), VariantType(tylst2, tyid2)) ->
        TypeID.equal tyid1 tyid2 && (combine iter_list tylst1 tylst2)

    | (HorzCommandType(catyl1), HorzCommandType(catyl2))
    | (VertCommandType(catyl1), VertCommandType(catyl2))
    | (MathCommandType(catyl1), MathCommandType(catyl2))
        ->
        begin
          try
            List.fold_left2 (fun b caty1 caty2 ->
              match (caty1, caty2) with
              | (MandatoryArgumentType(ty1), MandatoryArgumentType(ty2))
              | (OptionalArgumentType(ty1) , OptionalArgumentType(ty2) )
                  -> b && iter ty1 ty2
              | _ -> false
            ) true catyl1 catyl2
          with
          | Invalid_argument(_) -> false
        end

    | _ -> false

  and iter_list lst =
    lst |> List.fold_left (fun b (pty1, pty2) -> b && iter pty1 pty2) true

  and iter_or optrow1 optrow2 =
    match (optrow1, optrow2) with
    | (OptionRowEmpty, OptionRowEmpty)                       -> true
    | (OptionRowCons(ty1, tail1), OptionRowCons(ty2, tail2)) -> iter ty1 ty2 && iter_or tail1 tail2
    | (OptionRowVariable(_), OptionRowVariable(_))           -> false  (* -- does not handle free variables -- *)
    | _                                                      -> false

  in
  iter pty1 pty2


(* -- 'reflects pty1 pty2' returns whether 'pty2' is more general than 'pty1' -- *)
let reflects (Poly(pty1) : poly_type) (Poly(pty2) : poly_type) : bool =
(*
  let current_ht : BoundID.t BoundIDHashtbl.t = BoundIDHashtbl.create 32 in
    (* -- hash table mapping bound IDs in 'pty2' to bound IDs in 'pty1' -- *)
*)
  let current_bid_to_ty : poly_type_body BoundIDHashTable.t = BoundIDHashTable.create 32 in
    (* -- hash table mapping bound IDs in 'pty2' to types -- *)

  let rec aux ((_, tymain1) as ty1 : poly_type_body) ((_, tymain2) as ty2 : poly_type_body) =
(*
    let () = print_for_debug_variantenv ("reflects " ^ (string_of_mono_type_basic ty1) ^ " << " ^ (string_of_mono_type_basic ty2)) in (* for debug *)
*)
    let aux_list tylistcomb =
      tylistcomb |> List.fold_left (fun b (ty1, ty2) -> b && aux ty1 ty2) true
    in

    match (tymain1, tymain2) with
    | (SynonymType(tyl1, tyid1, tyreal1), _)               -> aux tyreal1 ty2
    | (_, SynonymType(tyl2, tyid2, tyreal2))               -> aux ty1 tyreal2

    | (TypeVariable(PolyFree({contents= MonoLink(tylink1)})), _) -> aux (lift_poly tylink1 |> (function Poly(pty) -> pty)) ty2
    | (_, TypeVariable(PolyFree({contents= MonoLink(tylink2)}))) -> aux ty1 (lift_poly tylink2 |> (function Poly(pty) -> pty))

    | (TypeVariable(PolyBound(bid1)), TypeVariable(PolyBound(bid2))) ->
        begin
          match BoundIDHashTable.find_opt current_bid_to_ty bid2 with
          | Some(tyold) ->
              poly_type_equal ty1 tyold

          | None ->
              if is_stronger_kind (BoundID.get_kind bid1) (BoundID.get_kind bid2) then
                begin BoundIDHashTable.add current_bid_to_ty bid2 ty1; true end
              else
                false
        end

    | (RecordType(tyasc1), TypeVariable(PolyBound(bid2))) ->
        begin
          match BoundIDHashTable.find_opt current_bid_to_ty bid2 with
          | Some(tyold) ->
              poly_type_equal ty1 tyold

          | None ->
              let kd2 = BoundID.get_kind bid2 in
              let binc =
                match kd2 with
                | UniversalKind      -> true
                | RecordKind(tyasc2) -> Assoc.domain_included tyasc2 tyasc1
              in
              if not binc then false else
                begin
                  BoundIDHashTable.add current_bid_to_ty bid2 ty1;
                  true
                end
        end

    | (_, TypeVariable(PolyBound(bid2))) ->
        begin
          match BoundIDHashTable.find_opt current_bid_to_ty bid2 with
          | Some(tyold) ->
              poly_type_equal ty1 tyold

          | None ->
              let kd2 = BoundID.get_kind bid2 in
              begin
                match kd2 with
                | UniversalKind -> begin BoundIDHashTable.add current_bid_to_ty bid2 ty1; true end
                | RecordKind(_) -> false
              end
        end

    | (RecordType(tyasc1), TypeVariable(PolyFree({contents= MonoFree(tvid2)} as tvref))) ->
        let kd2 = FreeID.get_kind tvid2 in
        let binc =
          match kd2 with
          | UniversalKind      -> true
          | RecordKind(tyasc2) -> Assoc.domain_included tyasc1 tyasc2
        in
        if binc then
          match unlift_poly ty1 with
          | None      -> false
          | Some(ty1) -> tvref := MonoLink(ty1); true
        else
          false

    | (_, TypeVariable(PolyFree({contents= MonoFree(tvid2)} as tvref))) ->
        let kd2 = FreeID.get_kind tvid2 in
        let binc =
          match kd2 with
          | UniversalKind -> true
          | RecordKind(_) -> false
        in
        if binc then
          match unlift_poly ty1 with
          | None      -> false
          | Some(ty1) -> tvref := MonoLink(ty1); true
        else
          false

    | (FuncType(optrow1, tyd1, tyc1), FuncType(optrow2, tyd2, tyc2)) ->
        (aux_or optrow1 optrow2) && (aux tyd1 tyd2) && (aux tyc1 tyc2)
          (* -- both domain and codomain are covariant -- *)

    | (HorzCommandType(catyl1), HorzCommandType(catyl2))
    | (VertCommandType(catyl1), VertCommandType(catyl2))
    | (MathCommandType(catyl1), MathCommandType(catyl2))
      ->
        begin
          try
            List.fold_left2 (fun b caty1 caty2 ->
              match (caty1, caty2) with
              | (MandatoryArgumentType(ty1), MandatoryArgumentType(ty2))
              | (OptionalArgumentType(ty1) , OptionalArgumentType(ty2) )
                  -> b && aux ty1 ty2
              | _ -> false
            ) true catyl1 catyl2
          with
          | Invalid_argument(_) -> false
        end

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

  and aux_or optrow1 optrow2 =
    match (optrow1, optrow2) with
    | (_, OptionRowEmpty) ->
        true

    | (OptionRowEmpty, OptionRowCons(_)) ->
        false

    | (OptionRowCons(ty1, tail1), OptionRowCons(ty2, tail2)) ->
        if aux ty1 ty2 then aux_or tail1 tail2 else false

    | (_, OptionRowVariable(PolyORFree(orviref))) ->
        begin
          match unlift_option_row optrow1 with
          | None          -> false
          | Some(optrow1) -> orviref := MonoORLink(optrow1); true
        end

    | (OptionRowVariable(_), _) ->
        false

  and is_stronger_kind (kd1 : poly_kind) (kd2 : poly_kind) =
    match (kd1, kd2) with
    | (_, UniversalKind)                       -> true
    | (UniversalKind, _)                       -> false
    | (RecordKind(tyasc1), RecordKind(tyasc2)) ->
        begin
          tyasc2 |> Assoc.fold (fun b k pty2 ->
            match Assoc.find_opt tyasc1 k with
            | Some(pty1) -> b && (aux pty1 pty2)
            | None       -> false
          ) true
        end
  in
  let b = aux pty1 pty2 in
(*
  begin
    if b then
      let pairlst =
        BoundIDHashTable.fold (fun bid ty acc ->
          Alist.extend acc (ty, bid)
        ) current_bid_to_ty Alist.empty |> Alist.to_list
      in
      instantiate
    else ()
  end;
*)
  b


let sigcheck (rng : Range.t) (qtfbl : quantifiability) (lev : level) (tyenv : t) (tyenvprev : t) (msigopt : manual_signature option) =

  let rec read_manual_signature (tyenvacc : t) (tyenvforsigI : t) (tyenvforsigO : t) (msig : manual_signature) (sigoptacc : signature option) =
    let iter = read_manual_signature in
      match msig with
      | [] -> (sigoptacc, tyenvacc)

      | SigType(tyargcons, tynm) :: tail ->
          let () = print_for_debug_variantenv ("SIGT " ^ tynm) in (* for debug *)
          begin
            match find_type_definition_for_inner tyenv tynm with
            | None ->
                raise (NotProvidingTypeImplementation(rng, tynm))

            | Some((tyid, dfn)) ->
                let tyenvforsigInew = add_type_definition tyenvforsigI tynm (tyid, dfn) in
                let len = List.length tyargcons in (* temporary; should check whether len is valid as to dfn *)
                let tyidout = TypeID.fresh (get_moduled_type_name tyenv tynm) in
                let sigoptaccnew = add_type_to_signature sigoptacc tynm tyidout len in
                let tyenvforsigOnew =
                  let tyenvsub = add_type_definition tyenvforsigO tynm (tyid, dfn) in
                  let addrlst = Alist.to_list tyenvsub.current_address in
                  let mtr = tyenvsub.main_tree in
                  match ModuleTree.update mtr addrlst (update_so (fun _ -> sigoptaccnew)) with
                  | None         -> assert false
                  | Some(mtrnew) -> { tyenvsub with main_tree = mtrnew; }
                in
                  iter tyenvacc tyenvforsigInew tyenvforsigOnew tail sigoptaccnew
          end

      | SigValue(varnm, mty, constrntcons) :: tail ->
          let () = print_for_debug_variantenv ("SIGV " ^ varnm) in (* for debug *)
          let tysigI = fix_manual_type_free qtfbl tyenvforsigI (Level.succ lev) mty constrntcons in
          let ptysigI = generalize lev tysigI in
          let tysigO = fix_manual_type_free qtfbl tyenvforsigO (Level.succ lev) mty constrntcons in
          let ptysigO = generalize lev tysigO in
          let () = print_for_debug_variantenv ("LEVEL " ^ (Level.show lev) ^ "; " ^ (string_of_mono_type_basic tysigI) ^ " ----> " ^ (string_of_poly_type_basic ptysigI)) in (* for debug *)
          begin
            match find_for_inner tyenv varnm with
            | None ->
                raise (NotProvidingValueImplementation(rng, varnm))

            | Some((ptyimp, _)) ->
                let b = reflects ptysigI ptyimp in
                if b then
                  let sigoptaccnew = add_val_to_signature sigoptacc varnm ptysigO in
                    iter tyenvacc tyenvforsigI tyenvforsigO tail sigoptaccnew
                else
                  raise (NotMatchingInterface(rng, varnm, tyenv, ptyimp, tyenvforsigO, ptysigO))
          end

      | SigDirect(csnm, mty, constrntcons) :: tail ->
          let () = print_for_debug_variantenv ("SIGD " ^ csnm) in (* for debug *)
(*
          let () = print_for_debug_variantenv ("D-OK0 " ^ (string_of_manual_type mty)) in (* for debug *)
*)
          let tysigI = fix_manual_type_free qtfbl tyenvforsigI (Level.succ lev) mty constrntcons in
          let () = print_for_debug_variantenv "D-OK1" in (* for debug *)
          let ptysigI = generalize lev tysigI in
          let tysigO = fix_manual_type_free qtfbl tyenvforsigO (Level.succ lev) mty constrntcons in
          let () = print_for_debug_variantenv "D-OK2" in (* for debug *)
          let ptysigO = generalize lev tysigO in
          let () = print_for_debug_variantenv ("LEVEL " ^ (Level.show lev) ^ "; " ^ (string_of_mono_type_basic tysigI) ^ " ----> " ^ (string_of_poly_type_basic ptysigI)) in (* for debug *)
          begin
            match find_for_inner tyenv csnm with
            | None ->
                raise (NotProvidingValueImplementation(rng, csnm))

            | Some((ptyimp, evidimp)) ->
                let b = reflects ptysigI ptyimp in
                  (* -- 'reflects pty1 pty2' may change 'pty2' -- *)
                if b then
                  let sigoptaccnew = add_val_to_signature sigoptacc csnm ptysigO in
                  let tyenvaccnew =
                    let mtr = tyenvacc.main_tree in
                    match ModuleTree.update mtr [] (update_vt (VarMap.add csnm (ptysigO, evidimp))) with
                    | None         -> assert false
                    | Some(mtrnew) -> { tyenvacc with main_tree = mtrnew; }
                  in
                    iter tyenvaccnew tyenvforsigI tyenvforsigO tail sigoptaccnew
                else
                  raise (NotMatchingInterface(rng, csnm, tyenv, ptyimp, tyenvforsigO, ptysigO))
          end
  in

    match msigopt with
    | None ->
        tyenv

    | Some(msig) ->
        let sigoptini = Some((TyNameMap.empty, VarMap.empty)) in
        match
          let open OptionMonad in
          Alist.chop_last tyenv.current_address >>= fun (addr_outer, mdlid) ->
          let addrlstprev = Alist.to_list tyenvprev.current_address in
          let mtrprev = tyenvprev.main_tree in
          ModuleTree.add_stage mtrprev addrlstprev mdlid
            (VarMap.empty, TyNameMap.empty, ConstrMap.empty, None) >>= fun mtrprevInew ->
          ModuleTree.add_stage mtrprev addrlstprev mdlid
            (VarMap.empty, TyNameMap.empty, ConstrMap.empty, sigoptini) >>= fun mtrprevOnew ->
          return ({ tyenv with main_tree = mtrprevInew; }, { tyenv with main_tree = mtrprevOnew; })
        with
        | None -> assert false
        | Some((tyenvforsigIini, tyenvforsigOini)) ->
            let (sigopt, tyenvdir) = read_manual_signature tyenv tyenvforsigIini tyenvforsigOini msig sigoptini in
            let addrlst = Alist.to_list tyenvdir.current_address in
            let mtr = tyenvdir.main_tree in
              match ModuleTree.update mtr addrlst (update_so (fun _ -> sigopt)) with
              | None         -> assert false  (* raise (UndefinedModuleNameList(addrlst |> List.map ModuleID.extract_name)) *)
              | Some(mtrnew) -> { tyenvdir with main_tree = mtrnew; }


module Raw =
  struct
    let fresh_type_id = TypeID.fresh
    let add_constructor = add_constructor
    let register_type = register_type
  end
