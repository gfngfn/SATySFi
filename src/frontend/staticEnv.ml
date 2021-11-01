
open MyUtil
open Types


module Distance = struct

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

end


module ValueNameMap = Map.Make(String)

module TypeNameMap = Map.Make(String)

module ModuleNameMap = Map.Make(String)

module SignatureNameMap = Map.Make(String)

module ConstructorMap = Map.Make(String)

module MacroNameMap = Map.Make(String)

type type_scheme = BoundID.t list * poly_type

type value_entry = {
  val_name  : EvalVarID.t option;
  val_type  : poly_type;
  val_stage : stage;
}

type type_entry = {
  type_scheme : type_scheme;
  type_kind   : kind;
}

type constructor_entry = {
  ctor_belongs_to : TypeID.t;
  ctor_parameter  : type_scheme;
}

type macro_entry = {
  macro_type : macro_type;
  macro_name : EvalVarID.t;
}

type signature =
  | ConcStructure of struct_signature
  | ConcFunctor   of functor_signature

and struct_signature =
  struct_signature_entry Alist.t

and struct_signature_entry =
  | SSValue     of var_name * value_entry
  | SSType      of type_name * type_entry
  | SSModule    of module_name * module_entry
  | SSSignature of signature_name * signature abstracted

and functor_signature = {
  opaques  : OpaqueIDSet.t;
  domain   : struct_signature;
  codomain : OpaqueIDSet.t * signature;
}

and module_entry = {
  mod_name      : ModuleID.t option;
  mod_signature : signature;
}

and type_environment = {
  values       : (value_entry * bool ref) ValueNameMap.t;
  types        : type_entry TypeNameMap.t;
  modules      : module_entry ModuleNameMap.t;
  signatures   : (signature abstracted) SignatureNameMap.t;
  constructors : constructor_entry ConstructorMap.t;
  macros       : macro_entry MacroNameMap.t;
}


module Typeenv = struct

  type t = type_environment


  let empty : t =
    {
      values       = ValueNameMap.empty;
      types        = TypeNameMap.empty;
      modules      = ModuleNameMap.empty;
      signatures   = SignatureNameMap.empty;
      constructors = ConstructorMap.empty;
      macros       = MacroNameMap.empty;
    }


  let add_macro (csnm : ctrlseq_name) (macentry : macro_entry) (tyenv : t) : t =
    { tyenv with macros = tyenv.macros |> MacroNameMap.add csnm macentry }


  let find_macro (csnm : ctrlseq_name) (tyenv : t) : macro_entry option =
    tyenv.macros |> MacroNameMap.find_opt csnm


  let add_value (varnm : var_name) (ventry : value_entry) (tyenv : t) : t =
    let is_used = ref false in
    { tyenv with values = tyenv.values |> ValueNameMap.add varnm (ventry, is_used) }


  let find_value (varnm : var_name) (tyenv : t) : value_entry option =
    tyenv.values |> ValueNameMap.find_opt varnm |> Option.map (fun (ventry, is_used) ->
      is_used := true;
      ventry
    )


  let add_type (tynm : type_name) (tentry : type_entry) (tyenv : t) : t =
    { tyenv with types = tyenv.types |> TypeNameMap.add tynm tentry }


  let find_type (tynm : type_name) (tyenv : t) : type_entry option =
    tyenv.types |> TypeNameMap.find_opt tynm


  let add_constructor (ctornm : constructor_name) (centry : constructor_entry) (tyenv : t) : t =
    { tyenv with constructors = tyenv.constructors |> ConstructorMap.add ctornm centry }


  let find_constructor (ctornm : constructor_name) (tyenv : t) : constructor_entry option =
    tyenv.constructors |> ConstructorMap.find_opt ctornm


  let add_module (m : module_name) (mentry : module_entry) (tyenv : t) : t =
    { tyenv with modules = tyenv.modules |> ModuleNameMap.add m mentry }


  let find_module (m : module_name) (tyenv : t) : module_entry option =
    tyenv.modules |> ModuleNameMap.find_opt m


  let add_signature (s : signature_name) (absmodsig : signature abstracted) (tyenv : t) : t =
    { tyenv with signatures = tyenv.signatures |> SignatureNameMap.add s absmodsig }


  let find_signature (s : signature_name) (tyenv : t) : (signature abstracted) option =
    tyenv.signatures |> SignatureNameMap.find_opt s

end


module StructSig = struct

  type t = struct_signature


  let empty : t =
    Alist.empty


  let add_value (x : var_name) (ventry : value_entry) (ssig : t) : t =
    Alist.extend ssig (SSValue(x, ventry))


  let find_value (x : var_name) (ssig : t) : value_entry option =
    ssig |> Alist.to_list_rev |> List.find_map (function
    | SSValue(x0, ventry) -> if String.equal x x0 then Some(ventry) else None
    | _                   -> None
    )


  let add_type (tynm : type_name) (tentry : type_entry) (ssig : t) : t =
    Alist.extend ssig (SSType(tynm, tentry))


  let find_type (tynm : type_name) (ssig : t) : type_entry option =
    ssig |> Alist.to_list_rev |> List.find_map (function
    | SSType(tynm0, tentry) -> if String.equal tynm tynm0 then Some(tentry) else None
    | _                     -> None
    )


  let add_module (m : module_name) (mentry : module_entry) (ssig : t) : t =
    Alist.extend ssig (SSModule(m, mentry))


  let find_module (m : module_name) (ssig : t) : module_entry option =
    ssig |> Alist.to_list_rev |> List.find_map (function
    | SSModule(m0, mentry) -> if String.equal m m0 then Some(mentry) else None
    | _                    -> None
    )


  let add_signature (s : signature_name) (absmodsig : signature abstracted) (ssig : t) : t =
    Alist.extend ssig (SSSignature(s, absmodsig))


  let find_signature (s : signature_name) (ssig : t) : (signature abstracted) option =
    ssig |> Alist.to_list_rev |> List.find_map (function
    | SSSignature(s0, absmodsig) -> if String.equal s s0 then Some(absmodsig) else None
    | _                          -> None
    )


  let fold ~v:fv ~t:ft ~m:fm ~s:fs acc (ssig : t) =
    ssig |> Alist.to_list |> List.fold_left (fun acc entry ->
      match entry with
      | SSValue(x, ventry)        -> fv x ventry acc
      | SSType(tynm, tentry)      -> ft tynm tentry acc
      | SSModule(m, mentry)       -> fm m mentry acc
      | SSSignature(s, absmodsig) -> fs s absmodsig acc
    ) acc


  let map_and_fold ~v:fv ~t:ft ~m:fm ~s:fs acc (ssig : t) =
      ssig |> Alist.to_list |> List.fold_left (fun (sigracc, acc) entry ->
        match entry with
        | SSValue(x, ventry) ->
            let (ventry, acc) = fv x ventry acc in
            (Alist.extend sigracc (SSValue(x, ventry)), acc)

        | SSType(tynm, tentry) ->
            let (tentry, acc) = ft tynm tentry acc in
            (Alist.extend sigracc (SSType(tynm, tentry)), acc)

        | SSModule(modnm, mentry) ->
            let (mentry, acc) = fm modnm mentry acc in
            (Alist.extend sigracc (SSModule(modnm, mentry)), acc)

        | SSSignature(signm, absmodsig) ->
            let (absmodsig, acc) = fs signm absmodsig acc in
            (Alist.extend sigracc (SSSignature(signm, absmodsig)), acc)

      ) (Alist.empty, acc)


  let map ~v:fv ~t:ft ~m:fm ~s:fs (ssig : t) : t =
    let (ssig, ()) =
      ssig |> map_and_fold
        ~v:(fun x ventry () -> (fv x ventry, ()))
        ~t:(fun tynm tentry () -> (ft tynm tentry, ()))
        ~m:(fun modnm mentry () -> (fm modnm mentry, ()))
        ~s:(fun signm absmodsig () -> (fs signm absmodsig, ()))
        ()
    in
    ssig


  exception Conflict of string


  let union (ssig1 : t) (ssig2 : t) : (t, string) result =
    let check_none s opt =
      match opt with
      | None    -> ()
      | Some(_) -> raise (Conflict(s))
    in
    try
      let ssig =
        ssig2 |> Alist.to_list |> List.fold_left (fun ssig entry ->
          let () =
            match entry with
            | SSValue(x, _)         -> check_none x (find_value x ssig1)
            | SSType(tynm, _)       -> check_none tynm (find_type tynm ssig1)
            | SSModule(modnm, _)    -> check_none modnm (find_module modnm ssig1)
            | SSSignature(signm, _) -> check_none signm (find_signature signm ssig1)
          in
          Alist.extend ssig entry
        ) ssig1
      in
      Ok(ssig)
    with
    | Conflict(s) -> Error(s)

end

(*
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


let open_module (rng : Range.t) (mdlnm : module_name) (tyenv : t) =
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

              | Some((_, evid, stage)) ->
                  vdmapU |> VarMap.add varnm (pty, evid, stage)
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


let find_for_inner (tyenv : t) (varnm : var_name) : (poly_type * EvalVarID.t * stage) option =
  let open OptionMonad in
  let mtr = tyenv.main_tree in
  let addrlst = Alist.to_list tyenv.current_address in
  ModuleTree.find_stage mtr addrlst >>= fun (vdmap, _, _, _) ->
  VarMap.find_opt varnm vdmap
*)


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


(*
module DependencyGraph = DirectedGraph.Make
  (struct
    type t = type_name
    let compare = compare
    let show s = s (* for debug *)
  end)


type vertex_label =
  | VariantVertex of Range.t * TypeID.t * untyped_type_argument list * constructor_branch list
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


let rec fix_manual_type_general (type a) (type b) (dpmode : dependency_mode) (tyenv : t) (lev : level) (freef : Range.t -> mono_type_variable_info ref -> (a, b) typ) (orfreef : mono_option_row_variable_info ref -> (a, b) option_row) (typaramf : Range.t -> string -> (a, b) type_main) (mnty : manual_type) : (a, b) typ =

  let rec aux (mnty : manual_type) : (a, b) typ =
    let (rng, mntymain) = mnty in
    let error tynm lenexp lenerr =
      raise (IllegalNumberOfTypeArguments(rng, tynm, lenexp, lenerr))
    in
    let ptymainnew =
      match mntymain with

      | MFuncType(mntyopts, mntydom, mntycod) -> FuncType(aux_or mntyopts, aux mntydom, aux mntycod)

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
            match (mdlnmlst, dpmode) with
            | ([], DependentMode(dg)) ->
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

            | _ ->
                find_in_variant_environment ()
          end

      | MTypeParam(tyargnm) ->
          typaramf rng tyargnm

    in
      (rng, ptymainnew)


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
*)

(*
let rec enumerate_constructors (pre : pre) (tyenv : t) (typeid : TypeID.t) : (constructor_name * (mono_type list -> mono_type)) list =
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


let rec find_constructor_candidates (pre : pre) (tyenv : t) (constrnm : constructor_name) : constructor_name list =
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
let rec add_mutual_cons (tyenv : t) (lev : level) (mutvarntcons : untyped_type_binding list) =

  let dg = DependencyGraph.create 32 in

  let rec add_each_type_as_vertex (mutvarntcons : untyped_type_binding list) =
    let iter = add_each_type_as_vertex in
    match mutvarntcons with
    | [] -> ()
    | (tynmtok, tyvars, constraints, UTBindVariant(utvarntcons)) :: tailcons ->
        let (tynmrng, tynm) = tynmtok in
        let kdtyvars = kind_type_arguments tyvars constraints in
        if DependencyGraph.mem_vertex tynm dg then
          let rng = extract_range_in_vertex_label (DependencyGraph.get_vertex dg tynm) in
            raise (MultipleTypeDefinition(rng, tynmrng, tynm))
        else
          let () = print_for_debug_variantenv ("AddV " ^ tynm) in (* for debug *)
          let tyid = TypeID.fresh (get_moduled_var_name tyenv tynm) in
          begin
            DependencyGraph.add_vertex dg tynm (VariantVertex(tynmrng, tyid, kdtyvars, utvarntcons));
            iter tailcons;
          end

    | (tynmtok, tyvars, constraints, UTBindSynonym(mnty)) :: tailcons ->
        let (tynmrng, tynm) = tynmtok in
        let kdtyvars = kind_type_arguments tyvars constraints in
        if DependencyGraph.mem_vertex tynm dg then
          let rng = extract_range_in_vertex_label (DependencyGraph.get_vertex dg tynm) in
            raise (MultipleTypeDefinition(rng, tynmrng, tynm))
        else
          let () = print_for_debug_variantenv ("AddS " ^ tynm) in (* for debug *)
          let tyid = TypeID.fresh (get_moduled_var_name tyenv tynm) in
          begin
            DependencyGraph.add_vertex dg tynm (SynonymVertex(tynmrng, tyid, kdtyvars, mnty, ref None));
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
              List.iter iter mtyarglist

        | MTypeName(mtyarglist, mdlnmlst, tynm) ->
            List.iter iter mtyarglist
    in
    let iter = add_each_dependency_as_edge in
    match mutvarntcons with
    | [] ->
        ()

    | (_, _, _, UTBindVariant(_)) :: tailcons ->
        iter tailcons

    | ((_, tynm), _, _, UTBindSynonym(mnty)) :: tailcons ->
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

    let rec register_each_constructor (tyargcons : untyped_type_argument list) (tynm : type_name) (acctyenv : t) (utvarntcons : constructor_branch list) =
      let iter = register_each_constructor tyargcons tynm in
        match utvarntcons with
        | [] ->
            acctyenv

        | UTConstructorBranch((rng, constrnm), mnty) :: tailcons ->
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
              if is_stronger_kind (BoundID.get_kind bid1) (BoundID.get_kind bid2) then begin
                BoundIDHashTable.add current_bid_to_ty bid2 ty1;
                true
              end else
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
                | UniversalKind ->
                    true

                | RecordKind(tyasc2) ->
                    Assoc.domain_included tyasc2 tyasc1 &&
                      List.for_all (fun (x, y) -> aux x y) (Assoc.intersection tyasc1 tyasc2)
              in
              if not binc then
                false
              else begin
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
                | UniversalKind -> BoundIDHashTable.add current_bid_to_ty bid2 ty1; true
                | RecordKind(_) -> false
              end
        end

    | (RecordType(tyasc1), TypeVariable(PolyFree({contents= MonoFree(tvid2)} as tvref))) ->
        let kd2 = FreeID.get_kind tvid2 in
        let binc =
          match kd2 with
          | UniversalKind ->
              true

          | RecordKind(tyasc2) ->
              Assoc.domain_included tyasc1 tyasc2 &&
                List.for_all (fun (ty1, ty2) -> aux ty1 ty2) (Assoc.intersection tyasc1 (Assoc.map_value lift_poly_body tyasc2))
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


let sigcheck (rng : Range.t) (pre : pre) (tyenv : t) (tyenvprev : t) (msigopt : (untyped_declaration list) option) =

  let lev = pre.level in

  let rec read_manual_signature (tyenvacc : t) (tyenvforsigI : t) (tyenvforsigO : t) (msig : untyped_declaration list) (sigoptacc : signature option) =
    let iter = read_manual_signature in
      match msig with
      | [] -> (sigoptacc, tyenvacc)

      | UTDeclTypeOpaque(tynmtok, tyargcons) :: tail ->
          let (_, tynm) = tynmtok in
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

      | UTDeclValue(valnmtok, constrntcons, mty) :: tail ->
          let (_, varnm) = valnmtok in
          let () = print_for_debug_variantenv ("SIGV " ^ varnm) in (* for debug *)
          let tysigI = fix_manual_type_free { pre with level = Level.succ lev; } tyenvforsigI mty constrntcons in
          let ptysigI = generalize lev tysigI in
          let tysigO = fix_manual_type_free { pre with level = Level.succ lev; } tyenvforsigO mty constrntcons in
          let ptysigO = generalize lev tysigO in
          let () = print_for_debug_variantenv ("LEVEL " ^ (Level.show lev) ^ "; " ^ (string_of_mono_type_basic tysigI) ^ " ----> " ^ (string_of_poly_type_basic ptysigI)) in (* for debug *)
          begin
            match find_for_inner tyenv varnm with
            | None ->
                raise (NotProvidingValueImplementation(rng, varnm))

            | Some((ptyimp, _, stageimp)) ->
                let stagereq = pre.stage in
                if stageimp = stagereq then
                  let b = reflects ptysigI ptyimp in
                  if b then
                    let sigoptaccnew = add_val_to_signature sigoptacc varnm ptysigO in
                      iter tyenvacc tyenvforsigI tyenvforsigO tail sigoptaccnew
                  else
                    raise (NotMatchingInterface(rng, varnm, tyenv, ptyimp, tyenvforsigO, ptysigO))
                else
                  raise (NotMatchingStage(rng, varnm, stageimp, stagereq))
          end

      | UTDeclDirect(cmdtok, constrntcons, mty) :: tail ->
          let (_, csnm) = cmdtok in
          let () = print_for_debug_variantenv ("SIGD " ^ csnm) in (* for debug *)
(*
          let () = print_for_debug_variantenv ("D-OK0 " ^ (string_of_manual_type mty)) in (* for debug *)
*)
          let tysigI = fix_manual_type_free { pre with level = Level.succ lev; } tyenvforsigI mty constrntcons in
          let () = print_for_debug_variantenv "D-OK1" in (* for debug *)
          let ptysigI = generalize lev tysigI in
          let tysigO = fix_manual_type_free { pre with level = Level.succ lev; } tyenvforsigO mty constrntcons in
          let () = print_for_debug_variantenv "D-OK2" in (* for debug *)
          let ptysigO = generalize lev tysigO in
          let () = print_for_debug_variantenv ("LEVEL " ^ (Level.show lev) ^ "; " ^ (string_of_mono_type_basic tysigI) ^ " ----> " ^ (string_of_poly_type_basic ptysigI)) in (* for debug *)
          begin
            match find_for_inner tyenv csnm with
            | None ->
                raise (NotProvidingValueImplementation(rng, csnm))

            | Some((ptyimp, evidimp, stage)) ->
                let b = reflects ptysigI ptyimp in
                  (* -- 'reflects pty1 pty2' may change 'pty2' -- *)
                if b then
                  let sigoptaccnew = add_val_to_signature sigoptacc csnm ptysigO in
                  let tyenvaccnew =
                    let mtr = tyenvacc.main_tree in
                    match ModuleTree.update mtr [] (update_vt (VarMap.add csnm (ptysigO, evidimp, stage))) with
                    | None         -> assert false
                    | Some(mtrnew) -> { tyenvacc with main_tree = mtrnew; }
                  in
                    iter tyenvaccnew tyenvforsigI tyenvforsigO tail sigoptaccnew
                else
                  raise (NotMatchingInterface(rng, csnm, tyenv, ptyimp, tyenvforsigO, ptysigO))
          end

      | _ ->
          failwith "TODO: other declarations"
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
*)