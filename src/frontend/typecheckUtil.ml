
open SyntaxBase
open StaticEnv
open Types
open TypeError


type 'a ok = ('a, type_error) result

module SubstMap = Map.Make(TypeID)

(* The type for substitutions that map opaque types to concrete types. *)
type substitution = type_scheme SubstMap.t

type variant_definition = type_name * TypeID.t * BoundID.t list * constructor_branch_map


let fresh_free_id (qtfbl : quantifiability) (lev : Level.t) : FreeID.t =
  FreeID.fresh lev (qtfbl = Quantifiable)


let fresh_free_row_id (lev : Level.t) (labset : LabelSet.t) : FreeRowID.t =
  FreeRowID.fresh lev labset


let unify (ty1 : mono_type) (ty2 : mono_type) : unit ok =
  Unification.unify_type ty1 ty2 |> Result.map_error (fun uerr -> TypeUnificationError(ty1, ty2, uerr))


let unify_row (rng : Range.t) (row1 : mono_row) (row2 : mono_row) : unit ok =
  Unification.unify_row row1 row2 |> Result.map_error (fun uerr -> RowUnificationError(rng, row1, row2, uerr))


let make_range_from_module_chain ((modident, projs) : module_name_chain) : Range.t =
  let (rngL, _) = modident in
  match List.rev projs with
  | []             -> rngL
  | (rngR, _) :: _ -> Range.unite rngL rngR


let find_module (tyenv : Typeenv.t) ((rng, modnm) : module_name ranged) : module_entry ok =
  let open ResultMonad in
  match tyenv |> Typeenv.find_module modnm with
  | None ->
      err (UndefinedModuleName(rng, modnm))

  | Some(mentry) ->
      return mentry


let resolve_module_chain (mentry0 : module_entry) (modidents : (module_name ranged) list) : module_entry ok =
  let open ResultMonad in
  modidents |> foldM (fun mentry (rng, modnm) ->
    match mentry.mod_signature with
    | ConcFunctor(fsig) ->
        err (NotAStructureSignature(rng, fsig))

    | ConcStructure(ssig) ->
        begin
          match ssig |> StructSig.find_module modnm with
          | None ->
              err (UndefinedModuleName(rng, modnm))

          | Some(mentry) ->
              return mentry
        end
  ) mentry0


let find_module_chain (tyenv : Typeenv.t) ((modident0, modidents) : module_name_chain) : module_entry ok =
  let open ResultMonad in
  let* mentry0 = find_module tyenv modident0 in
  resolve_module_chain mentry0 modidents


let add_to_type_environment_by_signature (ssig : StructSig.t) (tyenv : Typeenv.t) : Typeenv.t =
  ssig |> StructSig.fold
    ~v:(fun x ventry -> Typeenv.add_value x ventry)
    ~a:(fun csnm macentry -> Typeenv.add_macro csnm macentry)
    ~c:(fun ctornm centry -> Typeenv.add_constructor ctornm centry)
    ~f:(fun _tynm _pty tyenv -> tyenv)
    ~t:(fun tynm tentry -> Typeenv.add_type tynm tentry)
    ~m:(fun modnm mentry -> Typeenv.add_module modnm mentry)
    ~s:(fun signm absmodsig -> Typeenv.add_signature signm absmodsig)
    tyenv


let decode_manual_row_base_kind (mnrbkd : manual_row_base_kind) : row_base_kind ok =
  let open ResultMonad in
  mnrbkd |> foldM (fun labset (rng, label) ->
    if labset |> LabelSet.mem label then
      err (LabelUsedMoreThanOnce(rng, label))
    else
      return (labset |> LabelSet.add label)
  ) LabelSet.empty


let add_type_parameters (lev : Level.t) (tyvars : (type_variable_name ranged) list) (typarammap : type_parameter_map) : (type_parameter_map * BoundID.t list) ok =
  let open ResultMonad in
  let* (typarammap, bidacc) =
    tyvars |> foldM (fun (typarammap, bidacc) (rng, tyvarnm) ->
      if typarammap |> TypeParameterMap.mem tyvarnm then
        err (TypeParameterBoundMoreThanOnce(rng, tyvarnm))
      else
        let mbbid = MustBeBoundID.fresh lev in
        let bid = MustBeBoundID.to_bound_id mbbid in
        return (typarammap |> TypeParameterMap.add tyvarnm mbbid, Alist.extend bidacc bid)
    ) (typarammap, Alist.empty)
  in
  return (typarammap, Alist.to_list bidacc)


let add_row_parameters (lev : Level.t) (rowvars : (row_variable_name ranged * manual_row_base_kind) list) (rowparammap : row_parameter_map) : (row_parameter_map * BoundRowID.t list) ok =
  let open ResultMonad in
  let* (rowparammap, bridacc) =
    rowvars |> foldM (fun (rowparammap, bridacc) ((rng, rowvarnm), mnbrkd) ->
      if rowparammap |> RowParameterMap.mem rowvarnm then
        err (LabelUsedMoreThanOnce(rng, rowvarnm))
      else
        decode_manual_row_base_kind mnbrkd >>= fun labset ->
        let mbbrid = MustBeBoundRowID.fresh lev labset in
        let brid = MustBeBoundRowID.to_bound_id mbbrid in
        return (rowparammap |> RowParameterMap.add rowvarnm mbbrid, Alist.extend bridacc brid)
    ) (rowparammap, Alist.empty)
  in
  return (rowparammap, Alist.to_list bridacc)
