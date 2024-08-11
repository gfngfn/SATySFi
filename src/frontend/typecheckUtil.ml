
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
