
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


type quantifier =
  kind OpaqueIDMap.t
[@@deriving show { with_path = false }]

type 'a abstracted =
  quantifier * 'a
[@@deriving show { with_path = false }]

type type_scheme =
  BoundID.t list * poly_type
[@@deriving show { with_path = false }]

type 'v value_entry = {
  val_name  : 'v;
  val_type  : poly_type;
  val_stage : stage;
}
[@@deriving show { with_path = false }]

type type_entry = {
  type_scheme : type_scheme;
  type_kind   : kind;
}
[@@deriving show { with_path = false }]

type constructor_entry = {
  ctor_belongs_to : TypeID.t;
  ctor_parameter  : type_scheme;
}
[@@deriving show { with_path = false }]

type macro_entry = {
  macro_type : macro_type;
  macro_name : EvalVarID.t;
}
[@@deriving show { with_path = false }]

type 'v signature =
  | ConcStructure of 'v struct_signature
  | ConcFunctor   of 'v functor_signature

and 'v struct_signature =
  ('v struct_signature_entry) Alist.t
    [@printer (fun ppf ssentryacc ->
(*
      Format.fprintf ppf "%a" (Format.pp_print_list (pp_struct_signature_entry ppv)) (Alist.to_list ssentryacc)
*)
      ()
    )]

and 'v struct_signature_entry =
  | SSValue       of var_name * 'v value_entry
  | SSConstructor of constructor_name * constructor_entry
  | SSFold        of type_name * poly_type
  | SSType        of type_name * type_entry
  | SSModule      of module_name * 'v module_entry
  | SSSignature   of signature_name * (unit signature) abstracted

and 'v functor_signature = {
  opaques  : quantifier;
  domain   : 'v signature;
  codomain : ('v signature) abstracted;
  closure  : (module_name ranged * untyped_module * 'v type_environment) option;
}

and 'v module_entry = {
  mod_signature : 'v signature;
}

and 'v type_environment = {
  values       : ('v value_entry * bool ref) ValueNameMap.t;
  types        : type_entry TypeNameMap.t;
  modules      : ('v module_entry) ModuleNameMap.t;
  signatures   : (virtual_signature abstracted) SignatureNameMap.t;
  constructors : constructor_entry ConstructorMap.t;
  macros       : macro_entry MacroNameMap.t;
}

and virtual_signature = unit signature
[@@deriving show { with_path = false }]

type virtual_type_environment = unit type_environment

type target_value_entry = EvalVarID.t value_entry

type target_module_entry = EvalVarID.t module_entry

type target_signature = EvalVarID.t signature

type target_type_environment = EvalVarID.t type_environment

type target_struct_signature = EvalVarID.t struct_signature


module Typeenv = struct

  type 'v t = 'v type_environment


  let empty : 'v t =
    {
      values       = ValueNameMap.empty;
      types        = TypeNameMap.empty;
      modules      = ModuleNameMap.empty;
      signatures   = SignatureNameMap.empty;
      constructors = ConstructorMap.empty;
      macros       = MacroNameMap.empty;
    }


  let forget (tyenv : target_type_environment) : virtual_type_environment =
    {
      values =
        tyenv.values |> ValueNameMap.map (fun (ventry, bref) ->
          ({
            val_name  = ();
            val_type  = ventry.val_type;
            val_stage = ventry.val_stage;
          }, bref));
      types = tyenv.types;
      modules = tyenv.modules;
      signatures = tyenv.signatures;
      constructors = tyenv.constructors;
      macros = tyenv.macros;
    }


  let add_macro (csnm : ctrlseq_name) (macentry : macro_entry) (tyenv : 'v t) : 'v t =
    { tyenv with macros = tyenv.macros |> MacroNameMap.add csnm macentry }


  let find_macro (csnm : ctrlseq_name) (tyenv : 'v t) : macro_entry option =
    tyenv.macros |> MacroNameMap.find_opt csnm


  let add_value (varnm : var_name) (ventry : 'v value_entry) (tyenv : 'v t) : 'v t =
    let is_used = ref false in
    { tyenv with values = tyenv.values |> ValueNameMap.add varnm (ventry, is_used) }


  let find_value (varnm : var_name) (tyenv : 'v t) : ('v value_entry) option =
    tyenv.values |> ValueNameMap.find_opt varnm |> Option.map (fun (ventry, is_used) ->
      is_used := true;
      ventry
    )


  let add_type (tynm : type_name) (tentry : type_entry) (tyenv : 'v t) : 'v t =
    { tyenv with types = tyenv.types |> TypeNameMap.add tynm tentry }


  let find_type (tynm : type_name) (tyenv : 'v t) : type_entry option =
    tyenv.types |> TypeNameMap.find_opt tynm


  let add_constructor (ctornm : constructor_name) (centry : constructor_entry) (tyenv : 'v t) : 'v t =
    { tyenv with constructors = tyenv.constructors |> ConstructorMap.add ctornm centry }


  let find_constructor (ctornm : constructor_name) (tyenv : 'v t) : constructor_entry option =
    tyenv.constructors |> ConstructorMap.find_opt ctornm


  (* TODO (enhance): make this function more efficient *)
  let enumerate_constructors (tyid : TypeID.t) (tyenv : 'v t) : (constructor_name * type_scheme) list =
    ConstructorMap.fold (fun ctornm centry acc ->
      if TypeID.equal tyid centry.ctor_belongs_to then
        Alist.extend acc (ctornm, centry.ctor_parameter)
      else
        acc
    ) tyenv.constructors Alist.empty |> Alist.to_list


  let add_module (m : module_name) (mentry : 'v module_entry) (tyenv : 'v t) : 'v t =
    { tyenv with modules = tyenv.modules |> ModuleNameMap.add m mentry }


  let find_module (m : module_name) (tyenv : 'v t) : 'v module_entry option =
    tyenv.modules |> ModuleNameMap.find_opt m


  let add_signature (s : signature_name) (absmodsig : virtual_signature abstracted) (tyenv : 'v t) : 'v t =
    { tyenv with signatures = tyenv.signatures |> SignatureNameMap.add s absmodsig }


  let find_signature (s : signature_name) (tyenv : 'v t) : (virtual_signature abstracted) option =
    tyenv.signatures |> SignatureNameMap.find_opt s

end


module StructSig = struct

  type 'v t = 'v struct_signature


  let empty : 'v t =
    Alist.empty


  let add_value (x : var_name) (ventry : 'v value_entry) (ssig : 'v t) : 'v t =
    Alist.extend ssig (SSValue(x, ventry))


  let find_value (x : var_name) (ssig : 'v t) : ('v value_entry) option =
    ssig |> Alist.to_list_rev |> List.find_map (function
    | SSValue(x0, ventry) -> if String.equal x x0 then Some(ventry) else None
    | _                   -> None
    )


  let add_constructor (ctornm : constructor_name) (centry : constructor_entry) (ssig : 'v t) : 'v t =
    Alist.extend ssig (SSConstructor(ctornm, centry))


  let find_constructor (ctornm : constructor_name) (ssig : 'v t) : constructor_entry option =
    ssig |> Alist.to_list_rev |> List.find_map (function
    | SSConstructor(ctornm0, centry) -> if String.equal ctornm ctornm0 then Some(centry) else None
    | _                              -> None
    )


  let add_dummy_fold (tynm : type_name) (pty : poly_type) (ssig : 'v t) : 'v t =
    Alist.extend ssig (SSFold(tynm, pty))


  let find_dummy_fold (tynm : type_name) (ssig : 'v t) : poly_type option =
    ssig |> Alist.to_list_rev |> List.find_map (function
    | SSFold(tynm0, pty) -> if String.equal tynm tynm0 then Some(pty) else None
    | _                  -> None
    )


  let add_type (tynm : type_name) (tentry : type_entry) (ssig : 'v t) : 'v t =
    Alist.extend ssig (SSType(tynm, tentry))


  let find_type (tynm : type_name) (ssig : 'v t) : type_entry option =
    ssig |> Alist.to_list_rev |> List.find_map (function
    | SSType(tynm0, tentry) -> if String.equal tynm tynm0 then Some(tentry) else None
    | _                     -> None
    )


  let add_module (m : module_name) (mentry : 'v module_entry) (ssig : 'v t) : 'v t =
    Alist.extend ssig (SSModule(m, mentry))


  let find_module (m : module_name) (ssig : 'v t) : ('v module_entry) option =
    ssig |> Alist.to_list_rev |> List.find_map (function
    | SSModule(m0, mentry) -> if String.equal m m0 then Some(mentry) else None
    | _                    -> None
    )


  let add_signature (s : signature_name) (absmodsig : virtual_signature abstracted) (ssig : 'v t) : 'v t =
    Alist.extend ssig (SSSignature(s, absmodsig))


  let find_signature (s : signature_name) (ssig : 'v t) : (virtual_signature abstracted) option =
    ssig |> Alist.to_list_rev |> List.find_map (function
    | SSSignature(s0, absmodsig) -> if String.equal s s0 then Some(absmodsig) else None
    | _                          -> None
    )


  let fold ~v:fv ~c:fc ~f:ff ~t:ft ~m:fm ~s:fs acc (ssig : 'v t) =
    ssig |> Alist.to_list |> List.fold_left (fun acc entry ->
      match entry with
      | SSValue(x, ventry)            -> fv x ventry acc
      | SSConstructor(ctornm, centry) -> fc ctornm centry acc
      | SSFold(tynm, pty)             -> ff tynm pty acc
      | SSType(tynm, tentry)          -> ft tynm tentry acc
      | SSModule(m, mentry)           -> fm m mentry acc
      | SSSignature(s, absmodsig)     -> fs s absmodsig acc
    ) acc


  let map_and_fold ~v:fv ~c:fc ~f:ff ~t:ft ~m:fm ~s:fs acc (ssig : 'v t) =
      ssig |> Alist.to_list |> List.fold_left (fun (sigracc, acc) entry ->
        match entry with
        | SSValue(x, ventry) ->
            let (ventry, acc) = fv x ventry acc in
            (Alist.extend sigracc (SSValue(x, ventry)), acc)

        | SSConstructor(ctornm, centry) ->
            let (centry, acc) = fc ctornm centry acc in
            (Alist.extend sigracc (SSConstructor(ctornm, centry)), acc)

        | SSFold(tynm, pty) ->
            let (pty, acc) = ff tynm pty acc in
            (Alist.extend sigracc (SSFold(tynm, pty)), acc)

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


  let map ~v:fv ~c:fc ~f:ff ~t:ft ~m:fm ~s:fs (ssig : 'v t) : 'v t =
    let (ssig, ()) =
      ssig |> map_and_fold
        ~v:(fun x ventry () -> (fv x ventry, ()))
        ~c:(fun ctornm centry () -> (fc ctornm centry, ()))
        ~f:(fun tynm pty () -> (ff tynm pty, ()))
        ~t:(fun tynm tentry () -> (ft tynm tentry, ()))
        ~m:(fun modnm mentry () -> (fm modnm mentry, ()))
        ~s:(fun signm absmodsig () -> (fs signm absmodsig, ()))
        ()
    in
    ssig


  exception Conflict of string


  let union (ssig1 : 'v t) (ssig2 : 'v t) : ('v t, string) result =
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
            | SSValue(x, _)            -> check_none x (find_value x ssig1)
            | SSConstructor(ctornm, _) -> check_none ctornm (find_constructor ctornm ssig1)
            | SSFold(tynm, _)          -> check_none tynm (find_type tynm ssig1)
            | SSType(tynm, _)          -> check_none tynm (find_type tynm ssig1)
            | SSModule(modnm, _)       -> check_none modnm (find_module modnm ssig1)
            | SSSignature(signm, _)    -> check_none signm (find_signature signm ssig1)
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
*)
