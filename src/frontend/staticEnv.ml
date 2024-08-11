(* -unused-value-declaration *)
[@@@ocaml.warning "-32"]

open Types


module Distance = struct

  let edit_distance (s1 : string) (s2 : string) (mindist : int) : int =
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


  let initial_candidates (nm : string) =
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

type value_entry = {
  val_name  : EvalVarID.t option;
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
  macro_type : poly_macro_type;
  macro_name : EvalVarID.t option;
}
[@@deriving show { with_path = false }]

type signature =
  | ConcStructure of struct_signature
  | ConcFunctor   of functor_signature

and struct_signature =
  struct_signature_entry Alist.t
    [@printer (fun ppf ssentryacc ->
      Format.fprintf ppf "%a" (Format.pp_print_list pp_struct_signature_entry) (Alist.to_list ssentryacc)
    )]

and struct_signature_entry =
  | SSValue       of var_name * value_entry
  | SSMacro       of macro_name * macro_entry
  | SSConstructor of constructor_name * constructor_entry
  | SSFold        of type_name * poly_type
  | SSType        of type_name * type_entry
  | SSModule      of module_name * module_entry
  | SSSignature   of signature_name * signature abstracted

and functor_signature = {
  opaques  : quantifier;
  domain   : signature;
  codomain : signature abstracted;
  closure  : (module_name ranged * untyped_module * type_environment) option;
}

and module_entry = {
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
[@@deriving show { with_path = false }]


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


  let add_macro (csnm : macro_name) (macentry : macro_entry) (tyenv : t) : t =
    { tyenv with macros = tyenv.macros |> MacroNameMap.add csnm macentry }


  let find_macro (csnm : macro_name) (tyenv : t) : macro_entry option =
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


  (* TODO (enhance): make this function more efficient *)
  let enumerate_constructors (tyid : TypeID.t) (tyenv : t) : (constructor_name * type_scheme) list =
    ConstructorMap.fold (fun ctornm centry acc ->
      if TypeID.equal tyid centry.ctor_belongs_to then
        Alist.extend acc (ctornm, centry.ctor_parameter)
      else
        acc
    ) tyenv.constructors Alist.empty |> Alist.to_list


  let add_module (m : module_name) (mentry : module_entry) (tyenv : t) : t =
    { tyenv with modules = tyenv.modules |> ModuleNameMap.add m mentry }


  let find_module (m : module_name) (tyenv : t) : module_entry option =
    tyenv.modules |> ModuleNameMap.find_opt m


  let add_signature (s : signature_name) (absmodsig : signature abstracted) (tyenv : t) : t =
    { tyenv with signatures = tyenv.signatures |> SignatureNameMap.add s absmodsig }


  let find_signature (s : signature_name) (tyenv : t) : (signature abstracted) option =
    tyenv.signatures |> SignatureNameMap.find_opt s


  let fold_value (f : var_name -> value_entry -> 'a -> 'a) (tyenv : t) (acc : 'a) : 'a =
    ValueNameMap.fold (fun x (ventry, _is_used) -> f x ventry) tyenv.values acc

end


module StructSig = struct

  type t = struct_signature


  let empty : t =
    Alist.empty


  let add_macro (csnm : macro_name) (macentry : macro_entry) (ssig : t) : t =
    Alist.extend ssig (SSMacro(csnm, macentry))


  let find_macro (csnm : macro_name) (ssig : t) : macro_entry option =
    ssig |> Alist.to_list_rev |> List.find_map (function
    | SSMacro(csnm0, macentry) -> if String.equal csnm csnm0 then Some(macentry) else None
    | _                        -> None
    )


  let add_value (x : var_name) (ventry : value_entry) (ssig : t) : t =
    Alist.extend ssig (SSValue(x, ventry))


  let find_value (x : var_name) (ssig : t) : value_entry option =
    ssig |> Alist.to_list_rev |> List.find_map (function
    | SSValue(x0, ventry) -> if String.equal x x0 then Some(ventry) else None
    | _                   -> None
    )


  let add_constructor (ctornm : constructor_name) (centry : constructor_entry) (ssig : t) : t =
    Alist.extend ssig (SSConstructor(ctornm, centry))


  let find_constructor (ctornm : constructor_name) (ssig : t) : constructor_entry option =
    ssig |> Alist.to_list_rev |> List.find_map (function
    | SSConstructor(ctornm0, centry) -> if String.equal ctornm ctornm0 then Some(centry) else None
    | _                              -> None
    )


  let add_dummy_fold (tynm : type_name) (pty : poly_type) (ssig : t) : t =
    Alist.extend ssig (SSFold(tynm, pty))


  let find_dummy_fold (tynm : type_name) (ssig : t) : poly_type option =
    ssig |> Alist.to_list_rev |> List.find_map (function
    | SSFold(tynm0, pty) -> if String.equal tynm tynm0 then Some(pty) else None
    | _                  -> None
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


  let fold ~v:fv ~a:fa ~c:fc ~f:ff ~t:ft ~m:fm ~s:fs acc (ssig : t) =
    ssig |> Alist.to_list |> List.fold_left (fun acc entry ->
      match entry with
      | SSValue(x, ventry)            -> fv x ventry acc
      | SSMacro(csnm, macentry)       -> fa csnm macentry acc
      | SSConstructor(ctornm, centry) -> fc ctornm centry acc
      | SSFold(tynm, pty)             -> ff tynm pty acc
      | SSType(tynm, tentry)          -> ft tynm tentry acc
      | SSModule(m, mentry)           -> fm m mentry acc
      | SSSignature(s, absmodsig)     -> fs s absmodsig acc
    ) acc


  let map_and_fold ~v:fv ~a:fa ~c:fc ~f:ff ~t:ft ~m:fm ~s:fs acc (ssig : t) =
      ssig |> Alist.to_list |> List.fold_left (fun (ssig, acc) entry ->
        match entry with
        | SSValue(x, ventry) ->
            let (ventry, acc) = fv x ventry acc in
            (Alist.extend ssig (SSValue(x, ventry)), acc)

        | SSMacro(csnm, macentry) ->
            let (macentry, acc) = fa csnm macentry acc in
            (Alist.extend ssig (SSMacro(csnm, macentry)), acc)

        | SSConstructor(ctornm, centry) ->
            let (centry, acc) = fc ctornm centry acc in
            (Alist.extend ssig (SSConstructor(ctornm, centry)), acc)

        | SSFold(tynm, pty) ->
            let (pty, acc) = ff tynm pty acc in
            (Alist.extend ssig (SSFold(tynm, pty)), acc)

        | SSType(tynm, tentry) ->
            let (tentry, acc) = ft tynm tentry acc in
            (Alist.extend ssig (SSType(tynm, tentry)), acc)

        | SSModule(modnm, mentry) ->
            let (mentry, acc) = fm modnm mentry acc in
            (Alist.extend ssig (SSModule(modnm, mentry)), acc)

        | SSSignature(signm, absmodsig) ->
            let (absmodsig, acc) = fs signm absmodsig acc in
            (Alist.extend ssig (SSSignature(signm, absmodsig)), acc)

      ) (Alist.empty, acc)


  let map ~v:fv ~a:fa ~c:fc ~f:ff ~t:ft ~m:fm ~s:fs (ssig : t) : t =
    let (ssig, ()) =
      ssig |> map_and_fold
        ~v:(fun x ventry () -> (fv x ventry, ()))
        ~a:(fun csnm macentry () -> (fa csnm macentry, ()))
        ~c:(fun ctornm centry () -> (fc ctornm centry, ()))
        ~f:(fun tynm pty () -> (ff tynm pty, ()))
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
            | SSValue(x, _)            -> check_none x (find_value x ssig1)
            | SSMacro(csnm, _)         -> check_none csnm (find_macro csnm ssig1)
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


let find_candidates_in_type_environment (tyenv : Typeenv.t) (varnm : var_name) : var_name list =
  Distance.get_candidates Typeenv.fold_value tyenv varnm


let find_candidates_in_struct_sig (ssig : StructSig.t) (varnm : var_name) : var_name list =
  let fold_value f (ssig : StructSig.t) acc =
    StructSig.fold
      ~v:(fun x ventry acc -> f x ventry acc)
      ~a:(fun _ _ acc -> acc)
      ~c:(fun _ _ acc -> acc)
      ~f:(fun _ _ acc -> acc)
      ~t:(fun _ _ acc -> acc)
      ~m:(fun _ _ acc -> acc)
      ~s:(fun _ _ acc -> acc)
      acc
      ssig
  in
  Distance.get_candidates fold_value ssig varnm


type global_type_environment = StructSig.t GlobalTypeenv.t
