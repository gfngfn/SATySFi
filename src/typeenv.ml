open Types

module VarMap = Map.Make(String)


module ModuleID
: sig
    type t
    val compare : t -> t -> int
    val fresh : unit -> t
  end
= struct

    type t = int

    let compare i1 i2 = i1 - i2

    let current_id = ref 0

    let fresh () =
      begin
        incr current_id ;
        !current_id
      end

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
type t = current_address * name_to_id_map * (var_to_type_map ModuleTree.t)


let from_list (lst : (var_name * poly_type) list) =
  let vmapinit = List.fold_left (fun vmap (varnm, pty) -> VarMap.add varnm pty vmap) VarMap.empty lst in
    ([], ModuleNameMap.empty, ModuleTree.empty vmapinit)


let add ((addr, nmtoid, mtr) : t) (varnm : var_name) (pty : poly_type) =
  let mtrnew = ModuleTree.update mtr addr (VarMap.add varnm pty) in
    (addr, nmtoid, mtrnew)


let find ((addr, nmtoid, mtr) : t) (mdlnmlst : module_name list) (varnm : var_name) =
  let addrlast = List.map (fun nm -> ModuleNameMap.find nm nmtoid) mdlnmlst in
  let ptyopt =
    ModuleTree.search_backward mtr addr addrlast (fun sgl ->
      try Some(VarMap.find varnm sgl) with
      | Not_found -> None
    )
  in
    match ptyopt with
    | None      -> raise Not_found
    | Some(pty) -> pty


let enter_new_module ((addr, nmtoid, mtr) : t) (mdlnm : module_name) =
  let mdlid = ModuleID.fresh () in
  let addrnew = List.append addr [mdlid] in
  let nmtoidnew = ModuleNameMap.add mdlnm mdlid nmtoid in
  let mtrnew = ModuleTree.add_stage mtr addr mdlid VarMap.empty in
    (addrnew, nmtoidnew, mtrnew)


let leave_module ((addr, nmtoid, mtr) : t) =
  (List.rev (List.tl (List.rev addr)), nmtoid, mtr)
