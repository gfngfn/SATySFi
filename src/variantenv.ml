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


type definition        = Data of int | Synonym of (* (type_variable_info ref) list *) int * mono_type
type defined_type_list = (type_name * Typeid.t * definition) list
type constructor_list  = (constructor_name * Typeid.t * (type_variable_info ref) list * mono_type) list

(* PUBLIC *)
type t = defined_type_list * constructor_list


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


module DirectedGraph (Vertex : sig type t val compare : t -> t -> int end)
: sig
    type state = Remained | Touched | Done
    type vertex = Vertex.t
    type 'a t
    exception UndefinedSourceVertex
    exception UndefinedDestinationVertex
    val create : int -> 'a t
    val add_vertex : 'a t -> vertex -> 'a -> unit
    val find_vertex : 'a t -> vertex -> 'a
    val add_edge : 'a t -> vertex -> vertex -> unit
    val find_cycle : 'a t -> (vertex list) option
  end
= struct

    type state = Remained | Touched | Done

    type vertex = Vertex.t

    module DestSet = Set.Make(
      struct
        type t = vertex
        let compare = Vertex.compare
      end)

    type 'a t = (vertex, 'a * state ref * (DestSet.t) ref) Hashtbl.t


    exception Cyclic
    exception Loop of vertex
    exception UndefinedSourceVertex
    exception UndefinedDestinationVertex


    let create initsize = Hashtbl.create initsize


    let add_vertex dg vtx label =
      if Hashtbl.mem dg vtx then () else
        Hashtbl.add dg vtx (label, ref Remained, ref DestSet.empty)


    let find_vertex dg vtx =
      try
        let (label, _, _) = Hashtbl.find dg vtx in
          label
      with
      | Not_found -> raise UndefinedSourceVertex


    let add_edge dg vtx1 vtx2 =
      if not (Hashtbl.mem dg vtx2) then
        raise UndefinedDestinationVertex
      else
        let (_, _, destsetref) =
          try Hashtbl.find dg vtx1 with
          | Not_found -> raise UndefinedSourceVertex
        in
          if DestSet.mem vtx2 (!destsetref) then () else
            destsetref := DestSet.add vtx2 (!destsetref)


    let find_cycle dg =
      let rec aux vtx1 =
        try
          let (_, sttref, destsetref) = Hashtbl.find dg vtx1 in
            match !sttref with
            | Done     -> ()
            | Touched  -> raise Cyclic
            | Remained ->
                begin
                  sttref := Touched ;
                  DestSet.iter aux (!destsetref) ;
                  sttref := Done ;
                end
        with
        | Not_found -> assert false
      in
        try
          begin
            Hashtbl.iter (fun vtx1 (_, sttref, destsetref) ->
              begin
                begin
                  if DestSet.mem vtx1 (!destsetref) then
                    raise (Loop(vtx1))
                  else
                    ()
                end ;
                match !sttref with
                | Remained -> aux vtx1
                | _        -> ()
              end
            ) dg ;
            None
          end
        with
        | Loop(vtx) -> Some([vtx])
        | Cyclic ->
            let cycle =
              Hashtbl.fold (fun vtx1 (_, sttref, _) lst ->
                match !sttref with
                | Touched -> vtx1 :: lst
                | _       -> lst
              ) dg []
            in
              Some(cycle)

  end


module DependencyGraph = DirectedGraph(
  struct
    type t = type_name
    let compare = compare
  end)


type type_mode =
  | VariantMode
  | SynonymMode


type dependency_mode =
  | NoDependency
  | DependentMode of type_mode * type_name * (type_mode * Typeid.t * int) DependencyGraph.t


(* PUBLIC *)
let empty = ([], [])


let find_definition (defedtylst : defined_type_list) (tynm : type_name) : Typeid.t * definition =
  let rec aux lst =
    match lst with
    | []                                            -> raise Not_found
    | (tynmx, tyid, dfn) :: tail  when tynmx = tynm -> (tyid, dfn)
    | _ :: tail                                     -> aux tail
  in
    aux defedtylst


(* PUBLIC *)
let find_type_id ((defedtylst, _) : t) (tynm : type_name) =
  let (tyid, _) = find_definition defedtylst tynm in tyid


(* PUBLIC *)
let find_type_name ((defedtylst, _) : t) (tyid : Typeid.t) =
  let rec aux lst =
    match lst with
    | []                                                  -> raise Not_found
    | (tynm, tyidx, _) :: tail  when Typeid.eq tyidx tyid -> tynm
    | _ :: tail                                           -> aux tail
  in
    aux defedtylst


(* PUBLIC *)
let add ((defedtylst, varntenvmain) : t) (constrnm : constructor_name) (paramlist : (type_variable_info ref) list) (ty : mono_type) (varntnm : type_name) =
  let (tyid, _) = find_definition defedtylst varntnm in
  let rec aux accrev lst =
    match lst with
    | []                                      -> (constrnm, tyid, paramlist, ty) :: varntenvmain
    | (c, v, p, t) :: tail  when c = constrnm -> List.rev_append accrev ((constrnm, tyid, paramlist, ty) :: tail)
    | (c, v, p, t) :: tail                    -> aux ((c, v, p, t) :: accrev) tail
  in
    (defedtylst, aux [] varntenvmain)


(* PUBLIC *)
let add_list = List.fold_left (fun ve (c, v, p, t) -> add ve c v p t)


let fix_manual_type_general (dpmode : dependency_mode) (varntenv : t) (tyargmode : type_argument_mode) (mnty : manual_type) =
  let rec aux mnty =
    let (defedtylst, varntenvmain) = varntenv in
    let (rng, mntymain) = mnty in
    let iter = aux in
    let error tynm lenexp lenerr = raise (IllegalNumberOfTypeArguments(rng, tynm, lenexp, lenerr)) in
    let tymainnew =
      match mntymain with

      | MFuncType(mntydom, mntycod)    -> FuncType(iter mntydom, iter mntycod)
      | MProductType(mntylist)         -> ProductType(List.map iter mntylist)
      | MRecordType(mnasc)             -> RecordType(Assoc.map_value iter mnasc)

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
          let find_in_variant_environment () =
            try
              match find_definition defedtylst tynm with
              | (tyid, Data(argnum)) ->
                  if argnum <> len then error tynm argnum len else
                    VariantType(List.map iter mntyarglist, tyid)
(*
              | (tyid, Synonym(argnum, pty)) ->
                  if argnum <> len then error tynm argnum len else
                    TypeSynonym(List.map iter mntyarglist, tyid, pty)
*) (* temporary *)
            with
            | Not_found -> raise (UndefinedTypeName(rng, tynm))
          in
          begin
            match dpmode with
            | NoDependency -> find_in_variant_environment ()
            | DependentMode(objtymode, objtynm, dg) ->
                begin
                  try
                    let (tymode, tyid, objlen) = DependencyGraph.find_vertex dg tynm in
                      if len <> objlen then error tynm objlen len else
                      begin
                        begin
                          match objtymode with
                          | VariantMode -> ()
                          | SynonymMode ->
                              let () = print_for_debug_variantenv ("AddE " ^ objtynm ^ " ----> " ^ tynm) in (* for debug*)
                                DependencyGraph.add_edge dg objtynm tynm
                        end ;
                        match tymode with
                        | VariantMode -> VariantType(List.map iter mntyarglist, tyid)
                        | SynonymMode -> VariantType(List.map iter mntyarglist, tyid) (* temporary *)
                      end
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
                        let tvid = Tyvarid.fresh UniversalKind Quantifiable () (* temporary *) in
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
        (List.map (fun (_, tvref) -> tvref) (MapList.to_list tyargmaplist), ty)


let fix_manual_type (dpmode : dependency_mode) (varntenv : t) (tyargcons : untyped_type_argument_cons) (mnty : manual_type) =
  let tyargmaplist = MapList.create () in
  let rec aux cons =
    match cons with
    | UTEndOfTypeArgument                      -> ()
    | UTTypeArgumentCons(_, tyargnm, tailcons) ->
       let tvid = Tyvarid.fresh UniversalKind Quantifiable () (* temporary *) in
       begin MapList.add tyargmaplist tyargnm (ref (Free(tvid))) ; aux tailcons end
  in
  begin
    aux tyargcons ;
    fix_manual_type_general dpmode varntenv (StrictMode(tyargmaplist)) mnty
  end


(* PUBLIC *)
let fix_manual_type_for_inner_and_outer (qtfbl : quantifiability) (varntenv : t) (mnty : manual_type) =
  let tyargmaplist = MapList.create () in
  let tyin  = fix_manual_type_general NoDependency varntenv (FreeMode(tyargmaplist)) mnty in
  let tyout = fix_manual_type_general NoDependency varntenv (FreeMode(tyargmaplist)) mnty in
    (tyin, tyout)


let rec type_argument_length tyargcons =
  match tyargcons with
  | UTEndOfTypeArgument                -> 0
  | UTTypeArgumentCons(_, _, tailcons) -> 1 + (type_argument_length tailcons)


let register_variant_type (dg : (type_mode * Typeid.t * int) DependencyGraph.t) (varntenv : t) (len : int) (tynm : type_name) =
  let (defedtypelist, varntenvmain) = varntenv in
  print_for_debug_variantenv ("RV " ^ tynm) ; (* for debug *)
  try
    let (_, tyid, _) = DependencyGraph.find_vertex dg tynm in
      ((tynm, tyid, Data(len)) :: defedtypelist, varntenvmain)
  with
  | DependencyGraph.UndefinedSourceVertex -> failwith ("'" ^ tynm ^ "' not defined")


let register_synonym_type (dg : (type_mode * Typeid.t * int) DependencyGraph.t) (varntenv : t)
                  (tynm : type_name) (len : int) (ty : mono_type) =
  let (defedtypelist, varntenvmain) = varntenv in
  print_for_debug_variantenv ("RS " ^ tynm) ; (* for debug *)
  try
    let (_, tyid, _) = DependencyGraph.find_vertex dg tynm in
      ((tynm, tyid, Synonym(len, ty)) :: defedtypelist, varntenvmain)
  with
  | DependencyGraph.UndefinedSourceVertex -> failwith ("'" ^ tynm ^ "' not defined")

(*
(* PUBLIC *)
let rec apply_to_type_synonym (tyarglist : mono_type list) (pty : poly_type) =
  match (tyarglist, pty) with
  | (tyarghd :: tyargtl, Forall(tvid, kdstr, ptysub)) ->
      let ptynew = Typeenv.replace_id_poly [(tvid, tyarghd)] pty in
        apply_to_type_synonym tyargtl ptynew
  | ([], Forall(_, _, _))                             -> assert false
  | ([], Mono(ty))                                    -> ty
  | (_ :: _, Mono(_))                                 -> assert false
*) (* temporary *)

let rec add_variant_cons (dg : (type_mode * Typeid.t * int) DependencyGraph.t) (varntenv : t)
                           (tyargcons : untyped_type_argument_cons) (varntnm : type_name) (utvc : untyped_variant_cons) =

  let rec aux varntenv tyargcons utvc =
    let (rng, utvcmain) = utvc in
      match utvcmain with
      | UTEndOfVariant                          -> varntenv
      | UTVariantCons(constrnm, mnty, tailcons) ->
          let (paramlist, ty) = fix_manual_type (DependentMode(VariantMode, varntnm, dg)) varntenv tyargcons mnty in
          let varntenvnew = add varntenv constrnm paramlist ty varntnm in
            aux varntenvnew tyargcons tailcons
  in
  let tyarglen = type_argument_length tyargcons in
  let varntenvreg = register_variant_type dg varntenv tyarglen varntnm in
    aux varntenvreg tyargcons utvc







(* PUBLIC *)
let rec add_mutual_cons (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =

  let dg = DependencyGraph.create 32 in

  let rec register_each_type_name mutvarntcons =
    let iter = register_each_type_name in
    match mutvarntcons with
    | UTEndOfMutualVariant -> ()
    | UTMutualVariantCons(tyargcons, tynm, _, tailcons) ->
        let () = print_for_debug_variantenv ("Add1 " ^ tynm) in (* for debug *)
        let tyid = Typeid.fresh () in
        begin
          DependencyGraph.add_vertex dg tynm (VariantMode, tyid, type_argument_length tyargcons) ;
          iter tailcons
        end

    | UTMutualSynonymCons(tyargcons, tynm, _, tailcons) ->
        let () = print_for_debug_variantenv ("Add2 " ^ tynm) in (* for debug *)
        let tyid = Typeid.fresh () in
        begin
          DependencyGraph.add_vertex dg tynm (SynonymMode, tyid, type_argument_length tyargcons) ;
          iter tailcons
        end
  in

  let rec add_dependency_of_each_synonym_type varntenv mutvarntcons =
    let iter = add_dependency_of_each_synonym_type in
    match mutvarntcons with
    | UTEndOfMutualVariant -> varntenv
    | UTMutualVariantCons(tyargcons, tynm, utvc, tailcons) -> iter varntenv tailcons
        (* fix_manual_type (DependentMode(VariantMode, tynm, dg)) varntenv tyargcons *)
    | UTMutualSynonymCons(tyargcons, tynm, mnty, tailcons) ->
        let (_, ty) = fix_manual_type (DependentMode(SynonymMode, tynm, dg)) varntenv tyargcons mnty in
        let len = type_argument_length tyargcons in
        let varntenvnew = register_synonym_type dg varntenv tynm len ty in
          iter varntenvnew tailcons
  in
  begin
    register_each_type_name mutvarntcons ;
    let varntenvnew = add_dependency_of_each_synonym_type varntenv mutvarntcons in
    let cycleopt = DependencyGraph.find_cycle dg in
      match cycleopt with
      | Some(lst) -> raise (CyclicTypeDefinition(lst))
      | None      ->
          let varntenv_fin = read_variant_spec dg varntenvnew mutvarntcons in
            varntenv_fin
  end
(*  let varntenv_syn = read_synonym_spec dg varntenv mutvarntcons in *)

(*
and read_synonym_spec dg (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
    match mutvarntcons with
    | UTEndOfMutualVariant                                    -> varntenv
    | UTMutualVariantCons(_, _, _, tailcons)                  -> read_synonym_spec dg varntenv tailcons
    | UTMutualSynonymCons(tyargcons, tysynnm, mnty, tailcons) ->
        let varntenv_new = register_synonym_type dg varntenv tyargcons tysynnm mnty in
          read_synonym_spec dg varntenv_new tailcons
*)

and read_variant_spec dg (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
    match mutvarntcons with
    | UTEndOfMutualVariant                                     -> varntenv
    | UTMutualVariantCons(tyargcons, varntnm, utvc, tailcons)  ->
        let varntenvnew = add_variant_cons dg varntenv tyargcons varntnm utvc in
          read_variant_spec dg varntenvnew tailcons
    | UTMutualSynonymCons(_, _, _, tailcons)                   -> read_variant_spec dg varntenv tailcons

(*
(* PUBLIC *)
and add_mutual_cons_hidden (mdlnm : module_name) (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
  memo_all_name mdlnm varntenv mutvarntcons


and memo_variant_name (mdlnm : module_name) (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
  match mutvarntcons with
  | UTEndOfMutualVariant                                 -> varntenv
  | UTMutualVariantCons(tyargcons, varntnm, _, tailcons) ->
      let mdlvarntnm = append_module_name mdlnm varntnm in
      let tyarglen = type_argument_length tyargcons in
      let varntenvnew = register_variant_type dg varntenv tyarglen mdlvarntnm in
        memo_variant_name mdlnm varntenvnew tailcons
  | UTMutualSynonymCons(tyargcons, tysynnm, _, tailcons) -> memo_variant_name mdlnm varntenv tailcons


and memo_all_name (mdlnm : module_name) (varntenv : t) (mutvarntcons : untyped_mutual_variant_cons) =
  match mutvarntcons with
  | UTEndOfMutualVariant                                 -> varntenv
  | UTMutualVariantCons(tyargcons, varntnm, _, tailcons) ->
      let mdlvarntnm = append_module_name mdlnm varntnm in
      let tyarglen   = type_argument_length tyargcons in
      let varntenv_new = register_variant_type varntenv tyarglen mdlvarntnm in
        memo_all_name mdlnm varntenv_new tailcons
  | UTMutualSynonymCons(tyargcons, tysynnm, _, tailcons) ->
      let mdltysynnm = append_module_name mdlnm tysynnm in
      let tyarglen   = type_argument_length tyargcons in
      let varntenv_new = register_variant_type varntenv tyarglen mdltysynnm in
        memo_all_name mdlnm varntenv_new tailcons
*)

(* PUBLIC *)
let rec find (varntenv : t) (constrnm : constructor_name) =
  let (_, varntenvmain) = varntenv in
    let rec aux varntenvmain =
      match varntenvmain with
      | []                                      -> raise Not_found
      | (c, v, p, t) :: tail  when c = constrnm -> (v, p, t)
      | _ :: tail                               -> aux tail
    in
      aux varntenvmain
