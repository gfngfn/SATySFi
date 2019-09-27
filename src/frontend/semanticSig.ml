module type S = sig
  type ty
  type poly
  type kind
  type var

  val var_compare : var -> var -> int
end

module F(X : S) = struct
  type var_class =
    | V
    | C
    | T
    | M
  [@@deriving eq, ord]

  type label = var_class * string
  [@@deriving eq, ord]

  let show_label (cl, s) =
    match cl with
    | V -> "value '" ^ s ^ "'"
    | C -> "constructor '" ^ s ^ "'"
    | T -> "type '" ^ s ^ "'"
    | M -> "module '" ^ s ^ "'"

  module Struct = Map.Make(struct
    type t = label
    [@@deriving eq, ord]
  end)

  type direct =
    | Direct (* direct \x : ... *)
    | Indirect (* val x : ... *)

  type t =
    | AtomicType of X.ty * X.kind
    | AtomicTerm of {
        is_direct : direct;
        ty : X.poly;
      }
    | AtomicConstr of {
        var : X.var; (* Represents what abstract type this constructor belongs to. *)
        ty : X.ty; (* The type of the argument to this constructor. *)
      }
    | Structure of t Struct.t

  type var_info = {
    v : X.var;
    k : X.kind;
    location : label list;
  }

  module Exist : sig
    type 'a t

    val from_body : 'a -> 'a t
    val quantify1 : var_info -> 'a -> 'a t
    val quantify : var_info list -> 'a -> 'a t
    val get_body : 'a t -> 'a
    val get_quantifier : 'a t -> var_info list

    val map : ('a -> 'b) -> 'a t -> 'b t

    val map_with_location : ('a -> 'b) -> (label list -> label list) -> 'a t -> 'b t

    val merge : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  end = struct
    type 'a t =
      | Exist of var_info list * 'a

    let from_body x = Exist([], x)

    let quantify1 var x = Exist([var], x)

    let quantify vars x = Exist(vars, x)

    let get_body (Exist(_, x)) = x

    let get_quantifier (Exist(vs, _)) = vs

    let map f (Exist(vs, x)) = Exist(vs, f x)

    let map_with_location f g (Exist(vs, x)) =
      Exist(List.map (fun v -> {v with location = g v.location}) vs, f x)

    let merge f (Exist(vs1, x)) (Exist(vs2, y)) = Exist(List.append vs1 vs2, f x y)
  end

  type 'a exist = 'a Exist.t

  let from_body = Exist.from_body

  type ex_t = t exist

  module VMap = Map.Make(struct
    type t = X.var
    let compare = X.var_compare
  end)

  module M : sig
    val proj : t -> Struct.key -> t
    val projs : t -> Struct.key list -> t

    val show_location : label list -> string

    val subst : (X.ty VMap.t -> X.ty -> X.ty) ->
                (X.ty VMap.t -> X.poly -> X.poly) -> X.ty VMap.t -> t -> t

    val find_direct : t -> string list

    exception MissingLabel of Struct.key
  end = struct
    exception MissingLabel of Struct.key
    exception NotStructure

    let proj s l =
      match s with
      | Structure(s) ->
          begin
            match Struct.find_opt l s with
            | Some(s) -> s
            | None    -> raise (MissingLabel(l))
          end
      | _ -> raise NotStructure

    let rec projs s = function
      | []      -> s
      | l :: ls -> projs (proj s l) ls

    let rec show_location = function
      | []           -> ""
      | [(_, s)]     -> s
      | (_, s) :: ls -> s ^ "." ^ show_location ls

    let subst subst_type subst_poly tys =
      let rec aux = function
        | AtomicType(ty, k)             -> AtomicType(subst_type tys ty, k)
        | AtomicTerm{is_direct; ty = p} -> AtomicTerm{is_direct; ty = subst_poly tys p}
        | AtomicConstr{var; ty}         -> AtomicConstr{var; ty = subst_type tys ty}
        | Structure(s)                  -> Structure(Struct.map aux s)
      in
        aux

    (* This function only works if signatures are limited to flattened ones. *)
    let rec find_direct_aux l = function
      | AtomicTerm{is_direct = Direct} -> [l]
      | Structure(s)                   ->
          let f (_, l1) s1 xs = find_direct_aux l1 s1 @ xs in
          Struct.fold f s []
      | _ -> []

    let find_direct = find_direct_aux "<dummy>"
  end

  include M
end
