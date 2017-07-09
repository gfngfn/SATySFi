
module type SchemeType =
  sig
    type t
    type weight
    val equal : t -> t -> bool
    val hash : t -> int
    val add : weight -> weight -> weight
    val zero : weight
  end


module Make (GraphScheme : SchemeType)
: sig
    type t
    type vertex = GraphScheme.t
    type weight = GraphScheme.weight
    exception UndefinedSourceVertex
    exception UndefinedDestinationVertex
    val create : unit -> t
    val add_vertex : t -> vertex -> unit
    val add_edge : t -> vertex -> vertex -> weight -> unit
  end
= struct
    module MainTable = Hashtbl.Make(GraphScheme)
    module DestinationTable = Hashtbl.Make(GraphScheme)

    type vertex = GraphScheme.t
    type weight = GraphScheme.weight

    type label = Infinite | Finite of weight

    exception UndefinedSourceVertex
    exception UndefinedDestinationVertex

    type t = ((weight DestinationTable.t) * (label ref)) MainTable.t

    let ( +@ ) = GraphScheme.add

    let weight_zero = GraphScheme.zero


    let create () =
      MainTable.create 32


    let add_vertex (grph : t) (vtx : vertex) : unit =
      let dstbl = DestinationTable.create 32 in
      let lblref = ref Infinite in
        MainTable.add grph vtx (dstbl, lblref)


    let add_edge (grph : t) (vtx1 : vertex) (vtx2 : vertex) (wgt : weight) : unit =
      let (dstbl1, _) =
        try MainTable.find grph vtx1 with
        | Not_found -> raise UndefinedSourceVertex
      in
        if not (MainTable.mem grph vtx2) then
          raise UndefinedDestinationVertex
        else
          DestinationTable.add dstbl1 vtx2 wgt


    let shortest_path (grph : t) (vtx1 : vertex) (vtx2 : vertex) : (vertex list) option =
      let (dstbl1, lblref1) =
        try MainTable.find grph vtx1 with
        | Not_found -> raise UndefinedSourceVertex
      in
        if not (MainTable.mem grph vtx2) then
          raise UndefinedDestinationVertex
        else
          begin
            lblref1 := Finite(weight_zero) ;
            None (* temporary *)
          end

  end
