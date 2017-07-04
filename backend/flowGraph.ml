
module type SchemeType =
  sig
    type vertex
    type weight
  end


module Make (GraphScheme : SchemeType)
: sig
    type t
    type vertex = GraphScheme.vertex
    type weight = GraphScheme.weight
    exception UndefinedSourceVertex
    exception UndefinedDestinationVertex
    val create : unit -> t
    val add_vertex : t -> vertex -> unit
    val add_edge : t -> vertex -> vertex -> weight -> unit
  end
= struct
    module MainTable = Hashtbl
    module DestinationTable = Hashtbl

    type vertex = GraphScheme.vertex
    type weight = GraphScheme.weight

    exception UndefinedSourceVertex
    exception UndefinedDestinationVertex

    type t = (vertex, (vertex, weight) DestinationTable.t) MainTable.t


    let create () =
      MainTable.create 32


    let add_vertex (flgr : t) (vtx : vertex) =
      let dstbl = DestinationTable.create 32 in
        MainTable.add flgr vtx dstbl


    let add_edge (flgr : t) (vtx1 : vertex) (vtx2 : vertex) (wgt : weight) =
      let dstbl1 =
        try MainTable.find flgr vtx1 with
        | Not_found -> raise UndefinedSourceVertex
      in
        if not (Hashtbl.mem flgr vtx2) then
          raise UndefinedDestinationVertex
        else
          Hashtbl.add dstbl1 vtx2 wgt
  end
