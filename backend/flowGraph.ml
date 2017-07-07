
module type SchemeType =
  sig
    type t
    type weight
    val equal : t -> t -> bool
    val hash : t -> int
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

    exception UndefinedSourceVertex
    exception UndefinedDestinationVertex

    type t = (weight DestinationTable.t) MainTable.t


    let create () =
      MainTable.create 32


    let add_vertex (flgr : t) (vtx : vertex) : unit =
      let dstbl = DestinationTable.create 32 in
        MainTable.add flgr vtx dstbl


    let add_edge (flgr : t) (vtx1 : vertex) (vtx2 : vertex) (wgt : weight) : unit =
      let dstbl1 =
        try MainTable.find flgr vtx1 with
        | Not_found -> raise UndefinedSourceVertex
      in
        if not (MainTable.mem flgr vtx2) then
          raise UndefinedDestinationVertex
        else
          DestinationTable.add dstbl1 vtx2 wgt
  end
