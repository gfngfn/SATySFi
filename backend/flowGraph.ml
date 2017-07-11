
module Heap = Core.Heap.Removable


module type SchemeType =
  sig
    type t
    type weight
    val equal : t -> t -> bool
    val hash : t -> int
    val add : weight -> weight -> weight
    val compare : weight -> weight -> int
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

    type label = Infinite | Finite of weight * vertex option

    exception UndefinedSourceVertex
    exception UndefinedDestinationVertex

    type t = (weight DestinationTable.t * ((vertex Heap.Elt.t) option) ref * label ref) MainTable.t

    let ( +@ ) = GraphScheme.add

    let ( <@ ) wgt1 wgt2 = (GraphScheme.compare wgt1 wgt2) < 0

    let weight_zero = GraphScheme.zero

    let equal_vertex = GraphScheme.equal


    let create () =
      MainTable.create 32


    let add_vertex (grph : t) (vtx : vertex) : unit =
      let dstbl = DestinationTable.create 32 in
      let vheref = ref None in
      let lblref = ref Infinite in
        MainTable.add grph vtx (dstbl, vheref, lblref)


    let add_edge (grph : t) (vtx1 : vertex) (vtx2 : vertex) (wgt : weight) : unit =
      let (dstbl1, _, _) =
        try MainTable.find grph vtx1 with
        | Not_found -> raise UndefinedSourceVertex
      in
        if not (MainTable.mem grph vtx2) then
          raise UndefinedDestinationVertex
        else
          DestinationTable.add dstbl1 vtx2 wgt


    let compare_vertex (grph : t) (vtx1 : vertex) (vtx2 : vertex) =
      let ((_, _, lblref1), (_, _, lblref2)) =
        try (MainTable.find grph vtx1, MainTable.find grph vtx2) with
        | Not_found -> assert false
      in
        match (!lblref1, !lblref2) with
        | (_, Infinite)                  -> 1
        | (Infinite, _)                  -> -1
        | (Finite(d1, _), Finite(d2, _)) -> GraphScheme.compare d1 d2


    let shortest_path (grph : t) (vtxS : vertex) (vtxT : vertex) : (vertex list) option =

      let hp : vertex Heap.t = Heap.create ~cmp:(compare_vertex grph) () in

      let rec aux () =
        match Heap.pop hp with
        | None       -> None
        | Some(vtxP) ->
            let (dstblP, vherefP, vheP, lblrefP) =
              try
                let (dstblP, vherefP, lblrefP) = MainTable.find grph vtxP in
                  match !vherefP with
                  | None       -> assert false
                  | Some(vheP) -> (dstblP, vherefP, vheP, lblrefP)
              with
              | Not_found -> assert false
            in
            match !lblrefP with
            | Infinite         -> (* -- when Infinite is the least element in `hp`, i.e. `vtxT` is unreachable -- *)
                None
            | Finite(distP, _) ->
                begin
                  if equal_vertex vtxP vtxS then
                    Some([]) (* temporary; should backtrack vertices and return the path *)
                  else
                    begin
                      dstblP |> DestinationTable.iter (fun vtx wgt ->
                        let (_, vheref, lblref) =
                          try MainTable.find grph vtx with Not_found -> assert false
                        in
                          match !vheref with
                          | None      -> ()  (* -- when `vtx` is not a member of `hp`; equivalent to `Heap.mem ?equal:equal_vertex hp vtx` -- *)
                          | Some(vhe) ->
                              match !lblref with
                              | Infinite        -> begin lblref := Finite(distP +@ wgt, Some(vtxP)) end
                              | Finite(dist, _) ->
                                  let distfromP = distP +@ wgt in
                                    if distfromP <@ dist then
                                      begin lblref := Finite(distfromP, Some(vtxP)) end
                                    else ()
                      ) ;
                    Heap.remove hp vheP ;
                    vherefP := None ;
                    aux ()
                    end
                end
      in

      let (_, _, lblrefS) =
        try MainTable.find grph vtxS with
        | Not_found -> raise UndefinedSourceVertex
      in
        if not (MainTable.mem grph vtxT) then raise UndefinedDestinationVertex else
          begin
            (* -- initialization -- *)
            lblrefS := Finite(weight_zero, None) ;
            grph |> MainTable.iter (fun vtx (_, vheref, _) ->
              if equal_vertex vtx vtxS then () else
                let vhe = Heap.add_removable hp vtx in
                begin vheref := Some(vhe) end
            ) ;
            (* -- main iteration -- *)
            aux ()
          end

  end
