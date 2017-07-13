
let print_for_debug msg =
  print_endline msg


module Heap = Core.Heap.Removable


module type VertexType =
  sig
    type t
    val equal : t -> t -> bool
    val hash : t -> int
    val show : t -> string (* for debug *)
  end

module type WeightType =
  sig
    type t
    val show : t -> string (* for debug *)
    val add : t -> t -> t
    val compare : t -> t -> int
    val zero : t
  end


module Make (Vertex : VertexType) (Weight : WeightType)
: sig
    type t
    type vertex = Vertex.t
    type weight = Weight.t
    exception UndefinedSourceVertex
    exception UndefinedDestinationVertex
    val create : unit -> t
    val add_vertex : t -> vertex -> unit
    val add_edge : t -> vertex -> vertex -> weight -> unit
    val shortest_path : t -> vertex -> vertex -> (vertex list) option
  end
= struct
    module MainTable = Hashtbl.Make(Vertex)
    module DestinationTable = Hashtbl.Make(Vertex)

    type vertex = Vertex.t
    type weight = Weight.t

    type label = Infinite | Finite of weight * vertex option

    exception UndefinedSourceVertex
    exception UndefinedDestinationVertex

    type t = (weight DestinationTable.t * ((vertex Heap.Elt.t) option) ref * label ref) MainTable.t

    let ( +@ ) = Weight.add

    let ( <@ ) wgt1 wgt2 = (Weight.compare wgt1 wgt2) < 0

    let weight_zero = Weight.zero

    let equal_vertex = Vertex.equal


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
        let () = print_for_debug ("add (" ^ (Vertex.show vtx1) ^ " ----> " ^ (Vertex.show vtx2) ^ ") : " ^ (Weight.show wgt)) in (* for debug *)
          DestinationTable.add dstbl1 vtx2 wgt


    let compare_vertex (grph : t) (vtx1 : vertex) (vtx2 : vertex) =
      let ((_, _, lblref1), (_, _, lblref2)) =
        try (MainTable.find grph vtx1, MainTable.find grph vtx2) with
        | Not_found -> assert false
      in
        match (!lblref1, !lblref2) with
        | (Infinite, _)                  -> 1
        | (_, Infinite)                  -> -1
        | (Finite(d1, _), Finite(d2, _)) -> Weight.compare d1 d2


    let shortest_path (grph : t) (vtxS : vertex) (vtxT : vertex) : (vertex list) option =

      let rec backtrack (acc : vertex list) (vtx : vertex) =
        let (_, _, lblref) =
          try MainTable.find grph vtx with
          | Not_found -> assert false
        in
          match !lblref with
          | Infinite                   -> assert false
          | Finite(_, None)            -> List.rev acc
          | Finite(_, Some(vtxparent)) -> backtrack (vtxparent :: acc) vtxparent
      in

      let hp : vertex Heap.t = Heap.create ~cmp:(compare_vertex grph) () in

      let rec aux () =
        match Heap.pop hp with
        | None       -> None
        | Some(vtxP) ->
            let () = print_for_debug ("see " ^ (Vertex.show vtxP)) in (* for debug *)
              if equal_vertex vtxP vtxT then
                let path = backtrack [] vtxT in 
                  Some(path)
              else
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
                | Infinite         ->  (* -- when Infinite is the least element in `hp`, i.e. `vtxT` is unreachable -- *)
                    let () = print_for_debug "| infinite" in (* for debug *)
                      None
                | Finite(distP, _) ->
                    let () = print_for_debug ("| " ^ (Weight.show distP)) in (* for debug *)
                    begin
                      dstblP |> DestinationTable.iter (fun vtx wgt ->
                        let () = print_for_debug ("|--> " ^ (Vertex.show vtx) ^ " " ^ (Weight.show wgt)) in (*for debug *)
                        let (_, vheref, lblref) =
                          try MainTable.find grph vtx with Not_found -> assert false
                        in
                          match !vheref with
                          | None      -> ()  (* -- when `vtx` is not a member of `hp`; equivalent to `Heap.mem ?equal:equal_vertex hp vtx` -- *)
                          | Some(vhe) ->
                              match !lblref with
                              | Infinite        ->
                                  let distfromP = distP +@ wgt in
                                  begin
                                    lblref := Finite(distP +@ wgt, Some(vtxP)) ;
                                    print_for_debug ("  update " ^ (Vertex.show vtx) ^ " infinite -> " ^ (Weight.show distfromP)) ;
                                    let vhenew = Heap.update hp vhe vtx in
                                    vheref := Some(vhenew) ;
                                  end
                              | Finite(dist, _) ->
                                  let distfromP = distP +@ wgt in
                                    if distfromP <@ dist then
                                      begin
                                        lblref := Finite(distfromP, Some(vtxP)) ;
                                        print_for_debug ("  update " ^ (Vertex.show vtx) ^ " " ^ (Weight.show dist) ^ " -> " ^ (Weight.show distfromP)) ;
                                        let vhenew = Heap.update hp vhe vtx in
                                        vheref := Some(vhenew) ;
                                      end
                                    else ()
                      ) ;
                      Heap.remove hp vheP ;
                      vherefP := None ;
                      aux ()
                    end
      in

      let (dstblS, _, lblrefS) =
        try MainTable.find grph vtxS with
        | Not_found -> raise UndefinedSourceVertex
      in
        if not (MainTable.mem grph vtxT) then raise UndefinedDestinationVertex else
          begin
            (* -- initialization -- *)
            lblrefS := Finite(weight_zero, None) ;
            dstblS |> DestinationTable.iter (fun vtx wgt ->
              let (_, _, lblref) =
                try MainTable.find grph vtx with
                | Not_found -> assert false
              in
              let () = print_for_debug ("set " ^ (Vertex.show vtx) ^ " " ^ (Weight.show wgt)) in (* for debug *)
              begin lblref := Finite(wgt, Some(vtxS)) ; end
            ) ;
            grph |> MainTable.iter (fun vtx (_, vheref, _) ->
              if equal_vertex vtx vtxS then () else
                let vhe = Heap.add_removable hp vtx in
                begin vheref := Some(vhe) ; end
            ) ;
            (* begin : for debug *)
            grph |> MainTable.iter (fun vtx (_, _, lblref) ->
              match !lblref with
              | Infinite       -> print_for_debug ("initially " ^ (Vertex.show vtx) ^ " : infinite")
              | Finite(wgt, _) -> print_for_debug ("initially " ^ (Vertex.show vtx) ^ " : " ^ (Weight.show wgt))
            ) ;
            (* end : for debug *)
            (* -- main iteration -- *)
            aux ()
          end

  end
