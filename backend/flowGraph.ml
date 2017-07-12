
let print_for_debug msg =
  print_endline msg


module Heap = Core.Heap.Removable


module type SchemeType =
  sig
    type t
    type weight
    val equal : t -> t -> bool
    val hash : t -> int
    val show : t -> string (* for debug *)
    val show_weight : weight -> string (* for debug *)
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
    val shortest_path : t -> vertex -> vertex -> (vertex list) option
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
        let () = print_for_debug ("add (" ^ (GraphScheme.show vtx1) ^ " ----> " ^ (GraphScheme.show vtx2) ^ ") : " ^ (GraphScheme.show_weight wgt)) in (* for debug *)
          DestinationTable.add dstbl1 vtx2 wgt


    let compare_vertex (grph : t) (vtx1 : vertex) (vtx2 : vertex) =
      let ((_, _, lblref1), (_, _, lblref2)) =
        try (MainTable.find grph vtx1, MainTable.find grph vtx2) with
        | Not_found -> assert false
      in
        match (!lblref1, !lblref2) with
        | (Infinite, _)                  -> 1
        | (_, Infinite)                  -> -1
        | (Finite(d1, _), Finite(d2, _)) -> GraphScheme.compare d1 d2


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
            let () = print_for_debug ("see " ^ (GraphScheme.show vtxP)) in (* for debug *)
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
                    let () = print_for_debug ("| " ^ (GraphScheme.show_weight distP)) in (* for debug *)
                    begin
                      dstblP |> DestinationTable.iter (fun vtx wgt ->
                        let () = print_for_debug ("|--> " ^ (GraphScheme.show vtx) ^ " " ^ (GraphScheme.show_weight wgt)) in (*for debug *)
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
                                    print_for_debug ("  update " ^ (GraphScheme.show vtx) ^ " infinite -> " ^ (GraphScheme.show_weight distfromP)) ;
                                    let vhenew = Heap.update hp vhe vtx in
                                    vheref := Some(vhenew) ;
                                  end
                              | Finite(dist, _) ->
                                  let distfromP = distP +@ wgt in
                                    if distfromP <@ dist then
                                      begin
                                        lblref := Finite(distfromP, Some(vtxP)) ;
                                        print_for_debug ("  update " ^ (GraphScheme.show vtx) ^ " " ^ (GraphScheme.show_weight dist) ^ " -> " ^ (GraphScheme.show_weight distfromP)) ;
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
              let () = print_for_debug ("set " ^ (GraphScheme.show vtx) ^ " " ^ (GraphScheme.show_weight wgt)) in (* for debug *)
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
              | Infinite       -> print_for_debug ("initially " ^ (GraphScheme.show vtx) ^ " : infinite")
              | Finite(wgt, _) -> print_for_debug ("initially " ^ (GraphScheme.show vtx) ^ " : " ^ (GraphScheme.show_weight wgt))
            ) ;
            (* end : for debug *)
            (* -- main iteration -- *)
            aux ()
          end

  end
