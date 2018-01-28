
let print_for_debug msg =
  ()
(*
  print_endline msg
*)

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

    type label =
      | Infinite
          (* -- stands for being unreachable -- *)

      | Finite of weight * vertex option
          (* --
             (1) distance from the source vertex
             (2) "parent" vertex ('None' if it is the source vertex)
             --*)

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
      match MainTable.find_opt grph vtx1 with
      | None ->
          raise UndefinedSourceVertex

      | Some((dstbl1, _, _)) ->
          if not (MainTable.mem grph vtx2) then
            raise UndefinedDestinationVertex
          else
          let () = print_for_debug ("add (" ^ (Vertex.show vtx1) ^ " ----> " ^ (Vertex.show vtx2) ^ ") : " ^ (Weight.show wgt)) in (* for debug *)
            DestinationTable.add dstbl1 vtx2 wgt


    let compare_vertex (grph : t) (vtx1 : vertex) (vtx2 : vertex) =
      match (MainTable.find_opt grph vtx1, MainTable.find_opt grph vtx2) with
      | (None, _)
      | (_, None)
          -> assert false

      | (Some((_, _, lblref1)), Some((_, _, lblref2))) ->
          begin
            match (!lblref1, !lblref2) with
            | (Infinite, _)                  -> 1
            | (_, Infinite)                  -> -1
            | (Finite(d1, _), Finite(d2, _)) -> Weight.compare d1 d2
          end


    let shortest_path (grph : t) (vtxS : vertex) (vtxT : vertex) : (vertex list) option =

      let rec backtrack (acc : vertex Alist.t) (vtx : vertex) =
        match MainTable.find_opt grph vtx with
        | None -> assert false
        | Some((_, _, lblref)) ->
            begin
              match !lblref with
              | Infinite                   -> None  (* -- unreachable -- *)
              | Finite(_, None)            -> Some(Alist.to_list acc)
              | Finite(_, Some(vtxparent)) -> backtrack (Alist.extend acc vtxparent) vtxparent
            end
      in

      let hp : vertex Heap.t = Heap.create ~cmp:(compare_vertex grph) () in

      let rec aux () =
        match Heap.pop hp with
        | None       -> None
        | Some(vtxP) ->
            let () = print_for_debug ("see " ^ (Vertex.show vtxP)) in (* for debug *)
              if equal_vertex vtxP vtxT then
                let pathopt = backtrack Alist.empty vtxT in
                  pathopt
              else
                let (dstblP, vherefP, vheP, lblrefP) =
                  match MainTable.find_opt grph vtxP with
                  | None ->
                      assert false

                  | Some((dstblP, vherefP, lblrefP)) ->
                      begin
                        match !vherefP with
                        | None       -> assert false
                        | Some(vheP) -> (dstblP, vherefP, vheP, lblrefP)
                      end
                in
                match !lblrefP with
                | Infinite ->  (* -- when Infinite is the least element in `hp`, i.e. `vtxT` is unreachable -- *)
                    let () = print_for_debug "| infinite" in (* for debug *)
                      None

                | Finite(distP, _) ->
                    let () = print_for_debug ("| " ^ (Weight.show distP)) in (* for debug *)
                    begin
                      dstblP |> DestinationTable.iter (fun vtx wgt ->
                        let () = print_for_debug ("|--> " ^ (Vertex.show vtx) ^ " " ^ (Weight.show wgt)) in (*for debug *)
                        match MainTable.find_opt grph vtx with
                        | None ->
                            assert false

                        | Some((_, vheref, lblref)) ->
                            begin
                              match !vheref with
                              | None ->  (* -- when `vtx` is not a member of `hp`; equivalent to `Heap.mem ?equal:equal_vertex hp vtx` -- *)
                                  ()

                              | Some(vhe) ->
                                  begin
                                    match !lblref with
                                    | Infinite ->
                                        let distfromP = distP +@ wgt in
                                        begin
                                          lblref := Finite(distP +@ wgt, Some(vtxP));
                                          print_for_debug ("  update " ^ (Vertex.show vtx) ^ " infinite -> " ^ (Weight.show distfromP));
                                          let vhenew = Heap.update hp vhe vtx in
                                          vheref := Some(vhenew);
                                        end
                                    | Finite(dist, _) ->
                                        let distfromP = distP +@ wgt in
                                          if distfromP <@ dist then
                                            begin
                                              lblref := Finite(distfromP, Some(vtxP));
                                              print_for_debug ("  update " ^ (Vertex.show vtx) ^ " " ^ (Weight.show dist) ^ " -> " ^ (Weight.show distfromP));
                                              let vhenew = Heap.update hp vhe vtx in
                                              vheref := Some(vhenew);
                                            end
                                          else ()
                                  end
                            end
                      );
                      Heap.remove hp vheP;
                      vherefP := None;
                      aux ()
                    end
      in

      match MainTable.find_opt grph vtxS with
      | None ->
          raise UndefinedSourceVertex

      | Some((dstblS, _, lblrefS)) ->
          if not (MainTable.mem grph vtxT) then
            raise UndefinedDestinationVertex
          else
            begin
              (* -- initialization -- *)
              lblrefS := Finite(weight_zero, None);
              dstblS |> DestinationTable.iter (fun vtx wgt ->
                  match MainTable.find_opt grph vtx with
                  | None ->
                      assert false

                  | Some((_, _, lblref)) ->
                      let () = print_for_debug ("set " ^ (Vertex.show vtx) ^ " " ^ (Weight.show wgt)) in (* for debug *)
                      begin lblref := Finite(wgt, Some(vtxS)); end
              );
              grph |> MainTable.iter (fun vtx (_, vheref, _) ->
                if equal_vertex vtx vtxS then () else
                  let vhe = Heap.add_removable hp vtx in
                  begin vheref := Some(vhe); end
              );

              (* begin : for debug *)
              grph |> MainTable.iter (fun vtx (_, _, lblref) ->
                match !lblref with
                | Infinite       -> print_for_debug ("initially " ^ (Vertex.show vtx) ^ " : infinite")
                | Finite(wgt, _) -> print_for_debug ("initially " ^ (Vertex.show vtx) ^ " : " ^ (Weight.show wgt))
              );
              (* end : for debug *)

              (* -- main iteration -- *)
              aux ()
            end

  end
