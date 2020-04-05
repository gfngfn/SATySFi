
let print_for_debug msg =
(*
  print_endline msg;
*)
  ()

module Heap = Pairing_heap


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
    exception AlreadyDefinedVertex
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
    exception AlreadyDefinedVertex

    type t = (weight DestinationTable.t * ((vertex Heap.Elt.t) option) ref * label ref) MainTable.t
      (* --
         the main table maps each vertex to
         (1) its destination table,
         (2) its representation in the heap, and
         (3) its label that indicates
               the reachability and the distance from the source vertex
         -- *)

    let ( +@ ) = Weight.add

    let ( <@ ) wgt1 wgt2 = (Weight.compare wgt1 wgt2) < 0

    let weight_zero = Weight.zero


    let create () =
      MainTable.create 32


    let add_vertex (grph : t) (vtx : vertex) : unit =
      print_for_debug ("addV " ^ (Vertex.show vtx));
      if MainTable.mem grph vtx then
        raise (AlreadyDefinedVertex)
      else
        let dstbl = DestinationTable.create 32 in
        let vheref = ref None in
        let lblref = ref Infinite in
          MainTable.add grph vtx (dstbl, vheref, lblref)


    let add_edge (grph : t) (vtx1 : vertex) (vtx2 : vertex) (wgt : weight) : unit =
      let () = print_for_debug ("addE (" ^ (Vertex.show vtx1) ^ " ----> " ^ (Vertex.show vtx2) ^ ") : " ^ (Weight.show wgt)) in (* for debug *)
      match MainTable.find_opt grph vtx1 with
      | None ->
          raise UndefinedSourceVertex

      | Some((dstbl1, _, _)) ->
          if not (MainTable.mem grph vtx2) then
            raise UndefinedDestinationVertex
          else
            DestinationTable.add dstbl1 vtx2 wgt


    let compare_vertex (grph : t) (vtx1 : vertex) (vtx2 : vertex) =
      match (MainTable.find_opt grph vtx1, MainTable.find_opt grph vtx2) with
      | (None, _)
      | (_, None)
        (* -- when either 'vtx1' or 'vtx2' does not belong to 'grph'; it cannot happen -- *)
          -> assert false

      | (Some((_, _, lblref1)), Some((_, _, lblref2))) ->
          begin
            match (!lblref1, !lblref2) with
            | (Infinite, _)                  -> 1
            | (_, Infinite)                  -> -1
            | (Finite(d1, _), Finite(d2, _)) -> Weight.compare d1 d2
          end


    let shortest_path (grph : t) (vtxS : vertex) (vtxT : vertex) : (vertex list) option =
      let () = print_for_debug ("SHORTEST_PATH (" ^ (Vertex.show vtxS) ^ " ---> " ^ (Vertex.show vtxT) ^ ")") in (* for debug *)

      let rec backtrack (acc : vertex Alist.t) (vtx : vertex) : (vertex list) option =
        match MainTable.find_opt grph vtx with
        | None ->
          (* -- when the vertex 'vtx' does not belong to 'grph'; it cannot happen -- *)
            assert false

        | Some((_, _, lblref)) ->
            begin
              match !lblref with
              | Infinite                   -> None  (* -- unreachable -- *)
              | Finite(_, None)            -> Some(Alist.to_list acc)
              | Finite(_, Some(vtxparent)) -> backtrack (Alist.extend acc vtxparent) vtxparent
            end
      in

      let hp : vertex Heap.t = Heap.create ~cmp:(compare_vertex grph) () in

      let rec aux () : (vertex list) option =
        match Heap.pop hp with
        | None ->
            None

        | Some(vtxP) ->
            let () = print_for_debug ("see " ^ (Vertex.show vtxP)) in (* for debug *)
              if Vertex.equal vtxP vtxT then
                backtrack Alist.empty vtxT
              else
                let (dstblP, vherefP, vheP, lblrefP) =
                  match MainTable.find_opt grph vtxP with
                  | None ->
                    (* -- when 'vtxP' does not belong to 'grph'; it cannot happen -- *)
                      assert false

                  | Some((dstblP, vherefP, lblrefP)) ->
                      begin
                        match !vherefP with
                        | None       -> assert false
                        | Some(vheP) -> (dstblP, vherefP, vheP, lblrefP)
                      end
                in
                match !lblrefP with
                | Infinite ->
                  (* -- when 'Infinite' is the least element in 'hp', i.e. 'vtxT' is unreachable -- *)
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
                              | None ->
                                (* -- when 'vtx' is not a member of the heap 'hp';
                                      equivalent to 'Heap.mem ?equal:Vertex.equal hp vtx' -- *)
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
                if Vertex.equal vtx vtxS then () else
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
