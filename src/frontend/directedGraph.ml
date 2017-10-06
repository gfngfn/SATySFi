
let print_for_debug_digraph msg =
(*
  print_endline msg ;
*)
  ()


module type VertexType =
  sig
    type t
    val compare : t -> t -> int
    val show : t -> string (* for debug *)
  end


module type S =
  sig
    type vertex
    type 'a t
    exception UndefinedSourceVertex
    exception UndefinedDestinationVertex
    val create : int -> 'a t
    val add_vertex : 'a t -> vertex -> 'a -> unit
    val find_vertex : 'a t -> vertex -> 'a
    val iter_vertex : (vertex -> 'a -> unit) -> 'a t -> unit
    val fold_vertex : (vertex -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val mem_vertex : vertex -> 'a t -> bool
    val add_edge : 'a t -> vertex -> vertex -> unit
    val find_cycle : 'a t -> (vertex list) option
    val backward_bfs : (vertex -> 'a -> unit) -> 'a t -> unit
    val get_vertex : 'a t -> vertex -> 'a
  end


module Make (Vertex : VertexType) =
  struct

    module VertexSet = Set.Make(Vertex)

    type state = Remained | Touched | Done

    type vertex = Vertex.t

    type degree_out = int

    type 'a t = (vertex, 'a * degree_out ref * state ref * VertexSet.t ref * VertexSet.t ref) Hashtbl.t

    exception Cyclic
    exception Loop of vertex
    exception UndefinedSourceVertex
    exception UndefinedDestinationVertex


    let create initsize = Hashtbl.create initsize


    let add_vertex (dg : 'a t) (vtx : vertex) (label : 'a) =
      if Hashtbl.mem dg vtx then () else
        Hashtbl.add dg vtx (label, ref 0, ref Remained, ref VertexSet.empty, ref VertexSet.empty)


    let find_vertex (dg : 'a t) (vtx : vertex) =
      try
        let (label, _, _, _, _) = Hashtbl.find dg vtx in
          label
      with
      | Not_found -> raise UndefinedSourceVertex


    let iter_vertex (f : vertex -> 'a -> unit) (dg : 'a t) =
      dg |> Hashtbl.iter (fun vtx (label, _, _, _, _) -> f vtx label)


    let fold_vertex (f : vertex -> 'a -> 'b -> 'b) (dg : 'a t) (init : 'b) =
      Hashtbl.fold (fun vtx (label, _, _, _, _) acc -> f vtx label acc) dg init


    let mem_vertex (vtx : vertex) (dg : 'a t) =
      Hashtbl.mem dg vtx


    let add_edge (dg : 'a t) (vtx1 : vertex) (vtx2 : vertex) =
      let (_, _, _, srcsetref2, _) =
        try Hashtbl.find dg vtx2 with
        | Not_found -> raise UndefinedDestinationVertex
      in
      let (_, degref1, _, _, destsetref1) =
        try Hashtbl.find dg vtx1 with
        | Not_found -> raise UndefinedSourceVertex
      in
        if VertexSet.mem vtx2 (!destsetref1) then () else
          begin
            incr degref1 ;
            destsetref1 := VertexSet.add vtx2 (!destsetref1) ;
            srcsetref2 := VertexSet.add vtx1 (!srcsetref2) ;
          end


    let initialize_state (dg : 'a t) =
      dg |> Hashtbl.iter (fun vtx (_, _, sttref, _, _) -> sttref := Remained)


    let get_vertex_data (dg : 'a t) (vtx : vertex) =
      try Hashtbl.find dg vtx with
      |  Not_found -> assert false


    let get_vertex (dg : 'a t) (vtx : vertex) =
      let (res, _, _, _, _) = get_vertex_data dg vtx in res


    let find_cycle (dg : 'a t) =
      let rec aux vtx1 =
          let (_, _, sttref, _, destsetref) = get_vertex_data dg vtx1 in
            match !sttref with
            | Done     -> ()
            | Touched  -> raise Cyclic
            | Remained ->
                begin
                  sttref := Touched ;
                  VertexSet.iter aux (!destsetref) ;
                  sttref := Done ;
                end
      in
        try
          begin
            initialize_state dg ;
            dg |> Hashtbl.iter (fun vtx1 (_, _, sttref, _, destsetref) ->
              begin
                begin
                  if VertexSet.mem vtx1 (!destsetref) then
                    raise (Loop(vtx1))
                  else
                    ()
                end ;
                match !sttref with
                | Remained -> aux vtx1
                | _        -> ()
              end
            ) ;
            None
          end
        with
        | Loop(vtx) -> Some([vtx])
        | Cyclic ->
            let cycle =
              Hashtbl.fold (fun vtx1 (_, _, sttref, _, _) lst ->
                match !sttref with
                | Touched -> vtx1 :: lst
                | _       -> lst
              ) dg []
            in
              Some(cycle)


    let backward_bfs (f : vertex -> 'a -> unit) (dg : 'a t) =
        let vq = Queue.create () in
        let rec step () =
          try
            let vtx = Queue.pop vq in
              let () = print_for_debug_digraph ("pop " ^ (Vertex.show vtx)) in (* for debug *)
            let (label, _, sttref, srcsetref, _) = get_vertex_data dg vtx in
            begin
              f vtx label ;
              sttref := Done ;
              (!srcsetref) |> VertexSet.iter (fun vtx1 ->
                let () = print_for_debug_digraph ("see " ^ (Vertex.show vtx1)) in (* for debug *)
                let (_, _, sttref1, _, _) = get_vertex_data dg vtx1 in
                  match !sttref1 with
                  | Done     -> ()
                  | Touched  -> assert false
                  | Remained ->
                      let () = print_for_debug_digraph ("push " ^ (Vertex.show vtx1)) in (* for debug *)
                        Queue.push vtx1 vq
              ) ;
              step () ;
            end
          with
          | Queue.Empty -> ()
        in
        begin
          initialize_state dg ;
          dg |> Hashtbl.iter (fun vtx (_, degref, _, _, _) ->
            if !degref = 0 then
              let () = print_for_debug_digraph ("push0 " ^ (Vertex.show vtx)) in (* for debug *)
              Queue.push vtx vq
            else ()
          ) ;
          step () ;
        end

end
