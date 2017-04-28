
module type VertexType =
  sig
    type t
    val compare : t -> t -> int
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
    val iter_vertex : (vertex -> unit) -> 'a t -> unit
    val mem_vertex : vertex -> 'a t -> bool
    val add_edge : 'a t -> vertex -> vertex -> unit
    val find_cycle : 'a t -> (vertex list) option
    val backward_bfs : (vertex -> 'a -> unit) -> 'a t -> unit
  end


module Make (Vertex : VertexType) =
  struct

    type state = Remained | Touched | Done

    type vertex = Vertex.t
    module DestSet = Set.Make(
      struct
        type t = vertex
        let compare = Vertex.compare
      end)

    type degree_out = int

    type 'a t = (vertex, 'a * degree_out ref * state ref * (DestSet.t) ref) Hashtbl.t


    exception Cyclic
    exception Loop of vertex
    exception UndefinedSourceVertex
    exception UndefinedDestinationVertex


    let create initsize = Hashtbl.create initsize


    let add_vertex (dg : 'a t) (vtx : vertex) (label : 'a) =
      if Hashtbl.mem dg vtx then () else
        Hashtbl.add dg vtx (label, ref 0, ref Remained, ref DestSet.empty)


    let find_vertex (dg : 'a t) (vtx : vertex) =
      try
        let (label, _, _, _) = Hashtbl.find dg vtx in
          label
      with
      | Not_found -> raise UndefinedSourceVertex


    let iter_vertex (f : vertex -> unit) (dg : 'a t) =
      dg |> Hashtbl.iter (fun vtx _ -> f vtx)


    let mem_vertex (vtx : vertex) (dg : 'a t) =
      Hashtbl.mem dg vtx


    let add_edge (dg : 'a t) (vtx1 : vertex) (vtx2 : vertex) =
      if not (Hashtbl.mem dg vtx2) then
        raise UndefinedDestinationVertex
      else
        let (_, degref1, _, destsetref1) =
          try Hashtbl.find dg vtx1 with
          | Not_found -> raise UndefinedSourceVertex
        in
          if DestSet.mem vtx2 (!destsetref1) then () else
            begin
              incr degref1 ;
              destsetref1 := DestSet.add vtx2 (!destsetref1) ;
            end


    let initialize_state (dg : 'a t) =
      dg |> Hashtbl.iter (fun vtx (_, _, sttref, _) -> sttref := Remained)


    let get_vertex_data (dg : 'a t) (vtx : vertex) =
      try Hashtbl.find dg vtx with
      |  Not_found -> assert false


    let find_cycle (dg : 'a t) =
      let rec aux vtx1 =
          let (_, _, sttref, destsetref) = get_vertex_data dg vtx1 in
            match !sttref with
            | Done     -> ()
            | Touched  -> raise Cyclic
            | Remained ->
                begin
                  sttref := Touched ;
                  DestSet.iter aux (!destsetref) ;
                  sttref := Done ;
                end
      in
        try
          begin
            initialize_state dg ;
            dg |> Hashtbl.iter (fun vtx1 (_, _, sttref, destsetref) ->
              begin
                begin
                  if DestSet.mem vtx1 (!destsetref) then
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
              Hashtbl.fold (fun vtx1 (_, _, sttref, _) lst ->
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
            let (label, _, sttref, destsetref) = get_vertex_data dg vtx in
            begin
              f vtx label ;
              sttref := Done ;
              (!destsetref) |> DestSet.iter (fun vtx2 ->
                let (_, _, sttref, _) = get_vertex_data dg vtx2 in
                  match !sttref with
                  | Remained -> Queue.push vtx2 vq
                  | Touched  -> assert false
                  | Done     -> ()
              ) ;
              step () ;
            end
          with
          | Queue.Empty -> ()
        in
        begin
          initialize_state dg ;
          dg |> Hashtbl.iter (fun vtx (_, degref, _, _) ->
            if !degref = 0 then Queue.push vtx vq else ()
          ) ;
          step () ;
        end

end
