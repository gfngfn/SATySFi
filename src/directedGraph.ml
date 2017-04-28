
module type VertexType =
  sig
    type t
    val compare : t -> t -> int
  end


module type S =
  sig
    type state = Remained | Touched | Done
    type vertex
    type 'a t
    exception UndefinedSourceVertex
    exception UndefinedDestinationVertex
    val create : int -> 'a t
    val add_vertex : 'a t -> vertex -> 'a -> unit
    val find_vertex : 'a t -> vertex -> 'a
    val add_edge : 'a t -> vertex -> vertex -> unit
    val find_cycle : 'a t -> (vertex list) option
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

    type 'a t = (vertex, 'a * state ref * (DestSet.t) ref) Hashtbl.t


    exception Cyclic
    exception Loop of vertex
    exception UndefinedSourceVertex
    exception UndefinedDestinationVertex


    let create initsize = Hashtbl.create initsize


    let add_vertex (dg : 'a t) (vtx : vertex) (label : 'a) =
      if Hashtbl.mem dg vtx then () else
        Hashtbl.add dg vtx (label, ref Remained, ref DestSet.empty)


    let find_vertex (dg : 'a t) (vtx : vertex) =
      try
        let (label, _, _) = Hashtbl.find dg vtx in
          label
      with
      | Not_found -> raise UndefinedSourceVertex


    let add_edge (dg : 'a t) (vtx1 : vertex) (vtx2 : vertex) =
      if not (Hashtbl.mem dg vtx2) then
        raise UndefinedDestinationVertex
      else
        let (_, _, destsetref) =
          try Hashtbl.find dg vtx1 with
          | Not_found -> raise UndefinedSourceVertex
        in
          if DestSet.mem vtx2 (!destsetref) then () else
            destsetref := DestSet.add vtx2 (!destsetref)


    let find_cycle (dg : 'a t) =
      let rec aux vtx1 =
        try
          let (_, sttref, destsetref) = Hashtbl.find dg vtx1 in
            match !sttref with
            | Done     -> ()
            | Touched  -> raise Cyclic
            | Remained ->
                begin
                  sttref := Touched ;
                  DestSet.iter aux (!destsetref) ;
                  sttref := Done ;
                end
        with
        | Not_found -> assert false
      in
        try
          begin
            Hashtbl.iter (fun vtx1 (_, sttref, destsetref) ->
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
            ) dg ;
            None
          end
        with
        | Loop(vtx) -> Some([vtx])
        | Cyclic ->
            let cycle =
              Hashtbl.fold (fun vtx1 (_, sttref, _) lst ->
                match !sttref with
                | Touched -> vtx1 :: lst
                | _       -> lst
              ) dg []
            in
              Some(cycle)

  end
