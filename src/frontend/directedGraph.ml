
let print_for_debug_digraph msg =
(*
  print_endline msg;
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
    val backward_bfs_fold : ('b -> vertex -> 'a -> 'b) -> 'b -> 'a t -> 'b
    val backward_bfs : (vertex -> 'a -> unit) -> 'a t -> unit
    val get_vertex : 'a t -> vertex -> 'a
  end


module Make (Vertex : VertexType) =
  struct

    module VertexSet = Set.Make(Vertex)

    module VertexHashTable = Hashtbl.Make
      (struct
        type t = Vertex.t
        let equal v1 v2 = (Vertex.compare v1 v2 = 0)
        let hash = Hashtbl.hash
      end)

    type state =
      | Remained
      | Touched
      | Done

    type vertex = Vertex.t

    type degree_out = int

    type 'a entry = {
      label        : 'a;
      degree_out   : int ref;
      state        : state ref;
      sources      : VertexSet.t ref;
      destinations : VertexSet.t ref;
    }

    type 'a t = ('a entry) VertexHashTable.t

    exception Cyclic
    exception Loop of vertex
    exception UndefinedSourceVertex
    exception UndefinedDestinationVertex


    let create initsize = VertexHashTable.create initsize


    let add_vertex (dg : 'a t) (vtx : vertex) (label : 'a) =
      print_for_debug_digraph ("addV (" ^ (Vertex.show vtx) ^ ")");
      if VertexHashTable.mem dg vtx then () else
        VertexHashTable.add dg vtx {
          label        = label;
          degree_out   = ref 0;
          state        = ref Remained;
          sources      = ref VertexSet.empty;
          destinations = ref VertexSet.empty;
        }


    let find_vertex (dg : 'a t) (vtx : vertex) =
      match VertexHashTable.find_opt dg vtx with
      | Some(entry) -> entry.label
      | None        -> raise UndefinedSourceVertex


    let iter_vertex (f : vertex -> 'a -> unit) (dg : 'a t) =
      dg |> VertexHashTable.iter (fun vtx entry -> f vtx entry.label)


    let fold_vertex (f : vertex -> 'a -> 'b -> 'b) (dg : 'a t) (init : 'b) =
      VertexHashTable.fold (fun vtx entry acc -> f vtx entry.label acc) dg init


    let mem_vertex (vtx : vertex) (dg : 'a t) =
      VertexHashTable.mem dg vtx


    let add_edge (dg : 'a t) (vtx1 : vertex) (vtx2 : vertex) =
      print_for_debug_digraph ("addE (" ^ (Vertex.show vtx1) ^ " ---> " ^ (Vertex.show vtx2) ^ ")");
      let sources2 =
        match VertexHashTable.find_opt dg vtx2 with
        | Some(entry2) -> entry2.sources
        | None         -> raise UndefinedDestinationVertex
      in
      let (deg1, dests1) =
        match VertexHashTable.find_opt dg vtx1 with
        | Some(entry1) -> (entry1.degree_out, entry1.destinations)
        | None         -> raise UndefinedSourceVertex
      in
        if VertexSet.mem vtx2 !dests1 then () else begin
          incr deg1;
          dests1 := !dests1 |> VertexSet.add vtx2;
          sources2 := !sources2 |> VertexSet.add vtx1;
        end


    let initialize_state (dg : 'a t) : unit =
      dg |> VertexHashTable.iter (fun vtx entry -> entry.state := Remained)


    let get_vertex_data (dg : 'a t) (vtx : vertex) : 'a entry =
      match VertexHashTable.find_opt dg vtx with
      | Some(entry) -> entry
      | None        -> assert false


    let get_vertex (dg : 'a t) (vtx : vertex) : 'a =
      let entry = get_vertex_data dg vtx in entry.label


    let find_cycle (dg : 'a t) =
      let rec aux vtx1 =
        let entry = get_vertex_data dg vtx1 in
        let state = entry.state in
        match !state with
        | Done ->
            ()

        | Touched ->
            raise Cyclic

        | Remained ->
            state := Touched;
            !(entry.destinations) |> VertexSet.iter aux;
            state := Done
      in
        try
          begin
            initialize_state dg;
            dg |> VertexHashTable.iter (fun vtx1 entry ->
              let state = entry.state in
              let dests = entry.destinations in
              begin
                begin
                  if VertexSet.mem vtx1 (!dests) then
                    raise (Loop(vtx1))
                  else
                    ()
                end;
                match !state with
                | Remained -> aux vtx1
                | _        -> ()
              end
            );
            None
          end
        with
        | Loop(vtx) ->
            Some([vtx])

        | Cyclic ->
            let cycle =
              VertexHashTable.fold (fun vtx1 entry lst ->
                match !(entry.state) with
                | Touched -> vtx1 :: lst
                | _       -> lst
              ) dg []
            in
              Some(cycle)


    let backward_bfs_fold (f : 'b -> vertex -> 'a -> 'b) (init : 'b) (dg : 'a t) : 'b =
      let vq = Queue.create () in
      let rec step init =
        try
          let vtx = Queue.pop vq in
          let () = print_for_debug_digraph ("pop " ^ (Vertex.show vtx)) in (* for debug *)
          let entry = get_vertex_data dg vtx in
          let label = entry.label in
          let state = entry.state in
          let sources = entry.sources in
          let initnew = f init vtx label in
          state := Done;
          !sources |> VertexSet.iter (fun vtx1 ->
            let () = print_for_debug_digraph ("see " ^ (Vertex.show vtx1)) in (* for debug *)
            let entry1 = get_vertex_data dg vtx1 in
            let deg1 = entry1.degree_out in
            let state1 = entry1.state in
            decr deg1;
            match !state1 with
            | Done ->
                print_for_debug_digraph "(done)";  (* for debug *)
                ()

            | Touched ->
                print_for_debug_digraph "(touched)";  (* for debug *)
                ()

            | Remained ->
                if !deg1 = 0 then begin
                  print_for_debug_digraph ("push " ^ (Vertex.show vtx1));  (* for debug *)
                  state1 := Touched;
                  Queue.push vtx1 vq;
                end else
                  ()
          );
          step initnew;
        with
        | Queue.Empty -> init
      in
      initialize_state dg;
      dg |> VertexHashTable.iter (fun vtx entry ->
        let degref = entry.degree_out in
        let sttref = entry.state in
        if !degref = 0 then begin
          print_for_debug_digraph ("push0 " ^ (Vertex.show vtx));  (* for debug *)
          sttref := Touched;
          Queue.push vtx vq;
        end else ()
      );
      step init


    let backward_bfs (f : vertex -> 'a -> unit) (dg : 'a t) : unit =
      backward_bfs_fold (fun () -> f) () dg

end
