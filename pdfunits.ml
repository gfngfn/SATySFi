open Pdfutil

(* Units. To add a new unit, extend here and in the graph following. *)
type t = PdfPoint | Inch | Centimetre | Millimetre | Pixel

(* Building convertors *)

(* Conversions. Must form a connected graph. Each unit is listed at most once as
the first of each pair, and at mose once in each associated list. *)
  
(* Create the symmetric closure of the conversions graph, allowing any
conversion to be achieved by the following of the appropriate arcs. *)
let conversions dpi =
  let conversions =
    [Millimetre, [Centimetre, 10.]; (* 10mm = 1cm. *)
     PdfPoint, [Inch, 72.]; (* 72pt = 1in. *)
     Centimetre, [Inch, 2.54]; (* 2.54cm = 1in. *)
     Pixel, [Inch, dpi]] (* dpi pixels = 1in. *)
  in
    let conversions' = ref conversions in
      let insert unit (unit', k) =
        conversions' := 
          match lookup unit !conversions' with
          | None -> add unit [unit', k] !conversions'
          | Some cs -> replace unit ((unit', k)::cs) !conversions'
      in
        (* For each item, insert reverse arcs for all in its conversion list. *)
        iter
          (fun (u, cs) ->
             iter (fun (u', k) -> insert u' (u, 1. /. k)) cs)
          conversions;
      !conversions'
  
(* To convert, we use a breadth-first search to find the shortest path in the
graph, thus minimising the number of conversions. This is not optimal from a
floating-point perspective (where certain conversions are worse than others) *)

(* Create an index relating types unit to index numbers beginning at 0. *)
let index conversions =
  combine (map fst conversions) (ilist 0 (length conversions - 1))

(* Make an array of lists representing the conversions graph, using the index
numbers. *)
let conv_array index conversions =
  let adjacency_lists =
    map
      (fun (u, l) ->
         lookup_failnull u index,
         map (fun (u, k) -> lookup_failnull u index, k) l)
      conversions
  in
    Array.of_list (map snd adjacency_lists)

(* Colours for breadth-first search *)
type colour = White | Grey | Black

(* Perform a breadth-first search starting at u, thus creating a predecessor
subgraph pred, which is returned. *)
let breadth_first index conv_array u =
  let size = Array.length conv_array in
    let pred = Array.make size (-1) (*r $-1$ = null in predecessor array *)
    in let colours = Array.make size White (*r Colour array. *)
    in let s = lookup_failnull u index in (*r Source. *)
      let q = Queue.create () in
        Queue.add s q; (*r Queue for set of grey vertices. *)
        while not (Queue.is_empty q) do
          let u = Queue.peek q in
            iter
              (fun (i, _) ->
                 if colours.(i) = White then
                   begin
                     colours.(i) <- Grey;
                     pred.(i) <- u;
                     Queue.add i q
                   end)
              conv_array.(u);
            ignore (Queue.take q);
            colours.(u) <- Black;
        done;
        pred

(* Converting *)
          
(* Given source and destination units, we return a conversion function. This
 follows the appropriate arcs, accumulating the total multiplier. Obviously, the
 user can provide a third argument to do the computation immediately. *)
let convert dpi u u' =
  let conversions = conversions dpi in
    let index = index conversions in
      let conv_array = conv_array index conversions in
        let pred = breadth_first index conv_array u' in
          let i = ref (lookup_failnull u index)
          in let m = ref 1. in
            while not (pred.(!i) = -1) do
              let i' = pred.(!i) in
                m *.= lookup_failnull !i conv_array.(i');
                i := i'
            done;
            fun x -> x *. !m

