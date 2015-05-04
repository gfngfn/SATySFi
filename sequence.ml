open Types

  type 'a t = 'a list

  let empty = []

  (* 'a t -> 'a -> 'a t *)
  let rec append_element seq elem =
    elem :: seq

  (* 'a t ref -> 'a -> unit *)
  let append refseq elem =
    refseq := append_element !refseq elem

  (* 'a t -> 'a t *)
  let rec omit_last_element lst = 
    match lst with
      [] -> []
    | head :: tail -> tail

  (* 'a t ref -> unit *)
  let omit_last refseq =
    refseq := omit_last_element !refseq

  (* 'a t -> 'a *)
  let rec get_last_element seq =
    match seq with
      [] -> raise SequenceUnderflow
    | head :: tail -> head

  (* 'a t -> 'a *)
  let get_last refseq =
    get_last_element !refseq

  let rec append_list lst elem =
    match lst with
      [] -> [elem]
    | head :: tail -> head :: (append_list tail elem)

  let rec reverse_list lst =
    match lst with
      [] -> []
    | head :: tail -> append_list (reverse_list tail) head

  (* 'a list -> 'a t *)
  let of_list lst = reverse_list lst

  (* 'a t -> 'a list *)
  let to_list seq = reverse_list seq
