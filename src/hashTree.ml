
module Make (Key : Map.OrderedType) =
  struct

    module InternalMap = Map.Make(Key)

    type key = Key.t

    type 'a t = Node of 'a * ('a t) InternalMap.t


    let rec get (Node(x, imap) : 'a t) (addr : key list) =
      match addr with
      | []        -> x
      | k :: tail ->
          let hshtr = InternalMap.find k imap in get hshtr tail


    let rec update (Node(x, imap) : 'a t) (addr : key list) (f : 'a -> 'a) =
      match addr with
      | []        -> Node(f x, imap)
      | k :: tail ->
          let hshtr = InternalMap.find k imap in
            Node(x, InternalMap.add k (update hshtr tail f) imap)


    let rec search_backward (Node(x, imap) : 'a t) (addr : key list) (findf : 'a -> 'b option) =
      match addr with
      | []        -> findf x
      | k :: tail ->
          let hshtr = InternalMap.find k imap in
          let res = search_backward hshtr tail findf in
          begin
            match res with
            | None    -> findf x
            | Some(_) -> res
          end

  end
