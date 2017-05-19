
module Make (Key : Map.OrderedType) =
  struct

    module InternalMap = Map.Make(Key)

    type key = Key.t

    type 'a t = Node of 'a * ('a t) InternalMap.t


    let rec get (Node(x, imap) : 'a t) (addr : key list) =
      match addr with
      | []        -> x
      | k :: tail -> let hshtr = InternalMap.find k imap in get hshtr tail

  end
