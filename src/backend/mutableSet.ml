module Make (SetScheme : Hashtbl.HashedType) =
  struct

    module InternalHashTable = Hashtbl.Make(SetScheme)

    type elem = SetScheme.t

    type t = unit InternalHashTable.t

    let create = InternalHashTable.create

    let clear = InternalHashTable.clear

    let fold f ms init = InternalHashTable.fold (fun x () acc -> f x acc) ms init

    let mem ms x = InternalHashTable.mem ms x

    let add ms x = InternalHashTable.add ms x ()

  end
