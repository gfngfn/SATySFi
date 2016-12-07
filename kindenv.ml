open Types


type t = (Tyvarid.t, type_struct) Assoc.t


let add kenv tvid tystr =
  Assoc.add kenv tvid tystr

