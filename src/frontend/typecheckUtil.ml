
open StaticEnv
open TypeError


type 'a ok = ('a, type_error) result

module SubstMap = Map.Make(TypeID)

type substitution = type_scheme SubstMap.t
