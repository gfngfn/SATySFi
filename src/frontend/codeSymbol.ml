
type t = {
  id : EvalVarID.t;
}


let fresh (info : Range.t * string) : t =
  let evid = EvalVarID.fresh info in
  { id = evid; }


let unlift (symb : t) : EvalVarID.t =
  symb.id


let pp fmt symb =
  Format.fprintf fmt "Symbol(id = %a)" EvalVarID.pp symb.id
