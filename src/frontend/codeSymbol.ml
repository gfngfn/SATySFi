
type t = {
  id : EvalVarID.t;
}


let fresh (info : Range.t * string) : t =
  let evid = EvalVarID.fresh info in
(*
  let () =
    let (_, s) = info in
    Format.printf "CodeSymbol> add %s: %a\n" s EvalVarID.pp evid (* for debug *)
  in
*)
  { id = evid; }


let unlift (symb : t) : EvalVarID.t =
  symb.id


let pp fmt symb =
  Format.fprintf fmt "Symbol(id = %a)" EvalVarID.pp symb.id
