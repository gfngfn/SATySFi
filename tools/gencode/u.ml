
let default v = function
  | Some(x) -> x
  | None    -> v


let trim_re =
  let open Re in
  let sp = alt [ char ' '; char '\t' ] in
  let nl = alt [ str "\r\n"; char '\r'; char '\n' ] in
  (* -- /\A *\n(.*\n) *\z/ -- *)
  seq [
    bos;
    rep sp;
    nl;
    group @@ seq [ rep any; nl ];
    rep sp;
    eos;
  ] |> compile


let trim s =
  match Re.exec_opt trim_re s with
  | Some(gr) -> Re.Group.get gr 1
  | None     -> s


let opt_map f = function
  | Some(v) -> Some(f v)
  | None    -> None


let split_lines s =
  Core.String.split_lines s


let nullp = function
  | [] -> true
  | _  -> false


let const v = fun _ -> v


let puts fmt =
  Printf.printf (fmt ^^ "\n")


let ( @% ) fmt =
  Printf.sprintf fmt
