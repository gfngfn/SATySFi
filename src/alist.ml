
type 'a t = 'a list


let empty = []

let extend eacc x = x :: eacc

let append eacc lst = List.rev_append lst eacc

let to_list eacc = List.rev eacc

let of_list eacc = List.rev eacc

let chop_last eacc =
  match eacc with
  | []           -> None
  | last :: rest -> Some((rest, last))

let map f eacc =
  eacc |> to_list |> List.map f |> of_list

let fold_left f init eacc =
  eacc |> to_list |> List.fold_left f init

let fold_right f eacc init =
  List.fold_right f (eacc |> to_list) init
