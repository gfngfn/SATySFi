type 'a t = 'a list

let empty = []

let extend eacc x = x :: eacc

let append eacc lst = List.rev_append lst eacc

let to_list eacc = List.rev eacc

let to_list_rev eacc = eacc

let of_list lst = List.rev lst

let chop_last eacc =
  match eacc with
  | [] -> None
  | last :: rest -> Some (rest, last)

(* Modified map function to work directly with the reversed accumulator *)
let map f eacc =
  let rec map_aux acc = function
    | [] -> acc
    | x :: xs -> map_aux (f x :: acc) xs
  in map_aux [] eacc

(* Modified fold_left function to work directly with the reversed accumulator *)
let fold_left f init eacc =
  let rec fold_aux acc = function
    | [] -> acc
    | x :: xs -> fold_aux (f acc x) xs
  in fold_aux init eacc

let fold_right f eacc init =
  List.fold_right f (to_list eacc) init

let cat eacc1 eacc2 =
  List.append eacc2 eacc1
