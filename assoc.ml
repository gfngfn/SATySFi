
type ('a, 'b) t = ('a * 'b) list


let empty = []


let rec add ?eq:(eq = (=)) asc key value =
  match asc with
  | []                            -> (key, value) :: []
  | (k, v) :: tail  when eq k key -> (key, value) :: tail
  | (k, v) :: tail                -> (k, v) :: (add ~eq:eq tail key value)


let rec find ?eq:(eq = (=)) asc key =
  match asc with
  | []             -> raise Not_found
  | (k, v) :: tail -> if eq k key then v else find tail key


let to_list asc = asc


let of_list ?eq:(eq = (=)) lst =
  List.fold_right (fun (k, v) a -> add ~eq:eq a k v) empty lst


let rec map_value f asc =
  List.map (fun (k, v) -> (k, f v)) asc


let fold_value f init asc =
  List.fold_left (fun x (_, v) -> f x v) init asc


let to_value_list asc = List.map (fun (k, v) -> v) asc


let rec fold f init asc =
  List.fold_left f init asc
