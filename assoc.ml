
type ('a, 'b) t = ('a * 'b) list


let empty = []


let rec add asc key value =
  match asc with
  | []                           -> (key, value) :: []
  | (k, v) :: tail  when k = key -> (key, value) :: tail
  | (k, v) :: tail               -> (k, v) :: (add tail key value)


let rec find asc key =
  match asc with
  | []                           -> raise Not_found
  | (k, v) :: tail  when k = key -> v
  | (k, v) :: tail               -> find tail key


let rec map_value f asc =
  List.map (fun (k, v) -> (k, f v)) asc


let rec fold_value f =
  List.fold_left (fun x (_, v) -> f x v)


let to_value_list asc = List.map (fun (k, v) -> v) asc
