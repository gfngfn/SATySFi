
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


let map_value f asc =
  List.map (fun (k, v) -> (k, f v)) asc


let iter_value f asc =
  List.iter (fun (_, v) -> f v) asc


let fold_value f init asc =
  List.fold_left (fun x (_, v) -> f x v) init asc


let to_value_list asc = List.map (fun (k, v) -> v) asc


let rec fold f init asc =
  List.fold_left f init asc


let rec mem ?eq:(eq = (=)) key asc =
  match asc with
  | []                         -> false
  | (k, _) :: _  when eq k key -> true
  | _ :: tail                  -> mem ~eq:eq key tail


let domain_included ?eq:(eq = (=)) asc1 asc2 =
  List.fold_left (fun b (k, _) -> b && (mem ~eq:eq k asc2)) true asc1


let domain_same ?eq:(eq = (=)) asc1 asc2 =
  (domain_included ~eq:eq asc1 asc2) && (domain_included ~eq:eq asc2 asc1)


let combine_value ?eq:(eq = (=)) asc1 asc2 =
  let rec aux asc1 acclst =
    match asc1 with
    | []             -> List.rev acclst
    | (k, v) :: tail -> aux tail ((v, find ~eq:eq asc2 k) :: acclst)
  in
    aux asc1 []

let intersection ?eq:(eq = (=)) asc1 asc2 =
  let rec aux asc1 acclst =
    match asc1 with
    | []             -> List.rev acclst
    | (k, v) :: tail -> if mem ~eq:eq k asc2 then aux tail ((v, find ~eq:eq asc2 k) :: acclst)
                                             else aux tail acclst
  in
    aux asc1 []

let union ?eq:(eq = (=)) asc1 asc2 =
  let rec aux asc1 accasc =
    match asc1 with
    | []             -> List.rev accasc
    | (k, v) :: tail -> if mem ~eq:eq k accasc then aux tail accasc else aux tail ((k, v) :: accasc)
  in
    aux asc1 asc2
