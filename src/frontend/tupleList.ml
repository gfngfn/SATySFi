
type 'a t = 'a * 'a * 'a list


let make x1 x2 xs =
  (x1, x2, xs)


let map f (x1, x2, xs) =
  let y1 = f x1 in
  let y2 = f x2 in
  (y1, y2, xs |> List.map f)


let mapM f (x1, x2, xs) =
  let open ResultMonad in
  f x1 >>= fun y1 ->
  f x2 >>= fun y2 ->
  mapM f xs >>= fun ys ->
  return (y1, y2, ys)


let to_list (x1, x2, xs) =
  x1 :: x2 :: xs


let pp (type a) (ppa : Format.formatter -> a -> unit) (ppf : Format.formatter) ((x1, x2, xs) : a t) =
  Format.fprintf ppf "%a@ %a@ %a"
    ppa x1
    ppa x2
    (Format.pp_print_list ppa) xs
