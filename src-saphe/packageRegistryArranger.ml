
open PackageSystemBase


module CanonicalRegistryRemoteSet = Set.Make(struct
  type t = registry_remote

  let compare (r1 : t) (r2 : t) =
    let GitRegistry{ url = canonical_url1; branch = branch1 } = r1 in
    let GitRegistry{ url = canonical_url2; branch = branch2 } = r2 in
    List.compare String.compare [ canonical_url1; branch1 ] [ canonical_url2; branch2 ]
end)


let canonicalize_registry_remote (ferr : CanonicalRegistryUrl.error -> 'e) (GitRegistry{ url; branch } : registry_remote) : (registry_remote, 'e) result =
  let open ResultMonad in
  let* canonical_url = CanonicalRegistryUrl.make url |> Result.map_error ferr in
  return @@ GitRegistry{ url = canonical_url; branch }


let rec aux (ferr : CanonicalRegistryUrl.error -> 'e) (f : 'a -> registry_remote -> (registry_remote list * 'a, 'e) result) (already_seen : CanonicalRegistryRemoteSet.t) (stack : registry_remote list) (acc : 'a) : ('a, 'e) result =
  let open ResultMonad in
  match stack with
  | [] ->
      return acc

  | r :: stack ->
      let* canonical_registry_remote = canonicalize_registry_remote ferr r in
      if already_seen |> CanonicalRegistryRemoteSet.mem canonical_registry_remote then
        aux ferr f already_seen stack acc
      else
        let* (rs, acc) = f acc canonical_registry_remote in
        let already_seen = already_seen |> CanonicalRegistryRemoteSet.add canonical_registry_remote in
        let stack = List.append rs stack in
        aux ferr f already_seen stack acc


let main ~err:(ferr : CanonicalRegistryUrl.error -> 'e) (f : 'a -> registry_remote -> (registry_remote list * 'a, 'e) result) (init_stack : registry_remote list) (init_acc : 'a) : ('a, 'e) result =
  aux ferr f CanonicalRegistryRemoteSet.empty init_stack init_acc
