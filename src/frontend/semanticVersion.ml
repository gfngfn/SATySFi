
type t = Semver.t


let parse (s : string) : t option =
  Semver.of_string s


let to_string (semver : t) : string =
  Semver.to_string semver


let pp ppf semver =
  Format.fprintf ppf "%s" (to_string semver)


let show =
  to_string


let compare =
  Semver.compare


let is_compatible ~(old : t) ~(new_ : t) =
  let open Semver in
  match (old.major, new_.major) with
  | (0, 0) ->
      old.minor = new_.minor && old.patch <= new_.patch

  | _ ->
      old.major = new_.major &&
        ((old.minor < new_.minor) ||
          (old.minor == new_.minor && old.patch <= new_.patch))
