
(* Same as `Semver.t`, but build metada should be empty. *)
type t = Semver.t


let equal =
  Semver.equal


let parse (s : string) : t option =
  let open OptionMonad in
  let* semver = Semver.of_string s in
  let Semver.{ build; _ } = semver in
  match build with
  | []     -> return semver
  | _ :: _ -> None


let to_string =
  Semver.to_string


let pp ppf semver =
  Format.fprintf ppf "%s" (to_string semver)


let show =
  to_string


let compare =
  Semver.compare


let is_compatible ~(old : t) ~(new_ : t) =
  let open Semver in
  let { prerelease = prerelease_old; _ } = old in
  let { prerelease = prerelease_new; _ } = new_ in
  match (prerelease_old, prerelease_new) with
  | ([], []) ->
      begin
        match (old.major, new_.major) with
        | (0, 0) ->
            old.minor = new_.minor && old.patch <= new_.patch

        | _ ->
            old.major = new_.major &&
              ((old.minor < new_.minor) ||
                (old.minor == new_.minor && old.patch <= new_.patch))
      end

  | (_ :: _, _) | (_, _ :: _) ->
      Semver.equal old new_


type requirement =
  | CompatibleWith of t
[@@deriving show { with_path = false }]


let parse_requirement (s : string) : requirement option =
  match (String.get s 0, parse (String.sub s 1 (String.length s - 1))) with
  | exception Invalid_argument(_) ->
      None

  | ('^', Some(semver)) ->
      Some(CompatibleWith(semver))

  | _ ->
      None


let requirement_to_string (verreq : requirement) : string =
  match verreq with
  | CompatibleWith(semver) ->
      Printf.sprintf "^%s" (to_string semver)


let fulfill (req : requirement) (semver : t) : bool =
  match req with
  | CompatibleWith(semver_criterion) ->
      is_compatible ~old:semver_criterion ~new_:semver


let get_compatibility_unit (semver : t) : string =
  let Semver.{ major; minor; prerelease; _ } = semver in
  match prerelease with
  | [] ->
      if major = 0 then
        Printf.sprintf "0.%d" minor
      else
        Printf.sprintf "%d" major

  | _ :: _ ->
      to_string semver
