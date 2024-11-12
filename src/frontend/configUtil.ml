
open MyUtil
open CommonUtil
open EnvelopeSystemBase
open ConfigError

module ConfigDecoder = YamlDecoder.Make(YamlError)


let uppercased_identifier_decoder : string ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun s ->
  if is_uppercased_identifier s then
    succeed s
  else
    failure (fun context -> NotAnUppercasedIdentifier{ context; got = s })


let lowercased_identifier_decoder : string ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun s ->
  if is_lowercased_identifier s then
    succeed s
  else
    failure (fun context -> NotALowercasedIdentifier{ context; got = s })


let abs_path_decoder : abs_path ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun s ->
  match AbsPath.of_string s with
  | None          -> failure (fun context -> NotAnAbsolutePath{ context; got = s })
  | Some(abspath) -> succeed abspath


(* Note: the empty string is allowed *)
let relative_path_decoder : relative_path ConfigDecoder.t =
  let open ConfigDecoder in
  string >>= fun s ->
  if Filename.is_relative s then
    succeed s
  else
    failure (fun context -> NotARelativePath{ context; got = s })
