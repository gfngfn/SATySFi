
type error =
  | ContainsQueryParameter of { url : string }
  | NoUriScheme            of { url : string }
  | UnexpectedUrlScheme    of { url : string; scheme : string }


let make (s_registry_url : string) : (string, error) result =
  let open ResultMonad in
  let uri = Uri.of_string s_registry_url in

  (* Checks schemes: *)
  let* uri =
    match Uri.scheme uri with
    | None ->
        err @@ NoUriScheme{ url = s_registry_url }

    | Some(scheme) ->
        let scheme = String.lowercase_ascii scheme in
        begin
          match scheme with
          | "https" | "http" -> return (Uri.with_scheme uri (Some(scheme)))
          | _                -> err @@ UnexpectedUrlScheme{ url = s_registry_url; scheme }
        end
  in

  (* Canonicalizes paths: *)
  let uri =
    let path = Uri.path uri in
    let path = String.lowercase_ascii path in
    Uri.with_path uri path
  in

  (* Checks query parameters: *)
  let* () =
    match Uri.query uri with
    | []     -> return ()
    | _ :: _ -> err @@ ContainsQueryParameter{ url = s_registry_url }
  in

  return (Uri.to_string uri)
