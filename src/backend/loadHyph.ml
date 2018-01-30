
open MyUtil


type dir_path = string
type file_path = string

exception InvalidYOJSON               of file_path * string
exception OtherThanDictionary         of file_path
exception NotProvidingExceptionList   of file_path
exception ExceptionListOtherThanArray of file_path
exception InvalidExceptionElement     of file_path
exception NotProvidingPatternList     of file_path
exception PatternListOtherThanArray   of file_path
exception InvalidPatternElement       of file_path


module ExceptionMap = Map.Make(String)

type exception_map = (string list) ExceptionMap.t

type number = int

type beginning =
  | TopOfWord
  | ArbitraryBeginning

type final =
  | EndOfWord
  | ArbitraryFinal

type pattern = beginning * (Uchar.t * number) list * final

type t = exception_map * pattern list


let read_exception_list (srcpath : file_path) (jsonarr : Yojson.Safe.json) : exception_map =
  match jsonarr with
  | `List(jsonlst) ->
      jsonlst |> List.fold_left (fun mapacc json ->
        match json with
        | `Tuple[`String(wordfrom); `List(jsonlstto)] ->
            let fraclstto =
              jsonlstto |> List.map (function
                | `String(fracto) -> fracto
                | _               -> raise (InvalidExceptionElement(srcpath))
              )
            in
            mapacc |> ExceptionMap.add wordfrom fraclstto

        | _ ->
            raise (InvalidExceptionElement(srcpath))
      ) ExceptionMap.empty

  | _ ->
      raise (ExceptionListOtherThanArray(srcpath))


let numeric (uch : Uchar.t) : number option =
  let cp = Uchar.to_int uch in
  let cp0 = Char.code '0' in
  let cp9 = Char.code '9' in
    if cp0 <= cp && cp <= cp9 then
      Some(cp - cp0)
    else
      None


let convert_pattern (srcpath : file_path) (strpat : string) : pattern =
  let uchlstraw = InternalText.to_uchar_list (InternalText.of_utf8 strpat) in
  let (beginning, uchlstsub) =
      match uchlstraw with
      | [] ->
          raise (InvalidPatternElement(srcpath))

      | uch0 :: uchtail ->
          if uch0 = Uchar.of_char '.' then
            (TopOfWord, uchtail)
          else
            (ArbitraryBeginning, uchlstraw)
  in
  let (final, uchlst) =
    match List.rev uchlstsub with
    | [] ->
        raise (InvalidPatternElement(srcpath))

    | uchL :: uchrest ->
        if uchL = Uchar.of_char '.' then
          (EndOfWord, List.rev uchrest)
        else
          (ArbitraryFinal, uchlstsub)
  in
  let pairlst =
    uchlst |> list_fold_adjacent (fun acc uch _ optnext ->
      match numeric uch with
      | Some(_) ->
          acc

      | None ->
          begin
            match optnext with
            | None ->
                acc

            | Some(uchnext) ->
                let pair =
                  match numeric uchnext with
                  | None      -> (uch, 0)
                  | Some(num) -> (uch, num)
                in
                  Alist.extend acc pair
          end
    ) Alist.empty |> Alist.to_list
  in
    (beginning, pairlst, final)


let read_pattern_list (srcpath : file_path) (jsonarr : Yojson.Safe.json) : pattern list =
  match jsonarr with
  | `List(jsonlst) ->
      jsonlst |> List.map (function
        | `String(strpat) -> convert_pattern srcpath strpat
        | _               -> raise (InvalidPatternElement(srcpath))
      )

  | _ ->
      raise (PatternListOtherThanArray(srcpath))


let read_assoc (srcpath : file_path) (assoc : (string * Yojson.Safe.json) list) : t =
  let excpmap =
    match assoc |> List.assoc_opt "exceptions" with
    | None          -> raise (NotProvidingExceptionList(srcpath))
    | Some(jsonarr) -> read_exception_list srcpath jsonarr
  in
  let hyphpatlst =
    match assoc |> List.assoc_opt "patterns" with
    | None          -> raise (NotProvidingPatternList(srcpath))
    | Some(jsonarr) -> read_pattern_list srcpath jsonarr
  in
    (excpmap, hyphpatlst)


let main (satysfi_root_dir : dir_path) (filename : file_path) : t =
  let srcpath = Filename.concat satysfi_root_dir (Filename.concat "dist/hyph" filename) in
    try
      let json = Yojson.Safe.from_file srcpath in
          (* -- may raise 'Sys_error', or 'Yojson.Json_error' -- *)
        match json with
        | `Assoc(assoc) -> read_assoc srcpath assoc
        | json_other    -> raise (OtherThanDictionary(srcpath))
    with
    | Yojson.Json_error(msg) -> raise (InvalidYOJSON(srcpath, msg))


let empty = (ExceptionMap.empty, [])


let match_prefix (pairlst : (Uchar.t * number) list) (clst : (Uchar.t * number ref) list) : unit =
  let rec aux acc pairlst clst =
  match (pairlst, clst) with
  | (_ :: _, []) ->
      ()

  | ([], _) ->
      acc |> Alist.to_list |> List.iter (fun (numref, num) -> numref := max (!numref) num)

  | ((uchp, num) :: pairtail, (uchw, numref) :: ctail) ->
      if Uchar.equal uchp uchw then
        aux (Alist.extend acc (numref, num)) pairtail ctail
      else
        ()
  in
    aux Alist.empty pairlst clst


let rec match_every pairlst clst =
  match clst with
  | [] ->
      ()

  | _ :: ctail ->
      match_prefix pairlst clst;
      match_every pairlst ctail


let make_fraction fracacc =
  fracacc |> Alist.to_list


(* --
   'lookup_patterns':
     determines hyphen pattern of the given word.
     this implemenmtation is currently very inefficient. -- *)
let lookup_patterns (patlst : pattern list) (uchlst : Uchar.t list) : (Uchar.t list) list =
  let clst = uchlst |> List.map (fun uch -> (uch, ref 0)) in
  let () =
    patlst |> List.iter (fun (beginning, pairlst, final) ->
      match beginning with
      | TopOfWord          -> match_prefix pairlst clst
      | ArbitraryBeginning -> match_every pairlst clst
    )
  in
  let (acc, fracacc) =
    clst |> List.fold_left (fun (acc, fracacc) (uch, numref) ->
      if (!numref) mod 2 = 1 then
        let sfrac = make_fraction (Alist.extend fracacc uch) in
          (Alist.extend acc sfrac, Alist.empty)
      else
        (acc, Alist.extend fracacc uch)
    ) (Alist.empty, Alist.empty)
  in
    Alist.extend acc (make_fraction fracacc) |> Alist.to_list


let lookup ((excpmap, patlst) : t) (uchlst : Uchar.t list) : (Uchar.t list) list =
  match excpmap |> ExceptionMap.find_opt (InternalText.to_utf8 (InternalText.of_uchar_list uchlst)) with
  | Some(sfraclst) -> sfraclst |> List.map (fun sfrac -> InternalText.to_uchar_list (InternalText.of_utf8 sfrac))
  | None           -> lookup_patterns patlst uchlst
