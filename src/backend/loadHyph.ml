
open MyUtil
open CharBasis

module YS = Yojson.SafePos
module MYU = MyYojsonUtil

exception InvalidPatternElement of Range.t

type number = int

type hyph_rule =
  | Normal    of number list
  | Exception of string list

type pattern = (Uchar.t list) * hyph_rule

type match_result =
  | MatchNormal    of (number * number list) list
  | MatchException of string list


type answer =
  | Single    of uchar_segment list
  | Fractions of (uchar_segment list) list


module IntSet = Set.Make
  (struct
    type t = int
    let compare i j = i - j
  end)

module UcharMap = Map.Make
  (struct
    type t = Uchar.t
    let compare = Pervasives.compare
  end)

module IntMap = Map.Make
  (struct
    type t = int
    let compare = Pervasives.compare
  end)


module Trie
: sig

  type t

  val empty : t

  val make : pattern list -> t

  val match_every : t -> Uchar.t list -> match_result

  end
= struct

  (* implemented with double-array *)
  type node =
    {
      base  : int;
      check : int;
      rule  : hyph_rule option;
    }

  type t = node array * int (* minimum char code of pattern *)


  let empty = (Array.of_list [], 0)


  let make patlst =
    let mincode =
      patlst |> List.fold_left (fun acc (uchlst, _) ->
        uchlst |> List.fold_left Pervasives.min acc
      ) Uchar.max |> Uchar.to_int
    in
    let aux patlst =
      let map = List.fold_left (fun acc (ulst, rule) ->
        match ulst with
        | [] ->
            acc

        | hdch :: rest ->
            UcharMap.update hdch (fun opt ->
              match (opt, rest) with
              | (None, [])         -> Some([],   Some(rule))
              | (None, _)          -> Some([(rest, rule)], None)
              | (Some(ull, _), []) -> Some(ull,  Some(rule))
              | (Some(ull, r), _)  -> Some((rest, rule) :: ull, r)
            ) acc
      ) (UcharMap.empty) patlst
      in
      UcharMap.fold (fun k v acc -> (k, v) :: acc) map []
    in
    let rec search_base checkset indexlst offset =
      if List.exists (fun uc -> IntSet.mem ((Uchar.to_int uc) - mincode + offset) checkset) indexlst then
        search_base checkset indexlst (offset + 1)
      else
        offset
    in
    let rec iter stk basemap checkset checkmap rulemap =
      match stk with
      | [] ->
          let kmax = IntMap.fold (fun k _ acc ->
            Pervasives.max acc k
          ) checkmap 1
          in
          let darray = Array.make (kmax + 1) { base = -1; check = -1; rule = None; } in
          basemap |> IntMap.iter (fun k v ->
            darray.(k) <- { darray.(k) with base = v }
          );
          checkmap |> IntMap.iter (fun k v ->
            darray.(k) <- { darray.(k) with check = v }
          );
          rulemap |> IntMap.iter (fun k v ->
            darray.(k) <- { darray.(k) with rule = v }
          );
          darray

      | (node, patlst) :: rest ->
          let lst = aux patlst in
          let indexlst = List.map fst lst in
          let cldpatlst = List.map (fun x -> fst (snd x)) lst in
          let rulelst = List.map (fun x -> snd (snd x)) lst in
          let base =
            if (List.length cldpatlst) = 0 then
              -1
            else
              search_base checkset indexlst (node + 1)
          in
          let basemap = IntMap.add node base basemap in
          let checkset = indexlst |> List.fold_left (fun acc uc ->
            IntSet.add (base + (Uchar.to_int uc) - mincode) acc
          ) checkset
          in
          let (checkmap, rulemap) = List.fold_left2 (fun (cmap, smap) uc scopt ->
            let cmap = cmap |> IntMap.add (base + (Uchar.to_int uc) - mincode) node in
            let smap = smap |> IntMap.add (base + (Uchar.to_int uc) - mincode) scopt in
            (cmap, smap)
          ) (checkmap, rulemap) indexlst rulelst
          in
          let cldpats = List.map2 (fun uc child ->
            (base + (Uchar.to_int uc) - mincode, child)
          ) indexlst cldpatlst
          in
          iter (cldpats @ rest) basemap checkset checkmap rulemap
    in
    let darray = iter [(0, patlst)] (IntMap.empty) (IntSet.empty) (IntMap.empty) (IntMap.empty) in
    (darray, mincode)


  let match_prefix trie uchlst stpos res =
    let (darray, mincode) = trie in
    let alen = Array.length darray in
    let rec iter ulst pos node res =
      match ulst with
      | [] ->
          res

      | uch :: rest ->
          if darray.(node).base < 0 then
            res
          else
            let c = Uchar.to_int uch in
            if c < mincode then
              res
            else
              let c = c - mincode in
              let nextnode = darray.(node).base + c in
              if nextnode >= alen || darray.(nextnode).check <> node then
                res
              else
                match (darray.(nextnode).rule, res) with
                | (Some(Exception(r)), _)             -> (MatchException(r))
                | (Some(Normal(r)), MatchNormal(acc)) -> iter ulst (pos + 1) nextnode (MatchNormal((stpos, r) :: acc))
                | _                                   -> iter ulst (pos + 1) nextnode res
    in
      iter uchlst stpos 0 res


  let match_every trie uchlst =
    let rec iter ulst pos res =
      match ulst with
      | [] ->
          res

      | uch :: rest ->
          match res with
          | MatchNormal(_) ->
              iter rest (pos + 1) (match_prefix trie ulst pos res)
          | MatchException(_) ->
              res
    in
    iter uchlst 0 (MatchNormal([]))

end


type t = Trie.t


let empty = Trie.empty


let read_exception_list (json : YS.json) : pattern list =
  let jsonlst = json |> YS.Util.to_list in
  jsonlst |> List.fold_left (fun mapacc json ->
    match json with
    | (_, `Tuple[json1; json2]) ->
        let wordfrom = json1 |> YS.Util.to_string in
        let wfuchlst = InternalText.to_uchar_list (InternalText.of_utf8 wordfrom) in
        let jsonlstto = json2 |> YS.Util.to_list in
        let fraclstto = jsonlstto |> List.map YS.Util.to_string in
        Alist.extend mapacc (wfuchlst, Exception(fraclstto))

    | _ ->
        raise (YS.Util.Type_error("Expects pair", json))

  ) Alist.empty |> Alist.to_list


let numeric (uch : Uchar.t) : number option =
  let cp = Uchar.to_int uch in
  let cp0 = Char.code '0' in
  let cp9 = Char.code '9' in
    if cp0 <= cp && cp <= cp9 then
      Some(cp - cp0)
    else
      None


let specialmarker = Uchar.of_char '.'


let convert_pattern (rng : Range.t) (strpat : string) : pattern =
  let uchlstraw = InternalText.to_uchar_list (InternalText.of_utf8 strpat) in
  let (beginningopt, uchlstsub) =
    match uchlstraw with
    | [] ->
        raise (InvalidPatternElement(rng))

    | uch0 :: uchtail ->
        if uch0 = specialmarker then
          (None, uchlstraw)
        else
          match numeric uch0 with
          | None      -> (Some(0), uchlstraw)
          | Some(num) -> (Some(num), uchtail)
  in
  let (numlst, uchlst) =
    uchlstsub |> list_fold_adjacent (fun (nacc, uacc) uch _ optnext ->
      match numeric uch with
      | Some(_) ->
          (nacc, uacc)

      | None ->
          begin
            match optnext with
            | None ->
                (Alist.extend nacc 0, Alist.extend uacc uch)

            | Some(uchnext) ->
                let (num, uch) =
                  match numeric uchnext with
                  | None      -> (0, uch)
                  | Some(num) -> (num, uch)
                in
                (Alist.extend nacc num, Alist.extend uacc uch)
          end
    ) (Alist.empty, Alist.empty)
    |> (function (nlst, ulst) -> (Alist.to_list nlst, Alist.to_list ulst))
  in
  let numlst =
    match beginningopt with
    | None      -> numlst
    | Some(num) -> num :: numlst
  in
  (uchlst, Normal(numlst))


let read_pattern_list (json : YS.json) : pattern list =
  let jsons = json |> YS.Util.to_list  in
  jsons |> List.map (fun ((pos, _) as json) ->
    let rng = MYU.make_range pos in
    convert_pattern rng (YS.Util.to_string json)
  )


let read_assoc (assoc : MYU.assoc) : t =
  let excplst = assoc |> MYU.find "exceptions" |> read_exception_list in
  let hyphpatlst = assoc |> MYU.find "patterns" |> read_pattern_list in
  let pattrie = Trie.make (excplst @ hyphpatlst) in
  pattrie


let main (abspath : abs_path) : t =
  let pathstr = get_abs_path_string abspath in
  try
    let json = YS.from_file ~fname:pathstr pathstr in
      (* -- may raise 'Sys_error'  -- *)
    let assoc = json |> MYU.make_assoc in
    read_assoc assoc
  with
  | Yojson.Json_error(msg) -> MYU.syntax_error pathstr msg


let make_fraction fracacc =
  fracacc |> Alist.to_list


(* --
   'lookup_patterns':
     determines hyphen pattern of the given word.
     this implemenmtation is currently very inefficient. -- *)
let lookup_patterns (lmin : int) (rmin : int) (pattrie : Trie.t) (uchseglst : uchar_segment list) : (uchar_segment list) list =
  let uchlst = uchseglst |> List.map (fun (u, _) -> u) in
  let uchlstwithsm = (specialmarker :: uchlst) @ [specialmarker] in
  match Trie.match_every pattrie uchlstwithsm with
  | MatchNormal(rulelst) ->
      begin
        let len = List.length uchseglst in
        let clst = uchseglst |> List.map (fun uchseg -> (uchseg, ref 0)) in
        let () = rulelst |> List.iter (fun (pos, nlst) ->
          let (pos, nlst) =
            match (pos - 2) with
            | -2 -> (0, nlst)
            | -1 -> (0, List.tl nlst)
            | n  -> (n, nlst)
          in
          let rec aux i clst nlst =
            if i < pos then
              aux (i + 1) (List.tl clst) nlst
            else
              match (clst, nlst) with
              | ((uchseg, nref) :: crest, num :: nrest) ->
                  nref := max (!nref) num;
                  aux (i + 1) crest nrest

              | _ ->
                  ()
          in
          aux 0 clst nlst
        )
        in
        let (_, acc, fracaccopt) =
          clst |> List.fold_left (fun (i, acc, fracaccopt) (uchseg, numref) ->
            if (!numref) mod 2 = 1 && i + 1 >= lmin && len - (i + 1) >= rmin then
            (* -- if able to break the word with hyphen immediately after the current position -- *)
              let fracacc =
                match fracaccopt with
                | Some(fracacc) -> fracacc
                | None          -> Alist.empty
              in
                let sfrac = make_fraction (Alist.extend fracacc uchseg) in
                  (i + 1, Alist.extend acc sfrac, None)
            else
              match fracaccopt with
              | Some(fracacc) -> (i + 1, acc, Some(Alist.extend fracacc uchseg))
              | None          -> (i + 1, acc, Some(Alist.extend Alist.empty uchseg))
          ) (0, Alist.empty, None)
        in
          match fracaccopt with
          | Some(fracacc) -> Alist.extend acc (make_fraction fracacc) |> Alist.to_list
          | None          -> acc |> Alist.to_list
      end

  | MatchException(sfraclst) ->
      sfraclst |> List.map (fun sfrac ->
        let uchlst = InternalText.to_uchar_list (InternalText.of_utf8 sfrac) in
          uchlst |> List.map (fun uch -> (uch, []))
      )


let lookup (lmin : int) (rmin : int) (pattrie : t) (uchseglst : uchar_segment list) : answer =
  let fraclst = lookup_patterns lmin rmin pattrie uchseglst in
  match fraclst with
  | frac :: [] -> Single(frac)
  | _          -> Fractions(fraclst)
