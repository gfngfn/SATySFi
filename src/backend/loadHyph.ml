
open MyUtil
open CharBasis

module YS = Yojson.SafePos
module MYU = MyYojsonUtil

exception InvalidPatternElement of Range.t

type number = int

type hyph_rule =
  | Normal    of number list
  | Exception of string list

type pattern_element =
  | SpecialMarker
  | UChar of int

type pattern = (pattern_element list) * hyph_rule

type match_normal_single =
  | MatchNormalSingle of number * number list
      (* --
        `MatchNormalSingle(pos, ns)`

         * `pos`: the position in the word where the matched region starts
         * `ns`: the numbers given to the characters in the matched region
      -- *)

type match_result =
  | MatchNormal    of match_normal_single list
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
    type t = pattern_element
    let compare i j =
      match (i, j) with
      | (SpecialMarker, SpecialMarker) -> 0
      | (SpecialMarker, _) -> -1
      | (_, SpecialMarker) -> 1
      | (i, j) -> Pervasives.compare i j
  end)

module IntMap = Map.Make
  (struct
    type t = int
    let compare i j = i - j
  end)


(* -- Trie that holds hyphenation patterns (contains exceptions) -- *)
module PatternTrie
: sig

  type t

  val empty : t

  val make : pattern list -> t

  val match_every : t -> pattern_element list -> match_result

  end
= struct

  (* -- implemented with double-array -- *)
  type node =
    {
      base  : int;
      check : int;
      rule  : hyph_rule option;
    }

  (* -- `code_info`: the type of data for encoding characters into numbers in trie -- *)
  type code_info = {
    min_code            : int;
    max_code            : int;
    special_marker_code : int;
  }

  type base_map = int IntMap.t

  type check_map = int IntMap.t

  type rule_map = (hyph_rule option) IntMap.t

  type t = {
    nodes     : node array;
    code_info : code_info;
  }


  let empty = {
    nodes = Array.of_list [{ base = -1; check = 0; rule = None; }];
    code_info = {
      min_code            = 0;
      max_code            = 0;
      special_marker_code = 0;
    }
  }


  let make_code_info (patlst : pattern list) : code_info =
    let (mincode, maxcode) =
      patlst |> List.fold_left (fun acc (pelst, _) ->
        pelst |> List.fold_left (fun (min, max) e ->
          match e with
          | SpecialMarker -> acc
          | UChar(i)      -> (Pervasives.min i min, Pervasives.max i max)
        ) acc
      ) (Uchar.to_int Uchar.max, Uchar.to_int Uchar.min)
    in
    let smcode = maxcode + 1 in
    {
      min_code            = mincode;
      max_code            = maxcode;
      special_marker_code = smcode;
    }


  let int_of_pe_by_using_code_info (codeinfo : code_info) =
    let smcode  = codeinfo.special_marker_code in
    let mincode = codeinfo.min_code in
    function
    | SpecialMarker -> smcode - mincode
    | UChar(i)      -> i - mincode


  let int_of_pe_opt_by_using_code_info (codeinfo : code_info) =
    let smcode  = codeinfo.special_marker_code in
    let mincode = codeinfo.min_code in
    let maxcode = codeinfo.max_code in
    function
    | SpecialMarker ->
        Some(smcode - mincode)

    | UChar(i) ->
        if i < mincode || i > maxcode then
          None
        else
          Some(i - mincode)


  let make (patlst : pattern list) : t =
    let codeinfo = make_code_info patlst in
    let int_of_pe = int_of_pe_by_using_code_info codeinfo in

    (* --
      `aux patlst`:

      classifies patterns by their first character (or `SpecialMarker`).
      Each element `(pe, (pats, ruleopt))` of the return value stands for a class:

      * `pe`: the first character
      * `pats`: the patterns subsequent to `pe`
      * `ruleopt`: if a pattern ends with `pe`, stores its corresponding hypenation rule.
    -- *)
    let aux (patlst : pattern list) : (pattern_element * (pattern list * hyph_rule option)) list =
      let map =
        patlst |> List.fold_left (fun accmap (pelst, rule) ->
          match pelst with
          | [] ->
              accmap

          | hdch :: rest ->
              accmap |> UcharMap.update hdch (fun opt ->
                match (opt, rest) with
                | (None, [])         -> Some([],   Some(rule))
                | (None, _)          -> Some([(rest, rule)], None)
                | (Some(ull, _), []) -> Some(ull,  Some(rule))
                | (Some(ull, r), _)  -> Some((rest, rule) :: ull, r)
              )

        ) UcharMap.empty
      in
      UcharMap.fold (fun k v acc -> (k, v) :: acc) map []
    in

    (* --
      `search_base checkset indexlst offset`:

      * `checkset`: set of indices that have been already used in CHECK array
      * `indexlst`: indices of the children of the node that will be added to the trie
      * `offset`: minimum candidate for the answer

      finds the BASE value suitable for the newly added node.
    -- *)
    let rec search_base (checkset : IntSet.t) (indexlst : pattern_element list) (offset : int) : int =
      if indexlst |> List.exists (fun pe -> checkset |> IntSet.mem ((int_of_pe pe) + offset)) then
        search_base checkset indexlst (offset + 1)
      else
        offset
    in

    let make_double_array (basemap : base_map) (checkmap : check_map) (rulemap : rule_map) =
      let kmax =
        IntMap.fold (fun k _ acc ->
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
    in

    (* --
      `iter stk basemap checkset checkmap rulemap`:

      * `stk`: the stack keeping data that remain to be dealt with;
          each element `(i, pats)` of `stk` stands for
          “there is a node that should be stored at the index `i` and its children are `pats`.”
      * `basemap`: the BASE array under construction
      * `checkset`: the set of the already used indices of CHECK array (i.e. the domain of `checkmap`)
      * `checkmap`: the CHECK array under construction
      * `rulemap`: maps the leaves of the trie to their corresponding hyphenation rule

      iteratively constructs the BASE and CHECK array.
    -- *)
    let rec iter (stk : (int * pattern list) list) (basemap : base_map) (checkset : IntSet.t) (checkmap : check_map) (rulemap : rule_map) =
      match stk with
      | [] ->
          make_double_array basemap checkmap rulemap

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
          let basemap = basemap |> IntMap.add node base in
          let checkset =
            indexlst |> List.fold_left (fun accset pe ->
              accset |> IntSet.add (base + (int_of_pe pe))
            ) checkset
          in
          let (checkmap, rulemap) =
            List.fold_left2 (fun (cmap, rmap) pe scopt ->
              let cmap = cmap |> IntMap.add (base + (int_of_pe pe)) node in
              let rmap = rmap |> IntMap.add (base + (int_of_pe pe)) scopt in
              (cmap, rmap)
            ) (checkmap, rulemap) indexlst rulelst
          in
          let cldpats =
            List.map2 (fun pe child ->
              (base + (int_of_pe pe), child)
            ) indexlst cldpatlst
          in
          iter (cldpats @ rest) basemap checkset checkmap rulemap
    in

    let darray = iter [(0, patlst)] IntMap.empty IntSet.empty IntMap.empty IntMap.empty in
    {
      nodes     = darray;
      code_info = codeinfo;
    }

  (* --
    `match_prefix trie pelst stpos res`:

    * `trie`: the trie tree
    * `pelst`: a suffix of the original word to be matched
    * `stpos`: the start position of `pelst` in the original word
    * `res`: the current accumulated result of matching

    traverses `trie` and updates matching results.
  -- *)
  let match_prefix (trie : t) (pelst : pattern_element list) (stpos : number) (res : match_result) : match_result =
    let darray = trie.nodes in
    let int_of_pe_opt = int_of_pe_opt_by_using_code_info trie.code_info in
    let alen = Array.length darray in

    let rec iter pelst node res =
      match pelst with
      | [] ->
          res

      | pe :: rest ->
          if darray.(node).base < 0 then
          (* -- if the node corresponding to the index `node` is a leaf -- *)
            res
          else
            match int_of_pe_opt pe with
            | None ->
              (* -- if the character `pe` is out of the range the trie supports -- *)
                res

            | Some(i) ->
                let nextnode = darray.(node).base + i in
                if nextnode >= alen || darray.(nextnode).check <> node then
                (* -- if there is no branch for the next character `pe` -- *)
                  res
                else
                  match (darray.(nextnode).rule, res) with
                  | (Some(Exception(r)), _)             -> (MatchException(r))
                  | (Some(Normal(r)), MatchNormal(acc)) -> iter rest nextnode (MatchNormal(MatchNormalSingle(stpos, r) :: acc))
                  | _                                   -> iter rest nextnode res

    in
    iter pelst 0 res


  let match_every (trie : t) (pelst : pattern_element list) : match_result =
    let rec iter pelst pos res =
      match pelst with
      | [] ->
          res

      | _ :: rest ->
          match res with
          | MatchNormal(_) ->
              let res = res |> match_prefix trie pelst pos in
              iter rest (pos + 1) res

          | MatchException(_) ->
              res

    in
    iter pelst 0 (MatchNormal([]))

end


type t = PatternTrie.t


let empty = PatternTrie.empty


(* -- Special marker matches the beginning or ending of a word. -- *)
let specialmarker_uch = Uchar.of_char '.'


let add_specialmarker pelst =
  (SpecialMarker :: pelst) @ [SpecialMarker]


let read_exception_list (json : YS.json) : pattern list =
  let jsonlst = json |> YS.Util.to_list in
  jsonlst |> List.fold_left (fun mapacc json ->
    match json with
    | (_, `Tuple[json1; json2]) ->
        let wordfrom = json1 |> YS.Util.to_string in
        let wfuchlst = InternalText.to_uchar_list (InternalText.of_utf8 wordfrom) in
        let wfpelst = add_specialmarker (wfuchlst |> List.map (fun uch -> UChar(Uchar.to_int uch))) in
        let jsonlstto = json2 |> YS.Util.to_list in
        let fraclstto = jsonlstto |> List.map YS.Util.to_string in
        Alist.extend mapacc (wfpelst, Exception(fraclstto))

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


let convert_pattern (rng : Range.t) (strpat : string) : pattern =
  let uchlstraw = InternalText.to_uchar_list (InternalText.of_utf8 strpat) in
  let (beginningopt, uchlstsub) =
    match uchlstraw with
    | [] ->
        raise (InvalidPatternElement(rng))

    | uch0 :: uchtail ->
        if uch0 = specialmarker_uch then
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
  let pelst = uchlst |> List.map (fun uch ->
    if uch = specialmarker_uch then
      SpecialMarker
    else
      UChar(Uchar.to_int uch)
  )
  in
  let numlst =
    match beginningopt with
    | None      -> numlst
    | Some(num) -> num :: numlst
  in
  (pelst, Normal(numlst))


let read_pattern_list (json : YS.json) : pattern list =
  let jsons = json |> YS.Util.to_list  in
  jsons |> List.map (fun ((pos, _) as json) ->
    let rng = MYU.make_range pos in
    convert_pattern rng (YS.Util.to_string json)
  )


let read_assoc (assoc : MYU.assoc) : t =
  let excplst = assoc |> MYU.find "exceptions" |> read_exception_list in
  let hyphpatlst = assoc |> MYU.find "patterns" |> read_pattern_list in
  let pattrie = PatternTrie.make (excplst @ hyphpatlst) in
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
  `make_pattern_element_list_of_word uchseglst`:
    converts the given word `uchseglst` into a pattern element list
    in order to traverse the pattern trie.
-- *)
let make_pattern_element_list_of_word (uchseglst : uchar_segment list) : pattern_element list =
  let uchlst = uchseglst |> List.map (fun (u, _) -> u) in
  add_specialmarker (uchlst |> List.map (fun uch -> UChar(Uchar.to_int uch)))


(* --
   'lookup_patterns':
     determines hyphen pattern of the given word. -- *)
let lookup_patterns (lmin : int) (rmin : int) (pattrie : PatternTrie.t) (uchseglst : uchar_segment list) : (uchar_segment list) list =
  let pelst = make_pattern_element_list_of_word uchseglst in
  match PatternTrie.match_every pattrie pelst with
  | MatchNormal(rulelst) ->
      begin
        let len = List.length uchseglst in
        let clst = uchseglst |> List.map (fun uchseg -> (uchseg, ref 0)) in
        rulelst |> List.iter (function MatchNormalSingle(pos, nlst) ->
          let (pos, nlst) =
            match (pos - 2) with
            | -2 -> (0, List.tl nlst)
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
        );
        let (_, acc, fracaccopt) =
          clst |> List.fold_left (fun (i, acc, fracaccopt) (uchseg, numref) ->
            let fracacc =
              match fracaccopt with
              | Some(fracacc) -> fracacc
              | None          -> Alist.empty
            in
            if (!numref) mod 2 = 1 && i + 1 >= lmin && len - (i + 1) >= rmin then
            (* -- if able to break the word with hyphen immediately after the current position -- *)
              let sfrac = make_fraction (Alist.extend fracacc uchseg) in
              (i + 1, Alist.extend acc sfrac, None)
            else
              (i + 1, acc, Some(Alist.extend fracacc uchseg))
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
