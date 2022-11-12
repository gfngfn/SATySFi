
open MyUtil
open FontError

module V = Otfed.Value
module I = Otfed.Intermediate
module D = Otfed.Decode


type original_glyph_id = V.glyph_id

type 'a ok = ('a, font_error) result

type design_units = V.design_units

type per_mille =
  | PerMille of int

type metrics = per_mille * per_mille * per_mille


let pickup (xs : 'a list) (predicate : 'a -> bool) (e : 'b) (k : 'a -> ('b, 'e) result) : ('b, 'e) result =
  let open ResultMonad in
  match xs |> List.filter predicate with
  | head :: _ -> k head
  | []        -> return e


let ( <| ) f x = f x


type cid_system_info = {
  registry   : string;
  ordering   : string;
  supplement : int;
}


let adobe_identity = { registry = "Adobe"; ordering = "Identity"; supplement = 0; }


type font_registration =
  | CIDFontType0Registration   of cid_system_info * bool
      (* Last boolean: true iff it should embed /W information *)
  | CIDFontType2OTRegistration of cid_system_info * bool
      (* Last boolean: true iff it should embed /W information *)


let extract_registration ~(file_path : abs_path) (d : D.source) : font_registration ok =
  let open ResultMonad in
  begin
    match d with
    | D.Cff(cff) ->
        begin
          D.Cff.top_dict cff >>= fun top_dict ->
          let cid_system_info =
            match top_dict.cid_info with
            | None ->
                (* If not a CID-keyed font: *)
                adobe_identity

            | Some(cid_info) ->
                {
                  registry   = cid_info.registry;
                  ordering   = cid_info.ordering;
                  supplement = cid_info.supplement;
                }
          in
          return (CIDFontType0Registration(cid_system_info, true))
        end

    | D.Ttf(_) ->
        return (CIDFontType2OTRegistration(adobe_identity, true))
  end
    |> Result.map_error (fun e -> FailedToDecodeFont(file_path, e))


let get_main_decoder_single (abspath : abs_path) : (D.source * font_registration) ok =
  let open ResultMonad in
  let* data =
    read_file abspath
      |> Result.map_error (fun msg -> FailedToReadFont(abspath, msg))
  in
  let* source =
    D.source_of_string data
      |> Result.map_error (fun e -> FailedToDecodeFont(abspath, e))
  in
  match source with
  | D.Collection(_) ->
      err @@ NotASingleFont(abspath)

  | D.Single(d) ->
      extract_registration ~file_path:abspath d >>= fun registration ->
      return (d, registration)


let get_main_decoder_ttc (abspath : abs_path) (index : int) : (D.source * font_registration) ok =
  let open ResultMonad in
  let* data =
    read_file abspath
      |> Result.map_error (fun msg -> FailedToReadFont(abspath, msg))
  in
  let* source =
    D.source_of_string data
      |> Result.map_error (fun e -> FailedToDecodeFont(abspath, e))
  in
  match source with
  | D.Single(_) ->
      err @@ NotAFontCollectionElement(abspath, index)

  | D.Collection(ds) ->
      begin
        match List.nth_opt ds index with
        | None ->
            let num_elements = List.length ds in
            err @@ CollectionIndexOutOfBounds{ path = abspath; index; num_elements }

        | Some(d) ->
            extract_registration ~file_path:abspath d >>= fun registration ->
            return (d, registration)
      end


module UHt = Hashtbl.Make(struct
  type t = Uchar.t
  let equal = (=)
  let hash = Hashtbl.hash
end)


module GOHt = Hashtbl.Make(struct
  type t = original_glyph_id
  let equal = (=)
  let hash = Hashtbl.hash
end)

type subset_glyph_id = SubsetNumber of V.glyph_id

module GSHt = Hashtbl.Make(struct
  type t = subset_glyph_id
  let equal = (=)
  let hash = Hashtbl.hash
end)

type glyph_id_pair = {
  original_id : original_glyph_id;
  subset_id   : subset_glyph_id;
}

type glyph_id =
  | SubsetGlyphID of original_glyph_id * subset_glyph_id

type original_glyph_segment = original_glyph_id * original_glyph_id list

type glyph_segment = glyph_id * glyph_id list


let notdef = SubsetGlyphID(0, SubsetNumber(0))


let hex_of_glyph_id ((SubsetGlyphID(_, SubsetNumber(n))) : glyph_id) =
  let b0 = n / 256 in
  let b1 = n mod 256 in
  Printf.sprintf "%02X%02X" b0 b1


module SubsetMap : sig
  type t
  val create : abs_path -> D.source -> int -> t
  val intern : original_glyph_id -> t -> subset_glyph_id ok
  val to_list : t -> original_glyph_id list
end = struct

  type subset = {
    file_path          : abs_path;
    decoder            : D.source;
    original_to_subset : subset_glyph_id GOHt.t;
    subset_to_original : original_glyph_id GSHt.t;
    count : int ref;
    store : (original_glyph_id Alist.t) ref;
  }

  type t =
    | Subset of subset


  let create file_path d n =
    let ht = GOHt.create n in
    let revht = GSHt.create n in
    GOHt.add ht 0 (SubsetNumber(0));
    Subset{
      file_path          = file_path;
      decoder            = d;
      original_to_subset = ht;
      subset_to_original = revht;
      count = ref 0;
      store = ref (Alist.extend Alist.empty 0);
    }


  let get_elements_of_composite_glyph ~(file_path : abs_path) (d : D.source) (gidorg : original_glyph_id) : (original_glyph_id list) ok =
    let open ResultMonad in
    begin
      match d with
      | D.Cff(_) ->
          return []

      | D.Ttf(ttf) ->
          D.Ttf.loca ttf gidorg >>= function
          | None ->
              return []

          | Some(loc) ->
              D.Ttf.glyf ttf loc >>= fun ttf_glyph_info ->
              begin
                match ttf_glyph_info.description with
                | SimpleGlyph(_) ->
                    return []

                | CompositeGlyph(composite) ->
                    return begin
                      composite.composite_components |> List.map (fun components ->
                        components.V.Ttf.component_glyph_id
                      )
                    end
              end
    end
      |> Result.map_error (fun e -> FailedToDecodeFont(file_path, e))



  let rec intern (gidorg : original_glyph_id) (submap : t) : subset_glyph_id ok =
    let open ResultMonad in
    match submap with
    | Subset(r) ->
        let ht = r.original_to_subset in
        let revht = r.subset_to_original in
        let count = r.count in
        let store = r.store in
        begin
          match GOHt.find_opt ht gidorg with
          | Some(gidsub) ->
              return gidsub

          | None ->
              incr count;
              let gidsub = SubsetNumber(!count) in
              GOHt.add ht gidorg gidsub;
              GSHt.add revht gidsub gidorg;
              let alst = Alist.extend (!store) gidorg in
              store := alst;
              let* gidorgs_elem = get_elements_of_composite_glyph ~file_path:r.file_path r.decoder gidorg in
              let* () =
                gidorgs_elem |> foldM (fun () gidorg_elem ->
                  let* _ = intern gidorg_elem submap in
                  return ()
                ) ()
              in
              return gidsub
        end


  let to_list submap =
    match submap with
    | Subset(r) -> Alist.to_list !(r.store)

end


type subset_map = SubsetMap.t


module GlyphIDTable : sig
  type t
  val create : subset_map -> int -> t
  val add : Uchar.t -> original_glyph_id -> t -> unit ok
  val find_opt : Uchar.t -> t -> glyph_id_pair option
  val find_rev_opt : original_glyph_id -> t -> Uchar.t option
  val fold_rev : (subset_glyph_id -> Uchar.t -> 'a -> 'a) -> 'a -> t -> 'a
end = struct

    type t = {
      subset_map   : subset_map;
      main         : glyph_id_pair UHt.t;
      rev_subset   : Uchar.t GSHt.t;
      rev_original : Uchar.t GOHt.t;
    }


    let create submap n =
      let ht = UHt.create n in
      let revsubht = GSHt.create n in
      let revorght = GOHt.create n in
      {
        subset_map   = submap;
        main         = ht;
        rev_subset   = revsubht;
        rev_original = revorght;
      }


    let add (uch : Uchar.t) (gidorg : original_glyph_id) r =
      let open ResultMonad in
      let submap = r.subset_map in
      let ht = r.main in
      let revsubht = r.rev_subset in
      let revorght = r.rev_original in
      let* gidsub = submap |> SubsetMap.intern gidorg in
      UHt.add ht uch { original_id = gidorg; subset_id = gidsub; };
      match GSHt.find_opt revsubht gidsub with
      | None ->
          GSHt.add revsubht gidsub uch;
          GOHt.add revorght gidorg uch;
          return ()

      | Some(uch_pre) ->
          Logging.warn_noninjective_cmap uch_pre uch gidorg;
          return ()


    let find_opt uch r =
      UHt.find_opt r.main uch


    let find_rev_opt gidorg r =
      GOHt.find_opt r.rev_original gidorg


    let fold_rev f gidsub r =
      GSHt.fold f r.rev_subset gidsub

  end


type bbox = per_mille * per_mille * per_mille * per_mille


module GlyphBBoxTable : sig
  type t
  val create : int -> t
  val add : original_glyph_id -> per_mille * bbox -> t -> unit
  val find_opt : original_glyph_id -> t -> (per_mille * bbox) option
  val fold : (original_glyph_id -> per_mille * bbox -> 'a -> 'a) -> 'a -> t -> 'a
end = struct

    type t = (per_mille * bbox) GOHt.t

    let create =
      GOHt.create

    let add gid pair gmtbl =
      GOHt.add gmtbl gid pair

    let find_opt gid gmtbl =
      GOHt.find_opt gmtbl gid

    let fold f init gmtbl =
      GOHt.fold f gmtbl init

  end


let per_mille ~(units_per_em : int) (w : design_units) : per_mille =
  PerMille(int_of_float ((float_of_int (w * 1000)) /. (float_of_int units_per_em)))


module GSet = Set.Make
  (struct
    type t = original_glyph_id
    let compare = Stdlib.compare
  end)


module GMap = Map.Make
  (struct
    type t = original_glyph_id
    let compare = Stdlib.compare
  end)


type mark_class = int

type anchor_point = per_mille * per_mille

type mark_assoc = (V.glyph_id * V.mark_record) list


module MarkTable : sig
  type t
  val create : unit -> t
  val add_base : units_per_em:int -> int -> mark_assoc -> (V.glyph_id * V.base_record) list -> t -> unit
  val add_ligature : units_per_em:int -> int -> mark_assoc -> (V.glyph_id * V.ligature_attach) list -> t -> unit
  val add_mark_to_mark : units_per_em:int -> int -> mark_assoc -> (V.glyph_id * V.mark2_record) list -> t -> unit
  val find_base_opt : original_glyph_id * original_glyph_id -> t -> (anchor_point * anchor_point) option
  val find_ligature_opt : int -> original_glyph_id * original_glyph_id -> t -> (anchor_point * anchor_point) option
  val find_mark_to_mark_opt : original_glyph_id * original_glyph_id -> t -> (anchor_point * anchor_point) option
end
= struct

    type mark_to_base_entry = {
      class_count : int;
      mark_map    : (mark_class * anchor_point) GMap.t;
      base_map    : ((anchor_point option) array) GMap.t;
    }

    type mark_to_ligature_entry = {
      lig_class_count : int;
      lig_mark_map    : (mark_class * anchor_point) GMap.t;
      lig_base_map    : (((anchor_point option) array) array) GMap.t;
    }

    type mark_to_mark_entry = {
      class_count : int;
      mark_map    : (mark_class * anchor_point) GMap.t;
      mark2_map   : (anchor_point array) GMap.t;
    }

    type t = {
      mutable mark_to_base_table     : mark_to_base_entry list;
      mutable mark_to_ligature_table : mark_to_ligature_entry list;
      mutable mark_to_mark_table     : mark_to_mark_entry list;
    }


    let create () =
      { mark_to_base_table = []; mark_to_ligature_table = []; mark_to_mark_table = []; }


    let make_mark_map pmf markassoc =
      markassoc |> List.fold_left (fun map (gidmark, (i, (x, y, _))) ->
        map |> GMap.add gidmark (i, (pmf x, pmf y))
      ) GMap.empty


    let base_record pmf (x, y, _) =
      (pmf x, pmf y)


    let add_base ~units_per_em class_count markassoc baseassoc mktbl =
      let pmf = per_mille ~units_per_em in
      let mark_map = make_mark_map pmf markassoc in
      let base_map =
        baseassoc |> List.fold_left (fun map (gidbase, arr) ->
          map |> GMap.add gidbase (arr |> Array.map (Option.map (base_record pmf)))
        ) GMap.empty
      in
      let entry = { class_count; mark_map; base_map; } in
      mktbl.mark_to_base_table <- entry :: mktbl.mark_to_base_table


    let add_ligature ~units_per_em lig_class_count markassoc (ligassoc : (V.glyph_id * V.ligature_attach) list) mktbl =
      let pmf = per_mille ~units_per_em in
      let lig_mark_map =
        markassoc |> List.fold_left (fun map (gidmark, (i, (x, y, _))) ->
          map |> GMap.add gidmark (i, (pmf x, pmf y))
        ) GMap.empty
      in
      let lig_base_map =
        ligassoc |> List.fold_left (fun map (gidlig, comprcdlst) ->
          let lst = comprcdlst |> List.map (Array.map (Option.map (base_record pmf))) in
            map |> GMap.add gidlig (Array.of_list lst)
        ) GMap.empty
      in
      let entry = { lig_class_count; lig_mark_map; lig_base_map; } in
      mktbl.mark_to_ligature_table <- entry :: mktbl.mark_to_ligature_table


    let add_mark_to_mark ~units_per_em class_count markassoc mark2assoc mktbl =
      let pmf = per_mille ~units_per_em in
      let mark_map = make_mark_map pmf markassoc in
      let mark2_map =
        mark2assoc |> List.fold_left (fun map (gidmark2, arr) ->
          map |> GMap.add gidmark2 (arr |> Array.map (base_record pmf))
        ) GMap.empty
      in
      let entry = { class_count; mark_map; mark2_map; } in
      mktbl.mark_to_mark_table <- entry :: mktbl.mark_to_mark_table


    let find_base_opt (gidbase, gidmark) mktbl =
      let rec aux lst =
        match lst with
        | [] ->
            None

        | entry :: tail ->
            let baseopt = entry.base_map |> GMap.find_opt gidbase in
            let markopt = entry.mark_map |> GMap.find_opt gidmark in
            begin
              match (baseopt, markopt) with
              | (None, _) | (_, None)        -> aux tail
              | (Some(arr), Some(c, ptmark)) ->
                begin
                  match arr.(c) with
                  | Some(a) -> Some((a, ptmark))
                  | None    -> aux tail
                end
            end
      in
      aux mktbl.mark_to_base_table


    let find_ligature_opt i (gidlig, gidmark) mktbl =
      let rec aux lst =
        match lst with
        | [] ->
            None

        | entry :: tail ->
            let opt =
              let open OptionMonad in
              entry.lig_mark_map |> GMap.find_opt gidmark >>= fun (c, ptmark) ->
              entry.lig_base_map |> GMap.find_opt gidlig >>= fun ligatt ->
              ligatt.(i).(c) >>= fun ptlig ->
              return (ptlig, ptmark)
            in
            begin
              match opt with
              | None    -> aux tail
              | Some(_) -> opt
            end
      in
      aux mktbl.mark_to_ligature_table


    let find_mark_to_mark_opt (gidmark2, gidmark) mktbl =
      let rec aux lst =
        match lst with
        | [] ->
            None

        | entry :: tail ->
            let mark2opt = entry.mark2_map |> GMap.find_opt gidmark2 in
            let markopt = entry.mark_map |> GMap.find_opt gidmark in
            begin
              match (mark2opt, markopt) with
              | (None, _) | (_, None)        -> aux tail
              | (Some(arr), Some(c, ptmark)) -> Some((arr.(c), ptmark))
            end
      in
      aux mktbl.mark_to_mark_table

  end


let select_langsys gxxx_langsys script =
  let open ResultMonad in
  gxxx_langsys script >>= fun langsys_res ->
  let langsys =
    match langsys_res with
    | (Some(langsys), _)   -> langsys
    | (None, langsys :: _) -> langsys
    | (None, [])           -> remains_to_be_implemented "no langsys"
      (* TODO: should depend on the current language system *)
  in
  return langsys


let select_gpos_langsys = select_langsys D.Gpos.langsyses
let select_gsub_langsys = select_langsys D.Gsub.langsyses


let get_mark_table ~(file_path : abs_path) ~(units_per_em : int) (d : D.source) : MarkTable.t ok =
  let open ResultMonad in
  let script_tag = "latn" in  (* TODO: make Script tags changeable *)
  let mktbl = MarkTable.create () in
  begin
    let* () =
      D.Gpos.get d >>= function
      | None ->
        (* If the font does NOT have a GPOS table: *)
          return ()

      | Some(igpos) ->
          D.Gpos.scripts igpos >>= fun scripts ->
        pickup scripts (fun gs -> String.equal (D.Gpos.get_script_tag gs) script_tag) () <| fun script ->
          select_gpos_langsys script >>= fun langsys ->
          D.Gpos.features langsys >>= fun (_, features) ->
          let* () =
            begin
              pickup features (fun gf -> String.equal (D.Gpos.get_feature_tag gf) "mark") () <| fun feature_mark ->
                D.Gpos.fold_subtables
                  ~markbase1:(fun clscnt () markassoc baseassoc ->
                    MarkTable.add_base ~units_per_em clscnt markassoc baseassoc mktbl
                  )
                  ~marklig1:(fun clscnt () markassoc ligassoc ->
                    MarkTable.add_ligature ~units_per_em clscnt markassoc ligassoc mktbl
                  )
                  feature_mark ()
            end
          in
          let* () =
            begin
              pickup features (fun gf -> String.equal (D.Gpos.get_feature_tag gf) "mkmk") () <| fun feature_mkmk ->
                D.Gpos.fold_subtables
                  ~markmark1:(fun clscnt () mark1assoc mark2assoc ->
                    MarkTable.add_mark_to_mark ~units_per_em clscnt mark1assoc mark2assoc mktbl
                  )
                  feature_mkmk ()
            end
          in
          return ()
    in
    return mktbl
  end
    |> Result.map_error (fun e -> FailedToDecodeFont(file_path, e))


let ( -@ ) (PerMille(x1), PerMille(y1)) (PerMille(x2), PerMille(y2)) =
  (PerMille(x1 - x2), PerMille(y1 - y2))


let ( +@ ) (PerMille(x1), PerMille(y1)) (PerMille(x2), PerMille(y2)) =
  (PerMille(x1 + x2), PerMille(y1 + y2))


type per_mille_vector = per_mille * per_mille

type mark_info =
  | Mark of glyph_id * per_mille * per_mille_vector

type glyph_synthesis = glyph_id * mark_info list

type ligature_matching =
  | Match    of original_glyph_id * (original_glyph_id * per_mille_vector) list * original_glyph_segment list
  | ReachEnd


module LigatureTable : sig
  type single = {
    tail     : original_glyph_id list;
    ligature : original_glyph_id;
  }
  type t
  val create : subset_map -> int -> t
  val add : original_glyph_id -> single list -> t -> unit ok
  val fold_rev : (subset_glyph_id -> original_glyph_id list -> 'a -> 'a) -> 'a -> t -> 'a
  val match_prefix : original_glyph_segment list -> MarkTable.t -> t -> ligature_matching
end = struct

    (* the type for pairs of the tail GID array and the GID of the resulting ligature. *)
    type single = {
      tail     : original_glyph_id list;
      ligature : original_glyph_id;
    }

    type t = {
      subset_map  : subset_map;
      entry_table : (single list) GOHt.t;
      rev_table   : (original_glyph_id list) GSHt.t;
    }


    let create (submap : subset_map) (n : int) : t =
      let htmain = GOHt.create n in
      let htrev = GSHt.create n in
      { subset_map = submap; entry_table = htmain; rev_table = htrev; }


    let add (gidorg : original_glyph_id) (liginfos : single list) (ligtbl : t) : unit ok =
      let open ResultMonad in
      let htmain = ligtbl.entry_table in
      let htrev = ligtbl.rev_table in
      let submap = ligtbl.subset_map in
      GOHt.add htmain gidorg liginfos;
      liginfos |> foldM (fun () single ->
        let gidorgtail = single.tail in
        let gidorg_lig = single.ligature in
        let* gidsub_lig = submap |> SubsetMap.intern gidorg_lig in
        match GSHt.find_opt htrev gidsub_lig with
        | None ->
            GSHt.add htrev gidsub_lig (gidorg :: gidorgtail);
            return ()

        | Some(_) ->
            Logging.warn_noninjective_ligature gidorg_lig;
            return ()
      ) ()


    let fold_rev f init ligtbl =
      let htrev = ligtbl.rev_table in
      GSHt.fold (fun gidsub gidorglst acc -> f gidsub gidorglst acc) htrev init


    (* `backtrack_mark_to_mark mktbl markbasef gobase markpairacc gomark` returns:

       - `Some(p)` if `gomark` can be attached at the position `p`
          to `gobase`, to which every mark in `markpairacc` is already attached.
       - `None` otherwise. *)
    let backtrack_mark_to_mark mktbl markbasef gobase markpairacc gomark =
      let rec aux markpairacc =
        match Alist.chop_last markpairacc with
        | None ->
            begin
              match markbasef (gobase, gomark) mktbl with
              | None           -> None
              | Some((vB, vM)) -> Some(vB -@ vM)
            end

        | Some((rest, (gomarklast, pM2))) ->
            begin
              match MarkTable.find_mark_to_mark_opt (gomarklast, gomark) mktbl with
              | None             -> aux rest
              | Some((vM2, vM1)) -> Some(vM2 -@ vM1 +@ pM2)
            end
      in
        aux markpairacc


    (* `attach_marks mktbl markbasef gobase gomarks` returns:

       - `Some([(gm1, p1), ..., (gmN, pN)])` if every `gmI` in `gomarks`
         can be attached to `gobase` at the position `pI`.
       - `None` otherwise. *)
    let attach_marks is_ligature mktbl markbasef gobase gomarks =
      let rec aux markpairacc gomarks =
        match gomarks with
        | [] ->
            Some(Alist.to_list markpairacc)

        | gomark :: gomarktail ->
            begin
              match backtrack_mark_to_mark mktbl markbasef gobase markpairacc gomark with
              | None ->
                  if not is_ligature then Logging.warn_nonattachable_mark gomark gobase;
                  None

              | Some(p) ->
                  aux (Alist.extend markpairacc (gomark, p)) gomarktail
            end
      in
      aux Alist.empty gomarks


    (* `make_ligature_mark_info mktbl golig markpairs` returns:

       - `Some(markinfolst)` if all diacritical marks of `markpairs`
          are attachable to the ligature `golig`.
       - `None` otherwise. *)
    let make_ligature_mark_info mktbl golig markpairs =
      let rec aux acc = function
        | [] ->
            Some(Alist.to_list acc)

        | (i, gomarks) :: tail ->
            let markbasef = MarkTable.find_ligature_opt i in
            begin
              match attach_marks true mktbl markbasef golig gomarks with
              | None              -> None
              | Some(markinfolst) -> aux (Alist.append acc markinfolst) tail
            end
      in
      aux Alist.empty markpairs


    (* `prefix mktbl golig lst1 seglst2` returns:

       - `Some(seglst, markinfolst)`
`        if `lst1` is a prefix of `seglst2` and
         forming the ligature does not prevent any attachment of diacritical marks,
         where `seglst` is the rest of `seglst2`
         and `markinfolst` is the position information of diacritical marks.
       - `None` otherwise. *)
    let prefix (mktbl : MarkTable.t) (golig : original_glyph_id) (lst1 : original_glyph_id list) (seglst2 : original_glyph_segment list) : (original_glyph_segment list * (original_glyph_id * anchor_point) list) option =
      let rec aux i acc lst1 seglst2 =
        match (lst1, seglst2) with
        | ([], _) ->
            let markpairs = Alist.to_list acc in
            begin
              match make_ligature_mark_info mktbl golig markpairs with
              | None              -> None
              | Some(markinfolst) -> Some(seglst2, markinfolst)
            end

        | (head1 :: tail1, (head2, gomarks) :: tail2) ->
            if head1 = head2 then
              let acc = Alist.extend acc (i, gomarks) in
              aux (i + 1) acc tail1 tail2
            else
              None

        | _ ->
            None
      in
      aux 0 Alist.empty lst1 seglst2


    let lookup (mktbl : MarkTable.t) (liginfolst : single list) (segorglst : original_glyph_segment list) =
      let rec aux liginfolst =
        match liginfolst with
        | [] ->
            None

        | single :: liginfotail ->
            let gotail = single.tail in
            let golig = single.ligature in
            begin
              match prefix mktbl golig gotail segorglst with
              | None ->
                  aux liginfotail

              | Some(orgsegrest, markinfolst) ->
                  Some(golig, markinfolst, orgsegrest)
            end
      in
      aux liginfolst


    let match_prefix (segorglst : original_glyph_segment list) (mktbl : MarkTable.t) (ligtbl : t) =
      let mainht = ligtbl.entry_table in
      match segorglst with
      | [] ->
          ReachEnd

      | (gobase, gomarks) :: segorgtail ->
          begin
            match gomarks with
            | _ :: _ ->
              (* TODO: should refer to MarkToMark table
                 in order to handle diacritical marks after the first one *)
                begin
                  match attach_marks false mktbl MarkTable.find_base_opt gobase gomarks with
                  | None ->
                    (* If the diacritical marks cannot attach to the base: *)
                      Match(gobase, [], segorgtail)

                  | Some(markpairs) ->
                      Match(gobase, markpairs, segorgtail)
                end

            | [] ->
                begin
                  match GOHt.find_opt mainht gobase with
                  | None ->
                      Match(gobase, [], segorgtail)

                  | Some(liginfolst) ->
                      begin
                        match lookup mktbl liginfolst segorgtail with
                        | None                                   -> Match(gobase, [], segorgtail)
                        | Some((golig, markinfolst, segorgrest)) -> Match(golig, markinfolst, segorgrest)  (* TODO *)
                      end
                end
          end
end




let get_ligature_table ~(file_path : abs_path) (submap : subset_map) (d : D.source) : LigatureTable.t ok =
  let open ResultMonad in
  let inject res =
    res |> Result.map_error (fun e -> FailedToDecodeFont(file_path, e))
  in
  let script_tag = "latn" in  (* TODO: make Script tags changeable *)
  let ligtbl = LigatureTable.create submap 32 (* arbitrary constant; the initial size of the hash table *) in
  let* () =
    inject @@ D.Gsub.get d >>= function
    | None ->
      (* If the font does NOT have a GSUB table: *)
        return ()

    | Some(igsub) ->
        inject @@ D.Gsub.scripts igsub >>= fun scripts ->
        pickup scripts (fun gs -> String.equal (D.Gsub.get_script_tag gs) script_tag) () <| fun script ->
        inject @@ select_gsub_langsys script >>= fun langsys ->
        inject @@ D.Gsub.features langsys >>= fun (_, features) ->
        pickup features (fun gf -> String.equal (D.Gsub.get_feature_tag gf) "liga") () <| fun feature ->
        let* res =
          inject @@ D.Gsub.fold_subtables
            ~lig:(fun res (gid, liginfos) ->
              res >>= fun () ->
              let liginfos =
                liginfos |> List.map (fun (tail, ligature) -> LigatureTable.{ tail; ligature; })
              in
              ligtbl |> LigatureTable.add gid liginfos
            )
            feature (return ())
        in
        res
  in
  return ligtbl


module KerningTable : sig
  type t
  val create : int -> t
  val add : original_glyph_id -> original_glyph_id -> design_units -> t -> unit
  val add_by_class : D.Gpos.class_definition list -> D.Gpos.class_definition list -> (V.class_value * (V.class_value * V.value_record * V.value_record) list) list -> t -> unit
  val find_opt : original_glyph_id -> original_glyph_id -> t -> design_units option
end = struct

    module HtSingle = Hashtbl.Make(struct
      type t = V.glyph_id * V.glyph_id
      let equal = (=)
      let hash = Hashtbl.hash
    end)

    module HtClass = Hashtbl.Make(struct
      type t = V.class_value * V.class_value
      let equal = (=)
      let hash = Hashtbl.hash
    end)

    type subtable = D.Gpos.class_definition list * D.Gpos.class_definition list * design_units HtClass.t

    type t = int HtSingle.t * (subtable list) ref


    let create size =
      let htS = HtSingle.create size in
      let htC = ref [] in
      (htS, htC)


    let add gid1 gid2 wid (htS, _) =
      HtSingle.add htS (gid1, gid2) wid


    let add_by_class clsdeflst1 clsdeflst2 lst (_, refC) =
      let htC = HtClass.create 1024 (* arbitrary constant *) in
      lst |> List.iter (fun (cls1, pairposlst) ->
        pairposlst |> List.iter (fun (cls2, valrcd1, _valrcd2) ->
          match valrcd1.V.x_advance with
          | None      -> ()
          | Some(0)   -> ()
          | Some(xa1) -> HtClass.add htC (cls1, cls2) xa1
        )
      );
      refC := (clsdeflst1, clsdeflst2, htC) :: !refC


    let rec to_class_value gid clsdeflst =
      let iter = to_class_value gid in
        match clsdeflst with
        | []                                        -> None
        | D.Gpos.GlyphToClass(g, c) :: tail           -> if g = gid then Some(c) else iter tail
        | D.Gpos.GlyphRangeToClass(gs, ge, c) :: tail -> if gs <= gid && gid <= ge then Some(c) else iter tail


    let find_opt gid1 gid2 ((htS, refC) : t) =

      let rec find_for_subtables subtbllst =
        match subtbllst with
        | []                        -> None
        | (cdl1, cdl2, htC) :: tail ->
            match (to_class_value gid1 cdl1, to_class_value gid2 cdl2) with
            | (Some(cls1), Some(cls2)) ->
                let kernopt = HtClass.find_opt htC (cls1, cls2) in
                begin
                  match kernopt with
                  | Some(_) -> kernopt
                  | None    -> find_for_subtables tail
                end
            | _ -> find_for_subtables tail
      in

      let kernopt = HtSingle.find_opt htS (gid1, gid2) in
        match kernopt with
        | Some(_) -> kernopt
        | None    -> find_for_subtables (!refC)

  end


let get_kerning_table ~(file_path : abs_path) (d : D.source) =
    let open ResultMonad in
  let script_tag = "latn" in  (* TODO: make Script tags changeable *)
  let kerntbl = KerningTable.create 32 (* arbitrary constant; the initial size of the hash table *) in
  let inject res =
    res |> Result.map_error (fun e -> FailedToDecodeFont(file_path, e))
  in
  let* () =
    inject begin
      D.Kern.get d >>= function
      | None ->
          return ()

      | Some(ikern) ->
          D.Kern.fold (fun () kinfo ->
            match kinfo with
            | D.Kern.{
                horizontal   = true;
                minimum      = false;
                cross_stream = false;
              } ->
                (true, ())

            | _ ->
                (false, ())

          ) (fun () gid1 gid2 wid ->
            kerntbl |> KerningTable.add gid1 gid2 wid
          ) () ikern

    end
  in
  let* () =
    inject @@ D.Gpos.get d >>= function
    | None ->
        return ()

    | Some(igpos) ->
        inject @@ D.Gpos.scripts igpos >>= fun scripts ->
        pickup scripts (fun gs -> String.equal (D.Gpos.get_script_tag gs) script_tag) () <| fun script ->
        inject @@ select_gpos_langsys script >>= fun langsys ->
          (* TODO: make LangSys tags changeable *)
        inject @@ D.Gpos.features langsys >>= fun (_, features) ->
        pickup features (fun gf -> String.equal (D.Gpos.get_feature_tag gf) "kern") () <| fun feature ->
        inject @@ D.Gpos.fold_subtables
          ~pair1:(fun () (gid1, pairposlst) ->
            pairposlst |> List.iter (fun (gid2, valrcd1, _valrcd2) ->
              match valrcd1.Otfed.Value.x_advance with
              | None      -> ()
              | Some(xa1) -> kerntbl |> KerningTable.add gid1 gid2 xa1
            )
          )
          ~pair2:(fun clsdeflst1 clsdeflst2 () sublst ->
            kerntbl |> KerningTable.add_by_class clsdeflst1 clsdeflst2 sublst;
          )
          feature ()
  in
  return kerntbl


type decoder = {
  file_path           : abs_path;
  postscript_name     : string;
  main                : D.source;
  cmap_subtable       : V.Cmap.subtable;
  head_record         : I.Head.t;
  hhea_record         : I.Hhea.t;
  subset_map          : subset_map;
  glyph_id_table      : GlyphIDTable.t;
  glyph_bbox_table    : GlyphBBoxTable.t;
  kerning_table       : KerningTable.t;
  ligature_table      : LigatureTable.t;
  mark_table          : MarkTable.t;
  units_per_em        : int;
  default_ascent      : per_mille;
  default_descent     : per_mille;
}


let postscript_name (dcdr : decoder) : string =
  dcdr.postscript_name


let get_original_gid (_dcdr : decoder) (gid : glyph_id) : original_glyph_id =
  let SubsetGlyphID(gidorg, _) = gid in
  gidorg


let get_ttf_raw_bbox ~(file_path : abs_path) (ttf : D.ttf_source) (gidorg : original_glyph_id) : ((design_units * design_units * design_units * design_units) option) ok =
  let open ResultMonad in
  begin
    D.Ttf.loca ttf gidorg >>= function
    | None ->
        return None

    | Some(gloc) ->
        D.Ttf.glyf ttf gloc >>= fun ttf_glyph_info ->
        let V.{ x_min; y_min; x_max; y_max } = ttf_glyph_info.bounding_box in
        return (Some((x_min, y_min, x_max, y_max)))
  end
    |> Result.map_error (fun e -> FailedToDecodeFont(file_path, e))

let bbox_zero =
  (PerMille(0), PerMille(0), PerMille(0), PerMille(0))


let get_ttf_bbox ~(units_per_em : int) ~(file_path : abs_path) (ttf : D.ttf_source) (gidorg : original_glyph_id) : bbox ok =
  let open ResultMonad in
  let f = per_mille ~units_per_em in
  let* bbox_opt = get_ttf_raw_bbox ~file_path ttf gidorg in
  match bbox_opt with
  | None ->
      return bbox_zero

  | Some(bbox_raw) ->
      let (xmin_raw, ymin_raw, xmax_raw, ymax_raw) = bbox_raw in
      return (f xmin_raw, f ymin_raw, f xmax_raw, f ymax_raw)


let get_glyph_advance_width (dcdr : decoder) (gidorg_key : original_glyph_id) : per_mille ok =
  let open ResultMonad in
  let d = dcdr.main in
  begin
    D.Hmtx.get d >>= fun ihmtx ->
    D.Hmtx.access ihmtx gidorg_key >>= function
    | None            -> return @@ PerMille(0)
    | Some(adv, _lsb) -> return @@ per_mille ~units_per_em:dcdr.units_per_em adv
  end
    |> Result.map_error (fun e -> FailedToDecodeFont(dcdr.file_path, e))


let get_bbox (dcdr : decoder) (gidorg : original_glyph_id) : bbox ok =
  let open ResultMonad in
  let units_per_em = dcdr.units_per_em in
  let abspath = dcdr.file_path in
  match dcdr.main with
  | D.Ttf(ttf) ->
      get_ttf_bbox ~units_per_em ~file_path:abspath ttf gidorg

  | D.Cff(cff) ->
      begin
        D.Cff.charstring cff gidorg >>= function
        | None ->
            return bbox_zero
              (* needs reconsideration; maybe should emit an error *)

        | Some((_width_opt, charstring)) ->
            D.Cff.path_of_charstring charstring >>= fun paths ->
            return begin
              match V.calculate_bounding_box_of_paths paths with
              | None ->
                  bbox_zero

              | Some(bbox_raw) ->
                  let V.{ x_min; y_min; x_max; y_max } = bbox_raw in
                  let f = per_mille ~units_per_em in
                  (f x_min, f y_min, f x_max, f y_max)
            end
      end
        |> Result.map_error (fun e -> FailedToDecodeFont(abspath, e))


let get_glyph_metrics (dcdr : decoder) (gid : glyph_id) : metrics ok =
  let open ResultMonad in
  let bboxtbl = dcdr.glyph_bbox_table in
  let gidorg = get_original_gid dcdr gid in
  let* (wid, (_, ymin, _, ymax)) =
    match bboxtbl |> GlyphBBoxTable.find_opt gidorg with
    | Some(pair) ->
        return pair

    | None ->
        let* wid = get_glyph_advance_width dcdr gidorg in
        let* bbox = get_bbox dcdr gidorg in
        let pair = (wid, bbox) in
        bboxtbl |> GlyphBBoxTable.add gidorg pair;
        return pair
  in
  let hgt = ymax in
  let dpt = ymin in
  return (wid, hgt, dpt)


type 'a resource =
  | Data           of 'a
  | EmbeddedStream of int

type cmap =
  | PredefinedCMap of string
(*
  | CMapFile       of cmap_resource
*)

type font_stretch =
  | UltraCondensedStretch | ExtraCondensedStretch | CondensedStretch | SemiCondensedStetch
  | NormalStretch
  | SemiExpandedStretch | ExpandedStretch | ExtraExpandedStretch | UltraExpandedStretch


let intern_gid (dcdr : decoder) (gidorg : original_glyph_id) : glyph_id ok =
  let open ResultMonad in
  let* gidsub = dcdr.subset_map |> SubsetMap.intern gidorg in
  return @@ SubsetGlyphID(gidorg, gidsub)


let font_stretch_of_width_class = function
  | V.Os2.WidthUltraCondensed -> UltraCondensedStretch
  | V.Os2.WidthExtraCondensed -> ExtraCondensedStretch
  | V.Os2.WidthCondensed      -> CondensedStretch
  | V.Os2.WidthSemiCondensed  -> SemiCondensedStetch
  | V.Os2.WidthMedium         -> NormalStretch
  | V.Os2.WidthSemiExpanded   -> SemiExpandedStretch
  | V.Os2.WidthExpanded       -> ExpandedStretch
  | V.Os2.WidthExtraExpanded  -> ExtraExpandedStretch
  | V.Os2.WidthUltraExpanded  -> UltraExpandedStretch


let font_weight_of_weight_class = function
  | V.Os2.WeightThin       -> 100
  | V.Os2.WeightExtraLight -> 200
  | V.Os2.WeightLight      -> 300
  | V.Os2.WeightNormal     -> 400
  | V.Os2.WeightMedium     -> 500
  | V.Os2.WeightSemiBold   -> 600
  | V.Os2.WeightBold       -> 700
  | V.Os2.WeightExtraBold  -> 800
  | V.Os2.WeightBlack      -> 900


type font_descriptor = {
    font_name    : string;
    font_family  : string;
    font_stretch : font_stretch option;
    font_weight  : int option;  (* Ranges only over {100, 200, ..., 900}. *)
    flags        : int option;  (* TODO: handle this as a boolean record *)
    font_bbox    : bbox;
    italic_angle : float;
    ascent       : per_mille;
    descent      : per_mille;
    stemv        : float;
    font_data    : (D.source resource) ref;
  }


let to_flate_pdf_bytes (data : string) : string * Pdfio.bytes =
  let src_offset_ref = ref 0 in
  let src_len = String.length data in
  let write_byte_as_input buf =
    let src_offset = !src_offset_ref in
    if src_offset >= src_len then
      0
    else begin
      let len = if src_len - src_offset < 1024 then src_len - src_offset else 1024 in
      src_offset_ref := src_offset + len;
      Bytes.blit_string data src_offset buf 0 len;
      len
    end
  in
  let out_offset_ref = ref 0 in
  let bufout = Bytes.create (2 * src_len) in
    (* In the worst case, the output size is 1.003 times as large as the input size. *)
  let write_byte_as_output bufret len =
    let out_offset = !out_offset_ref in
    if len <= 0 then
      ()
    else begin
      out_offset_ref := out_offset + len;
      Bytes.blit bufret 0 bufout out_offset len
    end
  in
  Pdfflate.compress ~level:9 write_byte_as_input write_byte_as_output;
  let out_len = !out_offset_ref in
  let bt = Pdfio.bytes_of_string (String.sub (Bytes.to_string bufout) 0 out_len) in
  ("/FlateDecode", bt)


let subset_tag_id = ref 0

let get_subset_tag () =
  let tag_len = 6 in
  let base_str = Bytes.make tag_len 'A' in
  let rec aux pos cnt =
    if pos = tag_len then
      Bytes.to_string base_str
    else
      let c = Char.chr ((Char.code 'A') + (cnt mod 26)) in
      Bytes.set base_str pos c;
      aux (pos + 1) (cnt / 26)
  in
  incr subset_tag_id;
  aux 0 !subset_tag_id


let add_subset_tag tag_opt fontname =
  match tag_opt with
  | None      -> fontname
  | Some(tag) -> tag ^ "+" ^ fontname


let pdfstream_of_decoder (pdf : Pdf.t) (dcdr : decoder) (subtype_opt : string option) : (Pdf.pdfobject * string option) ok =
  let open ResultMonad in
  let d = dcdr.main in
  let gidorgs = SubsetMap.to_list dcdr.subset_map in
  let* (data, subset_tag) =
    begin
      Otfed.Subset.make ~omit_cmap:true d gidorgs >>= fun data ->
      return (data, Some(get_subset_tag ()))
    end
      |> Result.map_error (fun e -> FailedToMakeSubset(dcdr.file_path, e))
  in
  let (filter, bt) = to_flate_pdf_bytes data in
  let len = Pdfio.bytes_size bt in
  let contents = [
      ("/Length", Pdf.Integer(len));
      ("/Filter", Pdf.Name(filter));
    ]
  in
  let dict =
    match subtype_opt with
    | None          -> contents
    | Some(subtype) -> ("/Subtype", Pdf.Name("/" ^ subtype)) :: contents
  in
  let obj_stream = Pdf.Stream(ref (Pdf.Dictionary(dict), Pdf.Got(bt))) in
  let ir_stream = Pdf.addobj pdf obj_stream in
  return (Pdf.Indirect(ir_stream), subset_tag)


let get_glyph_id_main (cmapsubtbl : V.Cmap.subtable) (uch_key : Uchar.t) : V.glyph_id option =
  V.Cmap.Mapping.fold (fun uch gid acc_opt ->
    match acc_opt with
    | Some(_) -> acc_opt
    | None    -> if Uchar.equal uch uch_key then Some(gid) else None
  ) cmapsubtbl.mapping None


let cmap_predicate f =
  List.find_opt (fun (subtbl, format) -> f (subtbl.V.Cmap.subtable_ids, format))


let get_cmap_subtable ~(file_path : abs_path) (d : D.source) : V.Cmap.subtable ok =
  let open ResultMonad in
  let* opt =
    begin
      D.Cmap.get d >>= fun icmap ->
      D.Cmap.get_subtables icmap >>= fun isubtbls ->
      isubtbls |> mapM (fun isubtbl ->
        let format = D.Cmap.get_format_number isubtbl in
        D.Cmap.unmarshal_subtable isubtbl >>= fun subtbl ->
        return (subtbl, format)
      ) >>= fun subtbls ->
      let opt =
        List.fold_left (fun opt idspred ->
          match opt with
          | Some(_) -> opt
          | None    -> subtbls |> (cmap_predicate idspred)
        ) None [
          (fun (V.Cmap.{ platform_id; _ }, format) -> platform_id = 0 && format = 12);
          (fun (V.Cmap.{ platform_id; _ }, format) -> platform_id = 0 && format = 4);
          (fun (V.Cmap.{ platform_id; _ }, _) -> platform_id = 0);
          (fun (V.Cmap.{ platform_id; encoding_id }, _) -> platform_id = 3 && encoding_id = 10);
          (fun (V.Cmap.{ platform_id; encoding_id }, _) -> platform_id = 3 && encoding_id = 1);
          (fun (V.Cmap.{ platform_id; _ }, _) -> platform_id = 1);
        ]
      in
      return opt
    end
      |> Result.map_error (fun e -> FailedToDecodeFont(file_path, e))
  in
  match opt with
  | None              -> err @@ CannotFindUnicodeCmap(file_path)
  | Some((subtbl, _)) -> return subtbl


let get_glyph_id (dcdr : decoder) (uch : Uchar.t) : (glyph_id option) ok =
  let open ResultMonad in
  let gidtbl = dcdr.glyph_id_table in
  match gidtbl |> GlyphIDTable.find_opt uch with
  | Some(gidpair) ->
      return @@ Some(SubsetGlyphID(gidpair.original_id, gidpair.subset_id))

  | None ->
      match get_glyph_id_main dcdr.cmap_subtable uch with
      | None ->
          return None

      | Some(gidorg) ->
          let* () = gidtbl |> GlyphIDTable.add uch gidorg in
          let* gid = intern_gid dcdr gidorg in
          return @@ Some(gid)


let of_per_mille = function
  | PerMille(x) -> Pdf.Integer(x)


let of_per_mille_opt = function
  | None              -> Pdf.Null
  | Some(PerMille(x)) -> Pdf.Integer(x)


let of_per_mille_pair_opt = function
  | None                             -> Pdf.Null
  | Some((PerMille(a), PerMille(b))) -> Pdf.Array[Pdf.Integer(a); Pdf.Integer(b)]


let add_entry_if_non_null key value dict =
  if value <> Pdf.Null then
    (key, value) :: dict
  else
    dict


let font_descriptor_of_decoder (dcdr : decoder) (font_name : string) : font_descriptor ok =
  let open ResultMonad in
  let d = dcdr.main in
  let ihead = dcdr.head_record in
  let head_derived = ihead.I.Head.derived in
  let ihhea = dcdr.hhea_record in
  let units_per_em = dcdr.units_per_em in
  begin
    D.Os2.get d >>= fun ios2 ->
    let bbox =
      (per_mille ~units_per_em head_derived.x_min,
       per_mille ~units_per_em head_derived.y_min,
       per_mille ~units_per_em head_derived.x_max,
       per_mille ~units_per_em head_derived.y_max)
    in
    return {
      font_name    = font_name; (* PostScript name *)
      font_family  = "";    (* TODO: get this from decoder *)
      font_stretch = Some(font_stretch_of_width_class ios2.I.Os2.value.us_width_class);
      font_weight  = Some(font_weight_of_weight_class ios2.I.Os2.value.us_weight_class);
      flags        = None;  (* TODO: get this from decoder *)
      font_bbox    = bbox;
      italic_angle = 0.;    (* TODO: get this from decoder; 'post.italicAngle' *)
      ascent       = per_mille ~units_per_em ihhea.I.Hhea.value.ascender;
      descent      = per_mille ~units_per_em ihhea.I.Hhea.value.descender;
      stemv        = 0.;    (* TODO: get this from decoder *)
      font_data    = ref (Data(d));
    }
  end
    |> Result.map_error (fun e -> FailedToDecodeFont(dcdr.file_path, e))


let get_postscript_name ~(file_path : abs_path) (d : D.source) : string ok =
  let open ResultMonad in
  let* vname =
    D.Name.get d
      |> Result.map_error (fun e -> FailedToDecodeFont(file_path, e))
  in
  let name_opt =
    vname.V.Name.name_records |> List.find_map (fun name_record ->
      let V.Name.{ platform_id; encoding_id; name_id; name; _ } = name_record in
      if name_id = 6 then
        if platform_id = 0 || platform_id = 1 then
          Some(name)
        else if platform_id = 3 && encoding_id = 1 then
          Some(InternalText.to_utf8 (InternalText.of_utf16be name))
        else
          None
      else
        None
    )
  in
  match name_opt with
  | None ->
      err @@ PostscriptNameNotFound(file_path)

  | Some(name) ->
      return name


(* -w -unused-constructor *)
type[@ocaml.warning "-37"] embedding =
  | FontFile
  | FontFile2
  | FontFile3 of string


let font_file_info_of_embedding embedding =
  match embedding with
  | FontFile       -> ("/FontFile", None)
  | FontFile2      -> ("/FontFile2", None)
  | FontFile3(sub) -> ("/FontFile3", Some(sub))


module CIDFontType0 = struct

  type font = {
    cid_system_info : cid_system_info;
    base_font       : string;
    font_descriptor : font_descriptor;
    dw              : design_units option;
    dw2             : (int * int) option;
  }
    (* Doesn't have to contain information about /W entry;
       the resulting PDF file will be furnished with /W entry when output
       according to the glyph metrics table. *)


  let of_decoder (dcdr : decoder) (cidsysinfo : cid_system_info) : font ok =
    let open ResultMonad in
    let base_font = dcdr.postscript_name in
    let* font_descriptor = font_descriptor_of_decoder dcdr base_font in
    return {
      cid_system_info = cidsysinfo;
      base_font       = base_font;
      font_descriptor = font_descriptor;
      dw              = None;  (* temporary *)
      dw2             = None;  (* temporary *)
    }

end


(* -w -unused-constructor *)
type[@ocaml.warning "-37"] cid_to_gid_map =
  | CIDToGIDIdentity
  | CIDToGIDStream   of (string resource) ref  (* TODO *)


module CIDFontType2 = struct

  type font = {
      cid_system_info  : cid_system_info;
      base_font        : string;
      font_descriptor  : font_descriptor;
      dw               : int option;
      dw2              : (int * int) option;
      cid_to_gid_map   : cid_to_gid_map;
      is_pure_truetype : bool;
    }
    (* Doesn't have to contain information about /W entry;
       the /W entry will be added by using the glyph metrics table when outputting the PDF file. *)


  let of_decoder ~(is_pure_truetype : bool) (dcdr : decoder) (cidsysinfo : cid_system_info) : font ok =
    let open ResultMonad in
    let base_font = dcdr.postscript_name in
    let* font_descriptor = font_descriptor_of_decoder dcdr base_font in
    return {
      cid_system_info  = cidsysinfo;
      base_font        = base_font;
      font_descriptor  = font_descriptor;
      dw               = None;  (* TODO *)
      dw2              = None;  (* TODO *)
      is_pure_truetype = is_pure_truetype;
      cid_to_gid_map   = CIDToGIDIdentity;  (* TODO *)
    }

end


type cid_font =
  | CIDFontType0 of CIDFontType0.font
  | CIDFontType2 of CIDFontType2.font


let pdfobject_of_cmap _pdf cmap =
  match cmap with
  | PredefinedCMap(cmapname) -> Pdf.Name("/" ^ cmapname)
(*
  | CMapFile(res)            -> failwith "cmap file for Type 0 fonts; remains to be implemented."
*)

let pdfobject_of_bbox (PerMille(xmin), PerMille(ymin), PerMille(xmax), PerMille(ymax)) =
  Pdf.Array[Pdf.Integer(xmin); Pdf.Integer(ymin); Pdf.Integer(xmax); Pdf.Integer(ymax)]


module ToUnicodeCMap : sig
  type t
  val create : unit -> t
  val add_single : t -> subset_glyph_id -> Uchar.t list -> unit
  val stringify : t -> string
end = struct

  type t = ((Uchar.t list) GSHt.t) array


  let create () =
    Array.init 1024 (fun _ -> GSHt.create 32)


  let add_single touccmap gid uchlst =
    let i = match gid with SubsetNumber(n) -> n / 64 in
    GSHt.add (touccmap.(i)) gid uchlst


  let stringify touccmap =
    let prefix =
        "/CIDInit/ProcSet findresource begin "
      ^ "12 dict begin begincmap/CIDSystemInfo<<"
      ^ "/Registry(Adobe)/Ordering(UCS)/Supplement 0>> def"
      ^ "/CMapName/Adobe-Identity-UCS def/CMapType 2 def "
      ^ "1 begincodespacerange<0000><FFFF>endcodespacerange "
    in
    let postfix =
      "endcmap CMapName currentdict/CMap defineresource pop end end"
    in
    let buf = Buffer.create ((15 + (6 + 512) * 64 + 10) * 1024) in
    Array.iter (fun ht ->
      let num = GSHt.length ht in
      if num <= 0 then
        ()
      else
        begin
          Printf.bprintf buf "%d beginbfchar" num;
          GSHt.iter (fun (SubsetNumber(n)) uchlst ->
            let dst = (InternalText.to_utf16be_hex (InternalText.of_uchar_list uchlst)) in
            Printf.bprintf buf "<%04X><%s>" n dst
          ) ht;
          Printf.bprintf buf "endbfchar ";
        end
    ) touccmap;
    let strmain = Buffer.contents buf in
    let res = prefix ^ strmain ^ postfix in
    res

end


module Type0 = struct
  type font = {
    base_font        : string;
    encoding         : cmap;
    descendant_fonts : cid_font;  (* Represented as a singleton list in PDF. *)
  }


  let of_cid_font cidfont fontname cmap =
    {
      base_font        = fontname;
      encoding         = cmap;
      descendant_fonts = cidfont;
    }


  let pdfobject_of_font_descriptor (pdf : Pdf.t) (dcdr : decoder) fontdescr base_font embedding : (Pdf.pdfobject * string option) ok =
    let open ResultMonad in
    let (font_file_key, tagopt) = font_file_info_of_embedding embedding in
    let* (objstream, subset_tag_opt) = pdfstream_of_decoder pdf dcdr tagopt in
      (* Adds to the PDF the stream in which the font file is embedded. *)
    let obj_descr =
      Pdf.Dictionary[
        ("/Type"       , Pdf.Name("/FontDescriptor"));
        ("/FontName"   , Pdf.Name("/" ^ (add_subset_tag subset_tag_opt base_font)));
        ("/Flags"      , Pdf.Integer(4));  (* TODO: make this changeable *)
        ("/FontBBox"   , pdfobject_of_bbox fontdescr.font_bbox);
        ("/ItalicAngle", Pdf.Real(fontdescr.italic_angle));
        ("/Ascent"     , of_per_mille fontdescr.ascent);
        ("/Descent"    , of_per_mille fontdescr.descent);
        ("/StemV"      , Pdf.Real(fontdescr.stemv));
        (font_file_key , objstream);
      ]
    in
    let ir_descr = Pdf.addobj pdf obj_descr in
    return (Pdf.Indirect(ir_descr), subset_tag_opt)


  let pdfdict_of_cid_system_info cidsysinfo =
    Pdf.Dictionary[
      ("/Registry"  , Pdf.String(cidsysinfo.registry));
      ("/Ordering"  , Pdf.String(cidsysinfo.ordering));
      ("/Supplement", Pdf.Integer(cidsysinfo.supplement));
    ]


  let pdfobject_of_width_array (pdf : Pdf.t) (dcdr : decoder) : Pdf.pdfobject ok =
    let open ResultMonad in
    let bboxtbl = dcdr.glyph_bbox_table in
    let* arr =
      bboxtbl |> GlyphBBoxTable.fold (fun gidorg (PerMille(w), _) res ->
        let* acc = res in
        let* SubsetNumber(n) = dcdr.subset_map |> SubsetMap.intern gidorg in
        return (Pdf.Integer(n) :: Pdf.Array[Pdf.Integer(w)] :: acc)
      ) (return [])
    in
    let obj = Pdf.Array(arr) in
    let ir = Pdf.addobj pdf obj in
    return @@ Pdf.Indirect(ir)


  let pdfobject_of_to_unicode_cmap (pdf : Pdf.t) (dcdr : decoder) : Pdf.pdfobject =
    let gidtbl = dcdr.glyph_id_table in
    let ligtbl = dcdr.ligature_table in
    let touccmap = ToUnicodeCMap.create () in

    gidtbl |> GlyphIDTable.fold_rev (fun gidsub uch () ->
      ToUnicodeCMap.add_single touccmap gidsub [uch]
    ) ();

    ligtbl |> LigatureTable.fold_rev (fun gidlig gidlst () ->
      try
        let uchlst =
          gidlst |> List.map (fun gidorg ->
            match gidtbl |> GlyphIDTable.find_rev_opt gidorg with
            | None      -> raise Exit
            | Some(uch) -> uch
          )
        in
(*
        let pp_uchar_list fmt uchlst = Format.fprintf fmt "%s" (InternalText.to_utf8 (InternalText.of_uchar_list uchlst)) in  (* for debug *)
        let () = Format.printf "FontFormat> add ligature GID %04X -> [%a](debug)\n" gidlig pp_uchar_list uchlst in  (* for debug *)
*)
        ToUnicodeCMap.add_single touccmap gidlig uchlst
      with
      | Exit -> ()
    ) ();

    let str = ToUnicodeCMap.stringify touccmap in
    let iobytes = Pdfio.bytes_of_string str in
    let stream = Pdf.Got(iobytes) in
    let len = Pdfio.bytes_size iobytes in
    let objstream = Pdf.Stream(ref (Pdf.Dictionary[("/Length", Pdf.Integer(len))], stream)) in

    Pdfcodec.encode_pdfstream pdf Pdfcodec.Flate objstream;

    let ir = Pdf.addobj pdf objstream in
    Pdf.Indirect(ir)


  (* Returns a descendant font dictionary of Type 0 CIDFont as an indirect reference. *)
  let pdfobject_of_cid_type_0 pdf cidty0font dcdr : (Pdf.pdfobject * string option) ok =
    let open ResultMonad in
    let units_per_em = dcdr.units_per_em in
    let cidsysinfo = cidty0font.CIDFontType0.cid_system_info in
    let base_font  = cidty0font.CIDFontType0.base_font in
    let fontdescr  = cidty0font.CIDFontType0.font_descriptor in
    let* (obj_descr, subset_tag_opt) = pdfobject_of_font_descriptor pdf dcdr fontdescr base_font (FontFile3("OpenType")) in
    let* obj_warr = pdfobject_of_width_array pdf dcdr in
    let pmoptdw =
      cidty0font.CIDFontType0.dw |> Option.map (per_mille ~units_per_em)
    in
    let pmpairoptdw2 =
      cidty0font.CIDFontType0.dw2 |> Option.map (fun (a, b) -> (per_mille ~units_per_em a, per_mille ~units_per_em b))
    in
    let obj_descend =
      Pdf.Dictionary([
        ("/Type"          , Pdf.Name("/Font"));
        ("/Subtype"       , Pdf.Name("/CIDFontType0"));
        ("/BaseFont"      , Pdf.Name("/" ^ (add_subset_tag subset_tag_opt base_font)));
        ("/CIDSystemInfo" , pdfdict_of_cid_system_info cidsysinfo);
        ("/FontDescriptor", obj_descr);
        ("/W"             , obj_warr);
      ] |> add_entry_if_non_null "/DW"  (of_per_mille_opt pmoptdw)
        |> add_entry_if_non_null "/DW2" (of_per_mille_pair_opt pmpairoptdw2))
    in
    let ir_descend = Pdf.addobj pdf obj_descend in
    return (Pdf.Indirect(ir_descend), subset_tag_opt)


  (* Returns a descendant font dictionary of Type 2 CIDFont as an indirect reference. *)
  let pdfobject_of_cid_type_2 (pdf : Pdf.t) cidty2font (dcdr : decoder) : (Pdf.pdfobject * string option) ok =
    let open ResultMonad in
    let units_per_em = dcdr.units_per_em in
    let cidsysinfo = cidty2font.CIDFontType2.cid_system_info in
    let base_font  = cidty2font.CIDFontType2.base_font in
    let fontdescr  = cidty2font.CIDFontType2.font_descriptor in
    let font_file =
    (* Probably such conditional branching is not appropriate; should always choose true-branch *)
      if cidty2font.CIDFontType2.is_pure_truetype then
        FontFile2
      else
        FontFile3("OpenType")
    in
    let* (obj_descr, subset_tag_opt) = pdfobject_of_font_descriptor pdf dcdr fontdescr base_font font_file in
    let obj_cid_to_gid_map =
      match cidty2font.CIDFontType2.cid_to_gid_map with
      | CIDToGIDIdentity -> Pdf.Name("/Identity")
      | _                -> remains_to_be_implemented "/CIDToGIDMap other than /Identity"
    in
    let dwpm_opt =
      cidty2font.CIDFontType2.dw |> Option.map (fun dw -> per_mille ~units_per_em dw)
    in  (* Per mille *)
    let dw2pmpair_opt =
      cidty2font.CIDFontType2.dw2 |> Option.map (fun (a, b) ->
        (per_mille ~units_per_em a, per_mille ~units_per_em b)
      )
    in
    let* obj_warr = pdfobject_of_width_array pdf dcdr in
    let obj_descend =
      Pdf.Dictionary([
        ("/Type"          , Pdf.Name("/Font"));
        ("/Subtype"       , Pdf.Name("/CIDFontType2"));
        ("/BaseFont"      , Pdf.Name("/" ^ (add_subset_tag subset_tag_opt base_font)));
        ("/CIDSystemInfo" , pdfdict_of_cid_system_info cidsysinfo);
        ("/FontDescriptor", obj_descr);
        ("/W"             , obj_warr);
        ("/CIDToGIDMap"   , obj_cid_to_gid_map);
          (* should add more; /W2 *)
      ] |> add_entry_if_non_null "/DW"  (of_per_mille_opt dwpm_opt)
        |> add_entry_if_non_null "/DW2" (of_per_mille_pair_opt dw2pmpair_opt))
    in
    let ir_descend = Pdf.addobj pdf obj_descend in
    return (Pdf.Indirect(ir_descend), subset_tag_opt)


  let to_pdfdict (pdf : Pdf.t) ty0font (dcdr : decoder) : Pdf.pdfobject ok =
    let open ResultMonad in
    let cidfont       = ty0font.descendant_fonts in
    let base_font_ty0 = ty0font.base_font in
    let cmap          = ty0font.encoding in
    let* (objdescend, subset_tag_opt) =
      match cidfont with
      | CIDFontType0(cidty0font) -> pdfobject_of_cid_type_0 pdf cidty0font dcdr
      | CIDFontType2(cidty2font) -> pdfobject_of_cid_type_2 pdf cidty2font dcdr
    in
    let pdfobjtouc = pdfobject_of_to_unicode_cmap pdf dcdr in
    return @@ Pdf.Dictionary[
      ("/Type"           , Pdf.Name("/Font"));
      ("/Subtype"        , Pdf.Name("/Type0"));
      ("/Encoding"       , pdfobject_of_cmap pdf cmap);
      ("/BaseFont"       , Pdf.Name("/" ^ (add_subset_tag subset_tag_opt base_font_ty0)));  (* Can be arbitrary name. *)
      ("/DescendantFonts", Pdf.Array[objdescend]);
      ("/ToUnicode"      , pdfobjtouc);
    ]

end


type font =
  | Type0 of Type0.font


let make_dictionary (pdf : Pdf.t) (font : font) (dcdr : decoder) : Pdf.pdfobject ok =
  match font with
  | Type0(ty0font) -> Type0.to_pdfdict pdf ty0font dcdr


let make_decoder (abspath : abs_path) (d : D.source) : decoder ok =
  let open ResultMonad in
  let* cmapsubtbl = get_cmap_subtable ~file_path:abspath d in
  let submap = SubsetMap.create abspath d 32 in  (* temporary; initial size of hash tables *)
  let gidtbl = GlyphIDTable.create submap 256 in  (* temporary; initial size of hash tables *)
  let bboxtbl = GlyphBBoxTable.create 256 in  (* temporary; initial size of hash tables *)
  let* (rcdhhea, ascent, descent) =
    begin
      let* ihhea = D.Hhea.get d in
      return (ihhea, ihhea.value.ascender, ihhea.value.descender)
    end
      |> Result.map_error (fun e -> FailedToDecodeFont(abspath, e))
  in
  let* (rcdhead, units_per_em) =
    begin
      let* ihead = D.Head.get d in
      return (ihead, ihead.value.units_per_em)
    end
      |> Result.map_error (fun e -> FailedToDecodeFont(abspath, e))
  in
  let* postscript_name = get_postscript_name ~file_path:abspath d in
  let* kerntbl = get_kerning_table ~file_path:abspath d in
  let* ligtbl = get_ligature_table ~file_path:abspath submap d in
  let* mktbl = get_mark_table ~file_path:abspath ~units_per_em d in
  return {
    file_path           = abspath;
    postscript_name     = postscript_name;
    main                = d;
    cmap_subtable       = cmapsubtbl;
    head_record         = rcdhead;
    hhea_record         = rcdhhea;
    kerning_table       = kerntbl;
    ligature_table      = ligtbl;
    mark_table          = mktbl;
    subset_map          = submap;
    glyph_id_table      = gidtbl;
    glyph_bbox_table    = bboxtbl;
    units_per_em        = units_per_em;
    default_ascent      = per_mille ~units_per_em ascent;
    default_descent     = per_mille ~units_per_em descent;
  }


let cid_font_type_0 cidty0font fontname cmap =
  Type0(Type0.of_cid_font (CIDFontType0(cidty0font)) fontname cmap)


let cid_font_type_2 cidty2font fontname cmap =
  Type0(Type0.of_cid_font (CIDFontType2(cidty2font)) fontname cmap)


let get_font (dcdr : decoder) (fontreg : font_registration) : font ok =
  let open ResultMonad in
  let cmap = PredefinedCMap("Identity-H") in
  match fontreg with
  | CIDFontType0Registration(cidsysinfo, _embedW) ->
      let* cidty0font = CIDFontType0.of_decoder dcdr cidsysinfo in
      return (cid_font_type_0 cidty0font dcdr.postscript_name cmap)

  | CIDFontType2OTRegistration(cidsysinfo, _embedW) ->
      let is_pure_truetype = true in (* TODO: fix this *)
      let* cidty2font = CIDFontType2.of_decoder ~is_pure_truetype dcdr cidsysinfo in
      return (cid_font_type_2 cidty2font dcdr.postscript_name cmap)


let get_decoder_single (abspath : abs_path) : (decoder * font) ok =
  let open ResultMonad in
  let* (d, fontreg) = get_main_decoder_single abspath in
  let* dcdr = make_decoder abspath d in
  let* font = get_font dcdr fontreg in
  return (dcdr, font)


let get_decoder_ttc (abspath :abs_path) (index : int) : (decoder * font) ok =
  let open ResultMonad in
  let* (d, fontreg) = get_main_decoder_ttc abspath index in
  let* dcdr = make_decoder abspath d in
  let* font = get_font dcdr fontreg in
  return (dcdr, font)


let convert_to_ligatures (dcdr : decoder) (seglst : glyph_segment list) : (glyph_synthesis list) ok =
  let open ResultMonad in
  let ligtbl = dcdr.ligature_table in
  let mktbl = dcdr.mark_table in
  let intf = intern_gid dcdr in
(*
  let intsegf (gobase, gomarks) = (intf gobase, List.map intf gomarks) in
*)
  let orgf = get_original_gid dcdr in
  let orgsegf (base, marks) = (orgf base, List.map orgf marks) in

  let rec aux acc segorglst =
    match ligtbl |> LigatureTable.match_prefix segorglst mktbl with
    | ReachEnd ->
        return @@ Alist.to_list acc

    | Match(gidorg_lig, markorginfolst, segorgrest) ->
        let* markinfolst =
          markorginfolst |> mapM (fun (gidorg, v) ->
            let* gid = intf gidorg in
            let* (w, _, _) = get_glyph_metrics dcdr gid in
            return @@ Mark(gid, w, v)
          )
        in
        let* gid_lig = intf gidorg_lig in
        aux (Alist.extend acc (gid_lig, markinfolst)) segorgrest
  in
  let segorglst = seglst |> List.map orgsegf in
  aux Alist.empty segorglst


let find_kerning (dcdr : decoder) (gidprev : glyph_id) (gid : glyph_id) : per_mille option =
  let kerntbl = dcdr.kerning_table in
  let gidorgprev = get_original_gid dcdr gidprev in
  let gidorg = get_original_gid dcdr gid in
  let open OptionMonad in
  kerntbl |> KerningTable.find_opt gidorgprev gidorg >>= fun du ->
  return (per_mille ~units_per_em:dcdr.units_per_em du)


module MathInfoMap = Map.Make
  (struct
    type t = original_glyph_id
    let compare = Stdlib.compare
  end)


type math_kern = (design_units * design_units) list * design_units

type math_kern_info =
  {
    kernTR : math_kern;
    kernTL : math_kern;
    kernBR : math_kern;
    kernBL : math_kern;
  }

type math_variant_glyph = original_glyph_id * design_units

type math_decoder =
  {
    as_normal_font             : decoder;
    math_constants             : V.Math.math_constants;
    math_italics_correction    : per_mille MathInfoMap.t;
    math_top_accent_attachment : per_mille MathInfoMap.t;
    math_vertical_variants     : (math_variant_glyph list) MathInfoMap.t;
    math_horizontal_variants   : (math_variant_glyph list) MathInfoMap.t;
    math_kern_info             : math_kern_info MathInfoMap.t;
    script_style_info          : D.Gsub.feature option;
  }


let math_base_font (md : math_decoder) : decoder =
  md.as_normal_font


let get_main_math_value ((x, _) : V.Math.math_value_record) = x


let percent n =
  (float_of_int n) /. 100.


let to_design_units (md : math_decoder) (ratio : float) : design_units =
  let upem = md.as_normal_font.units_per_em in
    int_of_float (ratio *. (float_of_int upem))


let to_ratio (md : math_decoder) (du : design_units) : float =
  let upem = md.as_normal_font.units_per_em in
    (float_of_int du) /. (float_of_int upem)


let convert_kern (mkopt : V.Math.math_kern option) : math_kern =
  let f = get_main_math_value in
  let rec aux acc mk =
    match mk with
    | ([], kernlast :: [])                       -> (Alist.to_list acc, f kernlast)
    | (hgthead :: hgttail, kernhead :: kerntail) -> aux (Alist.extend acc (f hgthead, f kernhead)) (hgttail, kerntail)
    | _                                          -> assert false  (* TODO: report error *)
  in
  match mkopt with
  | None     -> ([], 0)
  | Some(mk) -> aux Alist.empty mk


let convert_kern_info (mkir : V.Math.math_kern_info_record) =
  {
    kernTR = convert_kern mkir.V.Math.top_right_math_kern;
    kernTL = convert_kern mkir.V.Math.top_left_math_kern;
    kernBR = convert_kern mkir.V.Math.bottom_right_math_kern;
    kernBL = convert_kern mkir.V.Math.bottom_left_math_kern;
  }


let assoc_to_map f gidassoc =
  gidassoc |> List.fold_left (fun map (gid, data) ->
    map |> MathInfoMap.add gid (f data)
  ) MathInfoMap.empty


let make_math_decoder_from_decoder (abspath : abs_path) (dcdr : decoder) (font : font) : (math_decoder * font) ok =
  let open ResultMonad in
  let inject res =
    res |> Result.map_error (fun e -> FailedToDecodeFont(abspath, e))
  in
  let units_per_em = dcdr.units_per_em in
  let d = dcdr.main in
  let* mathraw_opt =
    D.Math.get d
      |> Result.map_error (fun e -> FailedToDecodeFont(abspath, e))
  in
  match mathraw_opt with
  | None ->
      err @@ NoMathTable(abspath)

  | Some(mathraw) ->
      let micmap =
        mathraw.V.Math.math_glyph_info.V.Math.math_italics_correction
          |> assoc_to_map (fun v -> per_mille ~units_per_em (get_main_math_value v))
      in
      let mkimap =
        mathraw.V.Math.math_glyph_info.V.Math.math_kern_info
          |> assoc_to_map convert_kern_info
      in
      let mvertvarmap =
        mathraw.V.Math.math_variants.V.Math.vert_glyph_assoc
          |> assoc_to_map (fun mgconstr ->
            mgconstr.V.Math.math_glyph_variant_record_list |> List.map (function
            | V.Math.{ variant_glyph; advance_measurement } ->
                (variant_glyph, advance_measurement)
            )
          )
      in
      let mhorzvarmap =
        mathraw.V.Math.math_variants.V.Math.horiz_glyph_assoc
          |> assoc_to_map (fun mgconstr ->
            mgconstr.V.Math.math_glyph_variant_record_list |> List.map (function
            | V.Math.{ variant_glyph; advance_measurement } ->
                (variant_glyph, advance_measurement)
            )
          )
      in
      let* ssty_opt =
        inject @@ D.Gsub.get d >>= function
        | None ->
            return None

        | Some(igsub) ->
            inject @@ D.Gsub.scripts igsub >>= fun scripts ->
            pickup scripts (fun gs -> String.equal (D.Gsub.get_script_tag gs) "math") None <| fun script_math ->
            inject @@ select_gsub_langsys script_math >>= fun langsys ->
            inject @@ D.Gsub.features langsys >>= fun (_, features) ->
            pickup features (fun gf -> String.equal (D.Gsub.get_feature_tag gf) "ssty") None <| fun ssty ->
            return @@ Some(ssty)
      in
      let md =
        {
          as_normal_font             = dcdr;
          math_constants             = mathraw.V.Math.math_constants;
          math_italics_correction    = micmap;
          math_top_accent_attachment = MathInfoMap.empty;  (* TODO *)
          math_vertical_variants     = mvertvarmap;
          math_horizontal_variants   = mhorzvarmap;
          math_kern_info             = mkimap;
          script_style_info          = ssty_opt;
        }
      in
      return (md, font)


let get_math_decoder_single (abspath : abs_path) : (math_decoder * font) ok =
  let open ResultMonad in
  let* (dcdr, font) = get_decoder_single abspath in
  make_math_decoder_from_decoder abspath dcdr font


let get_math_decoder_ttc (abspath : abs_path) (index : int) : (math_decoder * font) ok =
  let open ResultMonad in
  let* (dcdr, font) = get_decoder_ttc abspath index in
  make_math_decoder_from_decoder abspath dcdr font


let get_math_script_variant (md : math_decoder) (gid : glyph_id) : glyph_id ok =
  let open ResultMonad in
  match md.script_style_info with
  | None ->
    (* If the font does NOT have 'ssty' feature table: *)
      return gid

  | Some(feature_ssty) ->
      let dcdr = md.as_normal_font in
      let abspath = dcdr.file_path in
      let gidorg = get_original_gid dcdr gid in
      let f_single opt (gidorgfrom, gidorgto) =
        match opt with
        | Some(_) -> opt
        | None    -> if gidorgfrom = gidorg then Some(gidorgto) else opt
      in
      let f_alt opt (gidorgfrom, gidorglst) =
        match (opt, gidorglst) with
        | (Some(_), _)          -> opt
        | (None, [])            -> opt
        | (None, gidorgto :: _) -> if gidorgfrom = gidorg then Some(gidorgto) else opt
      in
      let* gidorg_ssty_opt =
        D.Gsub.fold_subtables ~single:f_single ~alt:f_alt feature_ssty None
          |> Result.map_error (fun e -> FailedToDecodeFont(abspath, e))
      in
      match gidorg_ssty_opt with
      | None              -> return gid
      | Some(gidorg_ssty) -> intern_gid dcdr gidorg_ssty


let get_math_glyph_id (md : math_decoder) (uch : Uchar.t) : (glyph_id option) ok =
  let dcdr = md.as_normal_font in
  get_glyph_id dcdr uch


let truncate_negative (PerMille(x)) =
  PerMille(max 0 x)


let truncate_positive (PerMille(x)) =
  PerMille(min 0 x)


let get_math_glyph_metrics (md : math_decoder) (gid : glyph_id) : metrics ok =
  let open ResultMonad in
  let dcdr = md.as_normal_font in
  let* (wid, _, _) = get_glyph_metrics dcdr gid in
  let gidorg = get_original_gid dcdr gid in
  let* (_, ymin, _, ymax) = get_bbox md.as_normal_font gidorg in
  let hgt = truncate_negative ymax in
  let dpt = truncate_positive ymin in
  return (wid, hgt, dpt)


let get_math_correction_metrics (md : math_decoder) (gid : glyph_id) : per_mille option * math_kern_info option =
  let gidorg = get_original_gid md.as_normal_font gid in
  let micopt = md.math_italics_correction |> MathInfoMap.find_opt gidorg in
  let mkiopt = md.math_kern_info |> MathInfoMap.find_opt gidorg in
  (micopt, mkiopt)


let get_math_variants (md : math_decoder) (gid : glyph_id) (map : (math_variant_glyph list) MathInfoMap.t) : ((glyph_id * float) list) ok =
  let open ResultMonad in
  let dcdr = md.as_normal_font in
  let gidorg = get_original_gid dcdr gid in
  match map |> MathInfoMap.find_opt gidorg with
  | None ->
      return []

  | Some(assoc) ->
      assoc |> mapM (fun (gidorg, du) ->
        let* gid = intern_gid dcdr gidorg in
        return (gid, to_ratio md du)
      )


let get_math_vertical_variants (md : math_decoder) (gid : glyph_id) : ((glyph_id * float) list) ok =
  let mvertvarmap = md.math_vertical_variants in
  mvertvarmap |> get_math_variants md gid


let get_math_horizontal_variants (md : math_decoder) (gid : glyph_id) =
  let mhorzvarmap = md.math_horizontal_variants in
  mhorzvarmap |> get_math_variants md gid


type math_constants = {
(* General: *)
  axis_height                   : float;
(* Sub/superscripts: *)
  superscript_bottom_min        : float;
  superscript_shift_up          : float;
  superscript_baseline_drop_max : float;
  subscript_top_max             : float;
  subscript_shift_down          : float;
  subscript_baseline_drop_min   : float;
  script_scale_down             : float;
  script_script_scale_down      : float;
  space_after_script            : float;
  sub_superscript_gap_min       : float;
(* Fractions: *)
  fraction_rule_thickness       : float;
  fraction_numer_d_shift_up     : float;
  fraction_numer_d_gap_min      : float;
  fraction_denom_d_shift_down   : float;
  fraction_denom_d_gap_min      : float;
(* Radicals: *)
  radical_extra_ascender        : float;
  radical_rule_thickness        : float;
  radical_d_vertical_gap        : float;
(* Limits: *)
  upper_limit_gap_min           : float;
  upper_limit_baseline_rise_min : float;
  lower_limit_gap_min           : float;
  lower_limit_baseline_drop_min : float;
}


let get_main_ratio (md : math_decoder) (mvr : V.Math.math_value_record) : float =
  to_ratio md (get_main_math_value mvr)


let get_axis_height_ratio (md : math_decoder) : float =
  let mc = md.math_constants in
  get_main_ratio md mc.V.Math.axis_height


let get_math_constants (md : math_decoder) : math_constants =
  let mc = md.math_constants in
  let f = get_main_ratio md in
  {
    axis_height                   = f mc.V.Math.axis_height;

    superscript_bottom_min        = f mc.V.Math.superscript_bottom_min;
    superscript_shift_up          = f mc.V.Math.superscript_shift_up;
    superscript_baseline_drop_max = f mc.V.Math.superscript_baseline_drop_max;
    subscript_top_max             = f mc.V.Math.subscript_top_max;
    subscript_shift_down          = f mc.V.Math.subscript_shift_down;
    subscript_baseline_drop_min   = f mc.V.Math.subscript_baseline_drop_min;
    script_scale_down             = percent mc.V.Math.script_percent_scale_down;
    script_script_scale_down      = percent mc.V.Math.script_script_percent_scale_down;
    space_after_script            = f mc.V.Math.space_after_script;
    sub_superscript_gap_min       = f mc.V.Math.sub_superscript_gap_min;

    fraction_rule_thickness       = f mc.V.Math.fraction_rule_thickness;
    fraction_numer_d_shift_up     = f mc.V.Math.fraction_numerator_display_style_shift_up;
    fraction_numer_d_gap_min      = f mc.V.Math.fraction_num_display_style_gap_min;
    fraction_denom_d_shift_down   = f mc.V.Math.fraction_denominator_display_style_shift_down;
    fraction_denom_d_gap_min      = f mc.V.Math.fraction_denom_display_style_gap_min;

    radical_extra_ascender        = f mc.V.Math.radical_extra_ascender;
    radical_rule_thickness        = f mc.V.Math.radical_rule_thickness;
    radical_d_vertical_gap        = f mc.V.Math.radical_display_style_vertical_gap;

    upper_limit_gap_min           = f mc.V.Math.upper_limit_gap_min;
    upper_limit_baseline_rise_min = f mc.V.Math.upper_limit_baseline_rise_min;
    lower_limit_gap_min           = f mc.V.Math.lower_limit_gap_min;
    lower_limit_baseline_drop_min = f mc.V.Math.lower_limit_baseline_drop_min;
  }


let find_kern_ratio (md : math_decoder) (mkern : math_kern) (ratio : float) =
  let du = to_design_units md ratio in
  let (kernlst, kfinal) = mkern in
  let rec aux prevc kernlst =
    match kernlst with
    | []             -> to_ratio md kfinal
    | (c, k) :: tail -> if prevc <= du && du < c then to_ratio md k else aux c tail
  in
  aux 0 kernlst
