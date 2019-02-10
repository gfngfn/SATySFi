
open MyUtil


type original_glyph_id = Otfm.glyph_id

type 'a ok = ('a, Otfm.error) result

type design_units = int

type per_mille =
  | PerMille of int

type metrics = per_mille * per_mille * per_mille

type indirect = int

exception FailToLoadFontOwingToSystem of abs_path * string
exception BrokenFont                  of abs_path * string
exception CannotFindUnicodeCmap       of abs_path


let broken srcpath oerr s =
  let msg = Format.asprintf "%a" Otfm.pp_error oerr in
  raise (BrokenFont(srcpath, msg ^ "; " ^ s))


type cid_system_info = {
    registry   : string;
    ordering   : string;
    supplement : int;
  }


let adobe_identity = { registry = "Adobe"; ordering = "Identity"; supplement = 0; }


type font_registration =
  | CIDFontType0Registration   of cid_system_info * bool
      (* -- last boolean: true iff it should embed /W information -- *)
  | CIDFontType2OTRegistration of cid_system_info * bool
      (* -- last boolean: true iff it should embed /W information -- *)
  | CIDFontType2TTRegistration of cid_system_info * bool
      (* -- last boolean: true iff it should embed /W information -- *)


let extract_registration d =
  let open ResultMonad in
    Otfm.flavour d >>= function
    | Otfm.CFF ->
        Otfm.cff d >>= fun cffinfo ->
        let cidsysinfo =
          match cffinfo.Otfm.cid_info with
          | None ->
            (* -- if not a CID-keyed font -- *)
              adobe_identity

          | Some(cidinfo) ->
              {
                registry   = cidinfo.Otfm.registry;
                ordering   = cidinfo.Otfm.ordering;
                supplement = cidinfo.Otfm.supplement;
              }
        in
        return (d, CIDFontType0Registration(cidsysinfo, true))

    | Otfm.TTF_OT ->
        return (d, CIDFontType2OTRegistration(adobe_identity, true))

    | Otfm.TTF_true ->
        return (d, CIDFontType2TTRegistration(adobe_identity, true))


let get_main_decoder_single (abspath : abs_path) : ((Otfm.decoder * font_registration) option) ok =
  match string_of_file abspath with
  | Ok(s) ->
      let open ResultMonad in
      begin
        Otfm.decoder (`String(s)) >>= function
        | Otfm.TrueTypeCollection(_) -> return None
        | Otfm.SingleDecoder(d)      -> extract_registration d >>= fun pair -> return (Some(pair))
      end

  | Error(msg) ->
      raise (FailToLoadFontOwingToSystem(abspath, msg))


let get_main_decoder_ttc (abspath : abs_path) (i : int) : ((Otfm.decoder * font_registration) option) ok =
  match string_of_file abspath with
  | Ok(s) ->
      let open ResultMonad in
      begin
        Otfm.decoder (`String(s)) >>= function
        | Otfm.SingleDecoder(_) ->
            return None

        | Otfm.TrueTypeCollection(ttc) ->
            begin
              match List.nth_opt ttc i with
              | None ->
                  return None

              | Some(ttcelem) ->
                  Otfm.decoder_of_ttc_element ttcelem >>= fun d ->
                  extract_registration d >>= fun pair ->
                  return (Some(pair))
            end
      end

  | Error(msg) ->
      raise (FailToLoadFontOwingToSystem(abspath, msg))


module UHt = Hashtbl.Make
  (struct
    type t = Uchar.t
    let equal = (=)
    let hash = Hashtbl.hash
  end)


module GOHt = Hashtbl.Make
  (struct
    type t = original_glyph_id
    let equal = (=)
    let hash = Hashtbl.hash
  end)

type subset_glyph_id = SubsetNumber of Otfm.glyph_id

module GSHt = Hashtbl.Make
  (struct
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


module SubsetMap
: sig
    type t
    val create : int -> t
    val create_dummy : unit -> t
    val intern : original_glyph_id -> t -> subset_glyph_id
    val to_list : t -> (original_glyph_id list) option
  end
= struct

    type subset = {
      original_to_subset : subset_glyph_id GOHt.t;
      subset_to_original : original_glyph_id GSHt.t;
      count : int ref;
      store : (original_glyph_id Alist.t) ref;
    }

    type t =
      | Subset of subset
      | Dummy


    let create n =
      let ht = GOHt.create n in
      let revht = GSHt.create n in
      GOHt.add ht 0 (SubsetNumber(0));
      Subset{
        original_to_subset = ht;
        subset_to_original = revht;
        count = ref 0;
        store = ref (Alist.extend Alist.empty 0);
      }


    let create_dummy () =
      Dummy


    let intern gidorg submap =
      match submap with
      | Dummy ->
          SubsetNumber(gidorg)

      | Subset(r) ->
          let ht = r.original_to_subset in
          let revht = r.subset_to_original in
          let count = r.count in
          let store = r.store in
          begin
            match GOHt.find_opt ht gidorg with
            | Some(gidsub) ->
                gidsub

            | None ->
                incr count;
                let gidsub = SubsetNumber(!count) in
                GOHt.add ht gidorg gidsub;
                GSHt.add revht gidsub gidorg;
                let alst = Alist.extend (!store) gidorg in
                store := alst;
                gidsub
          end

    let to_list submap =
      match submap with
      | Subset(r) -> Some(Alist.to_list !(r.store))
      | Dummy     -> None

  end


type subset_map = SubsetMap.t


module GlyphIDTable
: sig
    type t
    val create : subset_map -> int -> t
    val add : Uchar.t -> original_glyph_id -> t -> unit
    val find_opt : Uchar.t -> t -> glyph_id_pair option
    val find_rev_opt : original_glyph_id -> t -> Uchar.t option
    val fold_rev : (subset_glyph_id -> Uchar.t -> 'a -> 'a) -> 'a -> t -> 'a
  end
= struct

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


    let add uch gidorg r =
      let submap = r.subset_map in
      let ht = r.main in
      let revsubht = r.rev_subset in
      let revorght = r.rev_original in
      let gidsub = submap |> SubsetMap.intern gidorg in
      UHt.add ht uch { original_id = gidorg; subset_id = gidsub; };
      match GSHt.find_opt revsubht gidsub with
      | None ->
          GSHt.add revsubht gidsub uch;
          GOHt.add revorght gidorg uch

      | Some(uchpre) ->
          Logging.warn_noninjective_cmap uchpre uch gidorg;
          ()


    let find_opt uch r =
      UHt.find_opt r.main uch


    let find_rev_opt gidorg r =
      GOHt.find_opt r.rev_original gidorg


    let fold_rev f gidsub r =
      GSHt.fold f r.rev_subset gidsub

  end


type bbox = per_mille * per_mille * per_mille * per_mille


module GlyphBBoxTable
: sig
    type t
    val create : int -> t
    val add : original_glyph_id -> per_mille * bbox -> t -> unit
    val find_opt : original_glyph_id -> t -> (per_mille * bbox) option
    val fold : (original_glyph_id -> per_mille * bbox -> 'a -> 'a) -> 'a -> t -> 'a
  end
= struct

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


let per_mille_raw (units_per_em : int) (w : design_units) : per_mille =
  PerMille(int_of_float ((float_of_int (w * 1000)) /. (float_of_int units_per_em)))


module GSet = Set.Make
  (struct
    type t = original_glyph_id
    let compare = Pervasives.compare
  end)


module GMap = Map.Make
  (struct
    type t = original_glyph_id
    let compare = Pervasives.compare
  end)


type mark_class = int

type anchor_point = per_mille * per_mille

type mark_assoc = (Otfm.glyph_id * Otfm.mark_record) list


module MarkTable
: sig
    type t
    val create : unit -> t
    val add_base : int -> int -> mark_assoc -> (Otfm.glyph_id * Otfm.base_record) list -> t -> unit
    val add_ligature : int -> int -> mark_assoc -> (Otfm.glyph_id * Otfm.ligature_attach) list -> t -> unit
    val add_mark_to_mark : int -> int -> mark_assoc -> (Otfm.glyph_id * Otfm.base_record) list -> t -> unit
    val find_base_opt : original_glyph_id * original_glyph_id -> t -> (anchor_point * anchor_point) option
    val find_ligature_opt : int -> original_glyph_id * original_glyph_id -> t -> (anchor_point * anchor_point) option
    val find_mark_to_mark_opt : original_glyph_id * original_glyph_id -> t -> (anchor_point * anchor_point) option
  end
= struct

    type mark_to_base_entry = {
      class_count : int;
      mark_map    : (mark_class * anchor_point) GMap.t;
      base_map    : (anchor_point array) GMap.t;
    }

    type mark_to_ligature_entry = {
      lig_class_count : int;
      lig_mark_map    : (mark_class * anchor_point) GMap.t;
      lig_base_map    : (((anchor_point option) array) array) GMap.t;
    }

    type t = {
      mutable mark_to_base_table     : mark_to_base_entry list;
      mutable mark_to_ligature_table : mark_to_ligature_entry list;
    }


    let create () =
      { mark_to_base_table = []; mark_to_ligature_table = []; }


    let make_mark_map pmf markassoc =
      markassoc |> List.fold_left (fun map (gidmark, (i, (x, y, _))) ->
        map |> GMap.add gidmark (i, (pmf x, pmf y))
      ) GMap.empty


    let base_record pmf (x, y, _) =
      (pmf x, pmf y)


    let add_base units_per_em class_count markassoc baseassoc mktbl =
      let pmf = per_mille_raw units_per_em in
      let mark_map = make_mark_map pmf markassoc in
      let base_map =
        baseassoc |> List.fold_left (fun map (gidbase, arr) ->
          map |> GMap.add gidbase (arr |> Array.map (base_record pmf))
        ) GMap.empty
      in
      let entry = { class_count; mark_map; base_map; } in
      mktbl.mark_to_base_table <- entry :: mktbl.mark_to_base_table


    let add_ligature units_per_em lig_class_count markassoc (ligassoc : (Otfm.glyph_id * Otfm.ligature_attach) list) mktbl =
      let pmf = per_mille_raw units_per_em in
      let lig_mark_map =
        markassoc |> List.fold_left (fun map (gidmark, (i, (x, y, _))) ->
          map |> GMap.add gidmark (i, (pmf x, pmf y))
        ) GMap.empty
      in
      let lig_base_map =
        ligassoc |> List.fold_left (fun map (gidlig, comprcdlst) ->
          let lst = comprcdlst |> List.map (Array.map (option_map (base_record pmf))) in
            map |> GMap.add gidlig (Array.of_list lst)
        ) GMap.empty
      in
      let entry = { lig_class_count; lig_mark_map; lig_base_map; } in
      mktbl.mark_to_ligature_table <- entry :: mktbl.mark_to_ligature_table


    let add_mark_to_mark = add_base


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
              | (Some(arr), Some(c, ptmark)) -> Some((arr.(c), ptmark))
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


    let find_mark_to_mark_opt = find_base_opt

  end


type error = [ Otfm.error | `Missing_script | `Missing_feature ]


let result_bind x f =
  match x with
  | Ok(v)    -> f v
  | Error(e) -> Error(e :> error)


let get_mark_table srcpath units_per_em d =
  let script_tag = "latn" in  (* temporary; should depend on the script *)
  let mktbl = MarkTable.create () in
  let res =
    let open ResultMonad in
    Otfm.gpos_script d >>= fun scriptlst ->
    match scriptlst |> List.find_opt (fun gs -> Otfm.gpos_script_tag gs = script_tag) with
    | None ->
        return ()

    | Some(script) ->
        Otfm.gpos_langsys script >>= fun (langsys, _) ->
          (* temporary; should depend on the current language system *)
        Otfm.gpos_feature langsys >>= fun (_, featurelst) ->
        begin
          match featurelst |> List.find_opt (fun gf -> Otfm.gpos_feature_tag gf = "mark") with
          | None ->
              return ()

          | Some(feature_mark) ->
              () |> Otfm.gpos feature_mark
                  ~markbase1:(fun clscnt () markassoc baseassoc ->
                    MarkTable.add_base units_per_em clscnt markassoc baseassoc mktbl
                  )
                  ~marklig1:(fun clscnt () markassoc ligassoc ->
                    MarkTable.add_ligature units_per_em clscnt markassoc ligassoc mktbl
                  )
        end >>= fun () ->
        begin
          match featurelst |> List.find_opt (fun gf -> Otfm.gpos_feature_tag gf = "mkmk") with
          | None ->
              return ()

          | Some(feature_mkmk) ->
              () |> Otfm.gpos feature_mkmk
                  ~markmark1:(fun clscnt () mark1assoc mark2assoc ->
                    MarkTable.add_mark_to_mark units_per_em clscnt mark1assoc mark2assoc mktbl
                  )
        end
  in
  match res with
  | Error(oerr) -> broken srcpath oerr "get_mark_table"
  | _           -> mktbl


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


module LigatureTable
: sig
    type single = {
      tail     : original_glyph_id list;
      ligature : original_glyph_id;
    }
    type t
    val create : subset_map -> int -> t
    val add : original_glyph_id -> single list -> t -> unit
    val fold_rev : (subset_glyph_id -> original_glyph_id list -> 'a -> 'a) -> 'a -> t -> 'a
    val match_prefix : original_glyph_segment list -> MarkTable.t -> t -> ligature_matching
  end
= struct

    type single = {
      tail     : original_glyph_id list;
      ligature : original_glyph_id;
    }
      (* -- pair of the tail GID array and the GID of the resulting ligature -- *)

    type t = {
      subset_map  : subset_map;
      entry_table : (single list) GOHt.t;
      rev_table   : (original_glyph_id list) GSHt.t;
    }


    let create submap n =
      let htmain = GOHt.create n in
      let htrev = GSHt.create n in
      { subset_map = submap; entry_table = htmain; rev_table = htrev; }


    let add gidorg liginfolst ligtbl =
      let htmain = ligtbl.entry_table in
      let htrev = ligtbl.rev_table in
      let submap = ligtbl.subset_map in
      begin
        GOHt.add htmain gidorg liginfolst;
        liginfolst |> List.iter (fun single ->
          let gidorgtail = single.tail in
          let gidorglig = single.ligature in
          let gidsublig = submap |> SubsetMap.intern gidorglig in
          match GSHt.find_opt htrev gidsublig with
          | None ->
              GSHt.add htrev gidsublig (gidorg :: gidorgtail)

          | Some(_) ->
              Logging.warn_noninjective_ligature gidorglig;
              ()
        );
      end


    let fold_rev f init ligtbl =
      let htrev = ligtbl.rev_table in
      GSHt.fold (fun gidsub gidorglst acc -> f gidsub gidorglst acc) htrev init


    (* --
       `backtrack_mark_to_mark mktbl markbasef gobase markpairacc gomark` returns:

       * `Some(p)` if `gomark` can be attached at the position `p`
          to `gobase`, to which every mark in `markpairacc` is already attached.

       * `None` otherwise.

       -- *)
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


    (* --
       `attach_marks mktbl markbasef gobase gomarks` returns:

       * `Some([(gm1, p1), ..., (gmN, pN)])` if every `gmI` in `gomarks`
         can be attached to `gobase` at the position `pI`.

       * `None` otherwise.

       -- *)
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


    (* --
       `make_ligature_mark_info mktbl golig markpairs` returns:

       * `Some(markinfolst)` if all diacritical marks of `markpairs`
          are attachable to the ligature `golig`.

       * `None` otherwise.

       -- *)
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


    (* --
       `prefix mktbl golig lst1 seglst2` returns:

       * `Some(seglst, markinfolst)`
`        if `lst1` is a prefix of `seglst2` and
         forming the ligature does not prevent any attachment of diacritical marks,
         where `seglst` is the rest of `seglst2`
         and `markinfolst` is the position information of diacritical marks.

       * `None` otherwise.

       -- *)
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
              (* temporary; should refer to MarkToMark table
                 in order to handle diacritical marks after the first one *)
                begin
                  match attach_marks false mktbl MarkTable.find_base_opt gobase gomarks with
                  | None ->
                    (* if the diacritical marks cannot attach to the base *)
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
                        | Some((golig, markinfolst, segorgrest)) -> Match(golig, markinfolst, segorgrest)  (* temporary *)
                      end
                end
          end
end


let get_ligature_table srcpath (submap : subset_map) (d : Otfm.decoder) : LigatureTable.t =
  let script_tag = "latn" in  (* temporary; should depend on the script *)
  let ligtbl = LigatureTable.create submap 32 (* arbitrary constant; the initial size of the hash table *) in
  let res =
    let (>>=) = result_bind in
    Otfm.gsub_script d >>= fun scriptlst ->
    pickup scriptlst (fun gs -> Otfm.gsub_script_tag gs = script_tag) `Missing_script >>= fun script ->
    Otfm.gsub_langsys script >>= fun (langsys, _) ->
      (* temporary; should depend on the current language system *)
    Otfm.gsub_feature langsys >>= fun (_, featurelst) ->
    pickup featurelst (fun gf -> Otfm.gsub_feature_tag gf = "liga") `Missing_feature >>= fun feature ->
    () |> Otfm.gsub feature ~lig:(fun () (gid, liginfolst) ->
      let liginfolst =
        liginfolst |> List.map (fun (tail, ligature) -> LigatureTable.{ tail; ligature; })
      in
      ligtbl |> LigatureTable.add gid liginfolst) >>= fun () ->
    Ok()
  in
  match res with
  | Ok(()) ->
      ligtbl

  | Error(e) ->
      begin
        match e with
        | `Missing_required_table(tag)
            when tag = Otfm.Tag.gsub -> ligtbl
        | `Missing_script            -> ligtbl
        | `Missing_feature           -> ligtbl
        | #Otfm.error as oerr        -> broken srcpath oerr "get_ligature_table"
      end


module KerningTable
: sig
    type t
    val create : int -> t
    val add : original_glyph_id -> original_glyph_id -> design_units -> t -> unit
    val add_by_class : Otfm.class_definition list -> Otfm.class_definition list -> (Otfm.class_value * (Otfm.class_value * Otfm.value_record * Otfm.value_record) list) list -> t -> unit
    val find_opt : original_glyph_id -> original_glyph_id -> t -> design_units option
  end
= struct

    module HtSingle = Hashtbl.Make
      (struct
        type t = Otfm.glyph_id * Otfm.glyph_id
        let equal = (=)
        let hash = Hashtbl.hash
      end)

    module HtClass = Hashtbl.Make
      (struct
        type t = Otfm.class_value * Otfm.class_value
        let equal = (=)
        let hash = Hashtbl.hash
      end)

    type subtable = Otfm.class_definition list * Otfm.class_definition list * design_units HtClass.t

    type t = int HtSingle.t * (subtable list) ref


    let create size =
      let htS = HtSingle.create size in
      let htC = ref [] in
        (htS, htC)


    let add gid1 gid2 wid (htS, _) =
      begin HtSingle.add htS (gid1, gid2) wid; end


    let add_by_class clsdeflst1 clsdeflst2 lst (_, refC) =
      let htC = HtClass.create 1024 (* arbitrary constant *) in
      begin
        lst |> List.iter (fun (cls1, pairposlst) ->
          pairposlst |> List.iter (fun (cls2, valrcd1, valrcd2) ->
            match valrcd1.Otfm.x_advance with
            | None      -> ()
            | Some(0)   -> ()
            | Some(xa1) -> HtClass.add htC (cls1, cls2) xa1
          )
        );
        refC := (clsdeflst1, clsdeflst2, htC) :: !refC;
      end


    let rec to_class_value gid clsdeflst =
      let iter = to_class_value gid in
        match clsdeflst with
        | []                                        -> None
        | Otfm.GlyphToClass(g, c) :: tail           -> if g = gid then Some(c) else iter tail
        | Otfm.GlyphRangeToClass(gs, ge, c) :: tail -> if gs <= gid && gid <= ge then Some(c) else iter tail


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


let get_kerning_table srcpath (d : Otfm.decoder) =
  let script_tag = "latn" in  (* temporary; should depend on the script *)
  let kerntbl = KerningTable.create 32 (* arbitrary constant; the initial size of the hash table *) in
  let _ =
    () |> Otfm.kern d (fun () kinfo ->
      match kinfo with
      | { Otfm.kern_dir = `H; Otfm.kern_kind = `Kern; Otfm.kern_cross_stream = false } -> (`Fold, ())
      | _                                                                              -> (`Skip, ())
    ) (fun () gid1 gid2 wid ->
      kerntbl |> KerningTable.add gid1 gid2 wid
    )
  in
  let res =
    let (>>=) = result_bind in
    Otfm.gpos_script d >>= fun scriptlst ->
    pickup scriptlst (fun gs -> Otfm.gpos_script_tag gs = script_tag) `Missing_script >>= fun script ->
    Otfm.gpos_langsys script >>= fun (langsys, _) ->
      (* temporary; should depend on the current language system *)
    Otfm.gpos_feature langsys >>= fun (_, featurelst) ->
    pickup featurelst (fun gf -> Otfm.gpos_feature_tag gf = "kern") `Missing_feature >>= fun feature ->
    () |> Otfm.gpos feature
      ~pair1:(fun () (gid1, pairposlst) ->
        pairposlst |> List.iter (fun (gid2, valrcd1, valrcd2) ->
          match valrcd1.Otfm.x_advance with
          | None      -> ()
          | Some(xa1) -> kerntbl |> KerningTable.add gid1 gid2 xa1
        )
      )
      ~pair2:(fun clsdeflst1 clsdeflst2 () sublst ->
        kerntbl |> KerningTable.add_by_class clsdeflst1 clsdeflst2 sublst;
      ) >>= fun () -> Ok()
  in
  match res with
  | Ok(()) ->
        kerntbl

  | Error(e) ->
      begin
        match e with
        | `Missing_required_table(t)
            when t = Otfm.Tag.gpos -> kerntbl
        | `Missing_script          -> kerntbl
        | `Missing_feature         -> kerntbl
        | #Otfm.error as oerr      -> broken srcpath oerr "get_kerning_table"
      end


type decoder = {
  file_path           : abs_path;
  main                : Otfm.decoder;
  cmap_subtable       : Otfm.cmap_subtable;
  head_record         : Otfm.head;
  hhea_record         : Otfm.hhea;
  subset_map          : subset_map;
  glyph_id_table      : GlyphIDTable.t;
  glyph_bbox_table    : GlyphBBoxTable.t;
  kerning_table       : KerningTable.t;
  ligature_table      : LigatureTable.t;
  mark_table          : MarkTable.t;
  charstring_info     : Otfm.charstring_info option;
  units_per_em        : int;
  default_ascent      : per_mille;
  default_descent     : per_mille;
}


let per_mille (dcdr : decoder) (w : design_units) : per_mille =
  per_mille_raw dcdr.units_per_em w


let get_original_gid (dcdr : decoder) (gid : glyph_id) : original_glyph_id =
  let SubsetGlyphID(gidorg, _) = gid in
  gidorg


let get_glyph_raw_bbox (dcdr : decoder) (gidorg : original_glyph_id)
    : ((design_units * design_units * design_units * design_units) option) ok =
  let d = dcdr.main in
  let open ResultMonad in
  Otfm.loca d gidorg >>= function
  | None ->
      return None

  | Some(gloc) ->
      Otfm.glyf d gloc >>= fun (_, rawbbox) ->
      return (Some(rawbbox))


let bbox_zero =
  (PerMille(0), PerMille(0), PerMille(0), PerMille(0))


let get_ttf_bbox (dcdr : decoder) (gidorg : original_glyph_id) : bbox =
  let f = per_mille dcdr in
  match get_glyph_raw_bbox dcdr gidorg with
  | Error(e) ->
      broken dcdr.file_path e (Printf.sprintf "get_ttf_bbox (gid = %d)" gidorg)

  | Ok(None) ->
      bbox_zero

  | Ok(Some(bbox_raw)) ->
      let (xmin_raw, ymin_raw, xmax_raw, ymax_raw) = bbox_raw in
      (f xmin_raw, f ymin_raw, f xmax_raw, f ymax_raw)


let get_glyph_advance_width (dcdr : decoder) (gidorgkey : original_glyph_id) : per_mille =
  let d = dcdr.main in
  let hmtxres =
    None |> Otfm.hmtx d (fun accopt gidorg adv lsb ->
      match accopt with
      | Some(_) -> accopt
      | None    -> if gidorg = gidorgkey then Some((adv, lsb)) else None
    )
  in
    match hmtxres with
    | Error(e)             -> broken dcdr.file_path e (Printf.sprintf "get_glyph_advance_width (gid = %d)" gidorgkey)
    | Ok(None)             -> PerMille(0)
    | Ok(Some((adv, lsb))) -> per_mille dcdr adv


let get_bbox (dcdr : decoder) (gidorg : original_glyph_id) : bbox =
  match dcdr.charstring_info with
  | None ->
    (* -- if the font is TrueType OT -- *)
      get_ttf_bbox dcdr gidorg

  | Some(csinfo) ->
    (* -- if the font is CFF OT -- *)
      begin
        match Otfm.charstring_absolute csinfo gidorg with
        | Error(oerr) ->
            broken dcdr.file_path oerr (Printf.sprintf "get_bbox (gid = %d)" gidorg)

        | Ok(None) ->
            bbox_zero
              (* needs reconsideration; maybe should emit an error *)

        | Ok(Some(pathlst)) ->
            begin
              match Otfm.charstring_bbox pathlst with
              | None ->
                  bbox_zero

              | Some(bbox_raw) ->
                  let (xmin_raw, ymin_raw, xmax_raw, ymax_raw) = bbox_raw in
                  let f = per_mille dcdr in
                  (f xmin_raw, f ymin_raw, f xmax_raw, f ymax_raw)
            end
      end


(* -- PUBLIC -- *)
let get_glyph_metrics (dcdr : decoder) (gid : glyph_id) : metrics =
  let bboxtbl = dcdr.glyph_bbox_table in
  let gidorg = get_original_gid dcdr gid in
  let (wid, (_, ymin, _, ymax)) =
    match bboxtbl |> GlyphBBoxTable.find_opt gidorg with
    | Some(pair) ->
        pair

    | None ->
        let wid = get_glyph_advance_width dcdr gidorg in
        let bbox = get_bbox dcdr gidorg in
        let pair = (wid, bbox) in
        bboxtbl |> GlyphBBoxTable.add gidorg pair;
        pair
  in
  let hgt = ymax in
  let dpt = ymin in
  (wid, hgt, dpt)


type 'a resource =
  | Data           of 'a
  | EmbeddedStream of int

type predefined_encoding =
  | StandardEncoding
  | MacRomanEncoding
  | WinAnsiEncoding

type differences = (string * int) list

type encoding =
  | ImplicitEncoding
  | PredefinedEncoding of predefined_encoding
  | CustomEncoding     of predefined_encoding * differences

type cmap =
  | PredefinedCMap of string
(*
  | CMapFile       of cmap_resource
*)

type matrix = float * float * float * float

type font_stretch =
  | UltraCondensedStretch | ExtraCondensedStretch | CondensedStretch | SemiCondensedStetch
  | NormalStretch
  | SemiExpandedStretch | ExpandedStretch | ExtraExpandedStretch | UltraExpandedStretch


let intern_gid (dcdr : decoder) (gidorg : original_glyph_id) : glyph_id =
  SubsetGlyphID(gidorg, dcdr.subset_map |> SubsetMap.intern gidorg)


let font_stretch_of_width_class srcpath = function
  | 0 -> UltraCondensedStretch
  | 1 -> ExtraCondensedStretch
  | 3 -> CondensedStretch
  | 4 -> SemiCondensedStetch
  | 5 -> NormalStretch
  | 6 -> SemiExpandedStretch
  | 7 -> ExpandedStretch
  | 8 -> ExtraExpandedStretch
  | 9 -> UltraExpandedStretch
  | w -> raise (BrokenFont(srcpath, "illegal width class " ^ (string_of_int w)))


type font_descriptor = {
    font_name    : string;
    font_family  : string;
    font_stretch : font_stretch option;
    font_weight  : int option;  (* -- ranges only over {100, 200, ..., 900} -- *)
    flags        : int option;  (* temporary; maybe should be handled as a boolean record *)
    font_bbox    : bbox;
    italic_angle : float;
    ascent       : per_mille;
    descent      : per_mille;
    stemv        : float;
    font_data    : (Otfm.decoder resource) ref;
      (* temporary; should contain more fields *)
  }


let to_flate_pdf_bytes (data : string) : string * Pdfio.bytes =
  let src_offset_ref = ref 0 in
  let src_len = String.length data in
  let write_byte_as_input buf =
    let src_offset = !src_offset_ref in
    if src_offset >= src_len then 0 else
      begin
        let len =
          if src_len - src_offset < 1024 then src_len - src_offset else 1024
        in
        src_offset_ref += len;
        Bytes.blit_string data src_offset buf 0 len;
        len
      end
  in
  let out_offset_ref = ref 0 in
  let bufout = Bytes.create (2 * src_len) in
    (* -- in the worst case the output size is 1.003 times as large as the input size -- *)
  let write_byte_as_output bufret len =
    let out_offset = !out_offset_ref in
    if len <= 0 then () else
      begin
        out_offset_ref += len;
        Bytes.blit bufret 0 bufout out_offset len
      end
  in
  Pdfflate.compress ~level:9 write_byte_as_input write_byte_as_output;
  let out_len = !out_offset_ref in
  let bt = Pdfio.bytes_of_string (String.sub (Bytes.to_string bufout) 0 out_len) in
  ("/FlateDecode", bt)


let pdfstream_of_decoder (pdf : Pdf.t) (dcdr : decoder) (subtypeopt : string option) : Pdf.pdfobject =
  let d = dcdr.main in
  let data =
      match SubsetMap.to_list dcdr.subset_map with
      | None ->
          begin
            match Otfm.decoder_src d with
            | `String(s) -> s
          end

      | Some(gidorglst) ->
          begin
            match OtfSubset.make d gidorglst with
            | Error(e) -> broken dcdr.file_path e "pdfstream_of_decoder"
            | Ok(s)    -> s
          end
  in
  let (filter, bt) = to_flate_pdf_bytes data in
  let len = Pdfio.bytes_size bt in
  let contents = [
      ("/Length", Pdf.Integer(len));
      ("/Filter", Pdf.Name(filter));
    ]
  in
  let dict =
    match subtypeopt with
    | None          -> contents
    | Some(subtype) -> ("/Subtype", Pdf.Name("/" ^ subtype)) :: contents
  in
  let objstream = Pdf.Stream(ref (Pdf.Dictionary(dict), Pdf.Got(bt))) in
  let irstream = Pdf.addobj pdf objstream in
  Pdf.Indirect(irstream)


let get_glyph_id_main srcpath (cmapsubtbl : Otfm.cmap_subtable) (uch : Uchar.t) : Otfm.glyph_id option =
  let cp = Uchar.to_int uch in
  let cmapres =
    Otfm.cmap_subtable cmapsubtbl (fun accopt mapkd (u0, u1) gid ->
      match accopt with
      | Some(_) ->
          accopt

      | None ->
          if u0 <= cp && cp <= u1 then
            match mapkd with
            | `Glyph_range -> Some(gid + (cp - u0))
            | `Glyph       -> Some(gid)
          else
            None
    ) None
  in
  match cmapres with
  | Error(e)      -> broken srcpath e (Printf.sprintf "get_glyph_id_main (cp = U+%04X)" cp)
  | Ok(opt)       -> opt


let cmap_predicate f =
  List.find_opt (fun subtbl -> f (Otfm.cmap_subtable_ids subtbl))


let get_cmap_subtable srcpath d =
  match Otfm.cmap d with
  | Error(oerr) ->
      broken srcpath oerr "get_cmap_subtable"

  | Ok(subtbllst) ->
      let opt =
        List.fold_left (fun opt idspred ->
          match opt with
          | Some(_) -> opt
          | None    -> subtbllst |> (cmap_predicate idspred)
        ) None [
          (fun (pid, _, format) -> pid = 0 && format = 12);
          (fun (pid, _, format) -> pid = 0 && format = 4);
          (fun (pid, _, _)      -> pid = 0);
          (fun (pid, eid, _)    -> pid = 3 && eid = 10);
          (fun (pid, eid, _)    -> pid = 3 && eid = 1);
          (fun (pid, _, _)      -> pid = 1);
        ]
      in
      match opt with
      | None         -> raise (CannotFindUnicodeCmap(srcpath))
      | Some(subtbl) -> subtbl


let add_element_of_composite_glyph (dcdr : decoder) (gidorg : original_glyph_id) =
  let d = dcdr.main in
  let submap = dcdr.subset_map in
  let open ResultMonad in
    let rec aux gidorg =
      let _ = submap |> SubsetMap.intern gidorg in
      Otfm.loca d gidorg >>= function
      | None ->
          return ()

      | Some(loc) ->
          Otfm.glyf d loc >>= fun (descrmain, _) ->
          begin
            match descrmain with
            | `Simple(_) ->
                return ()

            | `Composite(lst) ->
                lst |> List.fold_left (fun res (gidorg, _, _) ->
                  res >>= fun () ->
                  aux gidorg
                ) (return ())
          end
    in
    let res =
      Otfm.flavour d >>= function
      | Otfm.TTF_true | Otfm.TTF_OT -> aux gidorg
      | Otfm.CFF                    -> return ()
    in
    match res with
    | Error(e) -> broken dcdr.file_path e "add_element_of_composite_glyph"
    | Ok(())   -> ()


(* PUBLIC *)
let get_glyph_id (dcdr : decoder) (uch : Uchar.t) : glyph_id option =
  let gidtbl = dcdr.glyph_id_table in
    match gidtbl |> GlyphIDTable.find_opt uch with
    | Some(gidpair) ->
        Some(SubsetGlyphID(gidpair.original_id, gidpair.subset_id))

    | None ->
        let open OptionMonad in
        get_glyph_id_main dcdr.file_path dcdr.cmap_subtable uch >>= fun gidorg ->
        gidtbl |> GlyphIDTable.add uch gidorg;
        add_element_of_composite_glyph dcdr gidorg;
        let gid = intern_gid dcdr gidorg in
        return gid


(*
let get_glyph_raw_contour_list_and_bounding_box (dcdr : decoder) (gidorg : original_glyph_id)
    : ((((bool * design_units * design_units) list) list * (design_units * design_units * design_units * design_units)) option) ok =
  let d = dcdr.main in
  let open ResultMonad in
  Otfm.loca d gidorg >>= function
  | None ->
      return None

  | Some(gloc) ->
      Otfm.glyf d gloc >>= function
      | (`Composite(_), _)          -> return None
          (* temporary; does not deal with composite glyphs *)
      | (`Simple(precntrlst), bbox) -> return (Some((precntrlst, bbox)))
*)

let of_per_mille = function
  | PerMille(x) -> Pdf.Integer(x)


let of_per_mille_opt = function
  | None              -> Pdf.Null
  | Some(PerMille(x)) -> Pdf.Integer(x)


let of_per_mille_pair_opt = function
  | None                             -> Pdf.Null
  | Some((PerMille(a), PerMille(b))) -> Pdf.Array[Pdf.Integer(a); Pdf.Integer(b)]


let font_descriptor_of_decoder (dcdr : decoder) (font_name : string) =
  let d = dcdr.main in
  let rcdhead = dcdr.head_record in
  let rcdhhea = dcdr.hhea_record in
  match Otfm.os2 d with
  | Error(e) ->
      broken dcdr.file_path e "font_descriptor_of_decoder"

  | Ok(rcdos2) ->
      let bbox =
        (per_mille dcdr rcdhead.Otfm.head_xmin,
         per_mille dcdr rcdhead.Otfm.head_ymin,
         per_mille dcdr rcdhead.Otfm.head_xmax,
         per_mille dcdr rcdhead.Otfm.head_ymax)
      in
      {
        font_name    = font_name; (* -- same as Otfm.postscript_name dcdr -- *)
        font_family  = "";    (* temporary; should be gotten from decoder *)
        font_stretch = Some(font_stretch_of_width_class dcdr.file_path rcdos2.Otfm.os2_us_width_class);
        font_weight  = Some(rcdos2.Otfm.os2_us_weight_class);
        flags        = None;  (* temporary; should be gotten from decoder *)
        font_bbox    = bbox;
        italic_angle = 0.;    (* temporary; should be gotten from decoder; 'post.italicAngle' *)
        ascent       = per_mille dcdr rcdhhea.Otfm.hhea_ascender;
        descent      = per_mille dcdr rcdhhea.Otfm.hhea_descender;
        stemv        = 0.;    (* temporary; should be gotten from decoder *)
        font_data    = ref (Data(d));
          (* temporary; should contain more fields *)
      }


let get_postscript_name (dcdr : decoder) =
  let d = dcdr.main in
  match Otfm.postscript_name d with
  | Error(e)    -> broken dcdr.file_path e "get_postscript_name"
  | Ok(None)    -> assert false  (* temporary *)
  | Ok(Some(x)) -> x


type embedding =
  | FontFile
  | FontFile2
  | FontFile3 of string


let font_file_info_of_embedding embedding =
  match embedding with
  | FontFile       -> ("/FontFile", None)
  | FontFile2      -> ("/FontFile2", None)
  | FontFile3(sub) -> ("/FontFile3", Some(sub))

(*
module Type1Scheme_
= struct
    type font = {
        name            : string option;
          (* --
               obsolete field; required in PDF 1.0
               but optional in all other versions
             -- *)
        base_font       : string;
        first_char      : int;
        last_char       : int;
        widths          : int list;
        font_descriptor : font_descriptor;
        encoding        : encoding;
        to_unicode      : cmap_resource option;
      }


    let of_decoder dcdr fc lc =
      let base_font = get_postscript_name dcdr in
        {
          name            = None;
          base_font       = base_font;
          first_char      = fc;
          last_char       = lc;
          widths          = get_truetype_widths_list dcdr fc lc;
          font_descriptor = font_descriptor_of_decoder dcdr base_font;
          encoding        = PredefinedEncoding(StandardEncoding);
          to_unicode      = None;
        }


    let to_pdfdict_scheme fontsubtype embedding pdf trtyfont dcdr =
      let d = dcdr.main in
      let fontname  = trtyfont.base_font in
      let firstchar = trtyfont.first_char in
      let lastchar  = trtyfont.last_char in
      let widths    = trtyfont.widths in
      let fontdescr = trtyfont.font_descriptor in
      let (font_file_key, embedsubtypeopt) = font_file_info_of_embedding embedding in
      let irstream = add_stream_of_decoder pdf d embedsubtypeopt in
        (* -- add to the PDF the stream in which the font file is embedded -- *)
      let objdescr =
        Pdf.Dictionary[
          ("/Type"       , Pdf.Name("/FontDescriptor"));
          ("/FontName"   , Pdf.Name("/" ^ fontname));
          ("/Flags"      , Pdf.Integer(4));  (* temporary; should be variable *)
          ("/FontBBox"   , Pdf.Array[Pdf.Integer(0); Pdf.Integer(0); Pdf.Integer(0); Pdf.Integer(0)]);  (* temporary; should be variable *)
          ("/ItalicAngle", Pdf.Real(fontdescr.italic_angle));
          ("/Ascent"     , of_per_mille fontdescr.ascent);
          ("/Descent"    , of_per_mille fontdescr.descent);
          ("/StemV"      , Pdf.Real(fontdescr.stemv));
          (font_file_key , Pdf.Indirect(irstream));
        ]
      in
      let irdescr = Pdf.addobj pdf objdescr in
        Pdf.Dictionary[
          ("/Type"          , Pdf.Name("/Font"));
          ("/Subtype"       , Pdf.Name("/" ^ fontsubtype));
          ("/BaseFont"      , Pdf.Name("/" ^ fontname));
          ("/FirstChar"     , Pdf.Integer(firstchar));
          ("/LastChar"      , Pdf.Integer(lastchar));
          ("/Widths"        , Pdf.Array(List.map (fun x -> Pdf.Integer(x)) widths));
          ("/FontDescriptor", Pdf.Indirect(irdescr));
        ]
end

module Type1
= struct
    include Type1Scheme_

    let to_pdfdict = to_pdfdict_scheme "Type1" (FontFile3("Type1C"))
  end

module TrueType
= struct
    include Type1Scheme_

    let to_pdfdict = to_pdfdict_scheme "TrueType" FontFile2
  end

module Type3
= struct
    type font = {
        name            : string;
        font_bbox       : bbox;
        font_matrix     : matrix;
        encoding        : encoding;
        first_char      : int;
        last_char       : int;
        widths          : int list;
        font_descriptor : font_descriptor;
        to_unicode      : cmap_resource option;
      }
  end
*)

module CIDFontType0
= struct
    type font = {
        cid_system_info : cid_system_info;
        base_font       : string;
        font_descriptor : font_descriptor;
        dw              : design_units option;  (* represented by units defined by head.unitsPerEm *)
        dw2             : (int * int) option;
          (* temporary; should contain more fields; /W2 *)
      }
      (* --
         Doesn't have to contain information about /W entry;
         the PDF file will be furnished with /W entry when outputted
         according to the glyph metrics table
         -- *)


    let of_decoder dcdr cidsysinfo =
      let base_font = get_postscript_name dcdr in
        {
          cid_system_info = cidsysinfo;
          base_font       = base_font;
          font_descriptor = font_descriptor_of_decoder dcdr base_font;
          dw              = None;  (* temporary *)
          dw2             = None;  (* temporary *)
        }
  end


type cid_to_gid_map =
  | CIDToGIDIdentity
  | CIDToGIDStream   of (string resource) ref  (* temporary *)


module CIDFontType2
= struct
    type font = {
        cid_system_info  : cid_system_info;
        base_font        : string;
        font_descriptor  : font_descriptor;
        dw               : int option;
        dw2              : (int * int) option;
        cid_to_gid_map   : cid_to_gid_map;
        is_pure_truetype : bool;
          (* temporary; should contain more fields; /W2 *)
      }
      (* --
         Doesn't have to contain information about /W entry;
         the /W entry will be added by using the glyph metrics table when the PDF file is outputted
         -- *)


    let of_decoder dcdr cidsysinfo isptt =
      let base_font = get_postscript_name dcdr in
        {
          cid_system_info  = cidsysinfo;
          base_font        = base_font;
          font_descriptor  = font_descriptor_of_decoder dcdr base_font;
          dw               = None;  (* temporary *)
          dw2              = None;  (* temporary *)
          is_pure_truetype = isptt;
          cid_to_gid_map   = CIDToGIDIdentity;  (* temporary *)
        }
  end


type cid_font =
  | CIDFontType0 of CIDFontType0.font
  | CIDFontType2 of CIDFontType2.font


let pdfobject_of_cmap pdf cmap =
  match cmap with
  | PredefinedCMap(cmapname) -> Pdf.Name("/" ^ cmapname)
(*
  | CMapFile(res)            -> failwith "cmap file for Type 0 fonts; remains to be implemented."
*)

let pdfobject_of_bbox (PerMille(xmin), PerMille(ymin), PerMille(xmax), PerMille(ymax)) =
  Pdf.Array[Pdf.Integer(xmin); Pdf.Integer(ymin); Pdf.Integer(xmax); Pdf.Integer(ymax)]


module ToUnicodeCMap
: sig
    type t
    val create : unit -> t
    val add_single : t -> subset_glyph_id -> Uchar.t list -> unit
    val stringify : t -> string
  end
= struct

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
      Array.iteri (fun i ht ->
        let ht = touccmap.(i) in
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


module Type0
= struct
    type font = {
        base_font        : string;
        encoding         : cmap;
        descendant_fonts : cid_font;  (* -- represented as a singleton list in PDF -- *)
      }


    let of_cid_font cidfont fontname cmap =
      {
        base_font        = fontname;
        encoding         = cmap;
        descendant_fonts = cidfont;
      }


    let pdfobject_of_font_descriptor (pdf : Pdf.t) (dcdr : decoder) fontdescr base_font embedding : Pdf.pdfobject =
      let (font_file_key, tagopt) = font_file_info_of_embedding embedding in
      let objstream = pdfstream_of_decoder pdf dcdr tagopt in
        (* -- add to the PDF the stream in which the font file is embedded -- *)
      let objdescr =
        Pdf.Dictionary[
          ("/Type"       , Pdf.Name("/FontDescriptor"));
          ("/FontName"   , Pdf.Name("/" ^ base_font));
          ("/Flags"      , Pdf.Integer(4));  (* temporary; should be variable *)
          ("/FontBBox"   , pdfobject_of_bbox fontdescr.font_bbox);
          ("/ItalicAngle", Pdf.Real(fontdescr.italic_angle));
          ("/Ascent"     , of_per_mille fontdescr.ascent);
          ("/Descent"    , of_per_mille fontdescr.descent);
          ("/StemV"      , Pdf.Real(fontdescr.stemv));
          (font_file_key , objstream);
        ]
      in
      let irdescr = Pdf.addobj pdf objdescr in
      Pdf.Indirect(irdescr)


    let pdfdict_of_cid_system_info cidsysinfo =
      Pdf.Dictionary[
        ("/Registry"  , Pdf.String(cidsysinfo.registry));
        ("/Ordering"  , Pdf.String(cidsysinfo.ordering));
        ("/Supplement", Pdf.Integer(cidsysinfo.supplement));
      ]


    let pdfobject_of_width_array (pdf : Pdf.t) (dcdr : decoder) : Pdf.pdfobject =
      let bboxtbl = dcdr.glyph_bbox_table in
      let arr =
        bboxtbl |> GlyphBBoxTable.fold (fun gidorg (PerMille(w), _) acc ->
          let SubsetNumber(n) = dcdr.subset_map |> SubsetMap.intern gidorg in
          Pdf.Integer(n) :: Pdf.Array[Pdf.Integer(w)] :: acc
        ) []
      in
      let obj = Pdf.Array(arr) in
      let ir = Pdf.addobj pdf obj in
      Pdf.Indirect(ir)


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


    (* --
       pdfobject_of_cid_type_0:
         returns a descendant font dictionary of Type 0 CIDFont as an indirect reference
       -- *)
    let pdfobject_of_cid_type_0 pdf cidty0font dcdr : Pdf.pdfobject =
      let cidsysinfo = cidty0font.CIDFontType0.cid_system_info in
      let base_font  = cidty0font.CIDFontType0.base_font in
      let fontdescr  = cidty0font.CIDFontType0.font_descriptor in
      let objdescr = pdfobject_of_font_descriptor pdf dcdr fontdescr base_font (FontFile3("OpenType")) in
      let objwarr = pdfobject_of_width_array pdf dcdr in
      let pmoptdw =
        cidty0font.CIDFontType0.dw |> option_map (per_mille dcdr)
      in
      let pmpairoptdw2 =
        cidty0font.CIDFontType0.dw2 |> option_map (fun (a, b) -> (per_mille dcdr a, per_mille dcdr b))
      in
      let objdescend =
        Pdf.Dictionary[
          ("/Type"          , Pdf.Name("/Font"));
          ("/Subtype"       , Pdf.Name("/CIDFontType0"));
          ("/BaseFont"      , Pdf.Name("/" ^ base_font));
          ("/CIDSystemInfo" , pdfdict_of_cid_system_info cidsysinfo);
          ("/FontDescriptor", objdescr);
          ("/DW"            , of_per_mille_opt pmoptdw);
          ("/W"             , objwarr);
          ("/DW2"           , of_per_mille_pair_opt pmpairoptdw2);
            (* temporary; should add more; /W2 *)
        ]
      in
      let irdescend = Pdf.addobj pdf objdescend in
      Pdf.Indirect(irdescend)


    (* --
       pdfobject_of_cid_type_2:
         returns a descendant font dictionary of Type 2 CIDFont as an indirect reference
       -- *)
    let pdfobject_of_cid_type_2 pdf cidty2font dcdr : Pdf.pdfobject =
      let cidsysinfo = cidty2font.CIDFontType2.cid_system_info in
      let base_font  = cidty2font.CIDFontType2.base_font in
      let fontdescr  = cidty2font.CIDFontType2.font_descriptor in
      let font_file =
      (* probably such conditional branching is not appropriate; should always choose true-branch *)
        if cidty2font.CIDFontType2.is_pure_truetype then
          FontFile2
        else
          FontFile3("OpenType")
      in
      let objdescr = pdfobject_of_font_descriptor pdf dcdr fontdescr base_font font_file in
      let objcidtogidmap =
        match cidty2font.CIDFontType2.cid_to_gid_map with
        | CIDToGIDIdentity -> Pdf.Name("/Identity")
        | _                -> remains_to_be_implemented "/CIDToGIDMap other than /Identity"
      in
      let dwpmopt =
        cidty2font.CIDFontType2.dw |> option_map (fun dw -> per_mille dcdr dw)
      in  (* -- per mille -- *)
      let dw2pmpairopt =
        cidty2font.CIDFontType2.dw2 |> option_map (fun (a, b) -> (per_mille dcdr a, per_mille dcdr b))
      in
      let objwarr = pdfobject_of_width_array pdf dcdr in
      let objdescend =
        Pdf.Dictionary[
          ("/Type"          , Pdf.Name("/Font"));
          ("/Subtype"       , Pdf.Name("/CIDFontType2"));
          ("/BaseFont"      , Pdf.Name("/" ^ base_font));
          ("/CIDSystemInfo" , pdfdict_of_cid_system_info cidsysinfo);
          ("/FontDescriptor", objdescr);
          ("/DW"            , of_per_mille_opt dwpmopt);
          ("/W"             , objwarr);
          ("/DW2"           , of_per_mille_pair_opt dw2pmpairopt);
          ("/CIDToGIDMap"   , objcidtogidmap);
            (* should add more; /W2 *)
        ]
      in
      let irdescend = Pdf.addobj pdf objdescend in
      Pdf.Indirect(irdescend)


    let to_pdfdict pdf ty0font dcdr =
      let cidfont       = ty0font.descendant_fonts in
      let base_font_ty0 = ty0font.base_font in
      let cmap          = ty0font.encoding in
      let objdescend =
        match cidfont with
        | CIDFontType0(cidty0font) -> pdfobject_of_cid_type_0 pdf cidty0font dcdr
        | CIDFontType2(cidty2font) -> pdfobject_of_cid_type_2 pdf cidty2font dcdr
      in
      let pdfobjtouc = pdfobject_of_to_unicode_cmap pdf dcdr in
        Pdf.Dictionary[
          ("/Type"           , Pdf.Name("/Font"));
          ("/Subtype"        , Pdf.Name("/Type0"));
          ("/Encoding"       , pdfobject_of_cmap pdf cmap);
          ("/BaseFont"       , Pdf.Name("/" ^ base_font_ty0));  (* -- can be arbitrary name -- *)
          ("/DescendantFonts", Pdf.Array[objdescend]);
          ("/ToUnicode"      , pdfobjtouc);
        ]

  end

type font =
(*  | Type1    of Type1.font *)
(*  | Type1C *)
(*  | MMType1 *)
(*  | Type3 *)
(*  | TrueType of TrueType.font *)
  | Type0    of Type0.font


let make_dictionary (pdf : Pdf.t) (font : font) (dcdr : decoder) : Pdf.pdfobject =
  match font with
(*
  | FontFormat.Type1(ty1font)     -> FontFormat.Type1.to_pdfdict pdf ty1font dcdr
  | FontFormat.TrueType(trtyfont) -> FontFormat.TrueType.to_pdfdict pdf trtyfont dcdr
*)
  | Type0(ty0font) -> Type0.to_pdfdict pdf ty0font dcdr


let make_decoder (abspath : abs_path) (d : Otfm.decoder) : decoder =
  let cmapsubtbl = get_cmap_subtable abspath d in
  let submap =
    match Otfm.flavour d with
    | Error(e)                        -> broken abspath e "make_decoder"
    | Ok(Otfm.TTF_true | Otfm.TTF_OT) -> SubsetMap.create 32  (* temporary; initial size of hash tables *)
    | Ok(Otfm.CFF)                    -> SubsetMap.create_dummy ()
  in
  let gidtbl = GlyphIDTable.create submap 256 in  (* temporary; initial size of hash tables *)
  let bboxtbl = GlyphBBoxTable.create 256 in  (* temporary; initial size of hash tables *)
  let (rcdhhea, ascent, descent) =
    match Otfm.hhea d with
    | Ok(rcdhhea) -> (rcdhhea, rcdhhea.Otfm.hhea_ascender, rcdhhea.Otfm.hhea_descender)
    | Error(e)    -> broken abspath e "make_decoder (hhea)"
  in
  let (rcdhead, units_per_em) =
    match Otfm.head d with
    | Ok(rcdhead) -> (rcdhead, rcdhead.Otfm.head_units_per_em)
    | Error(e)    -> broken abspath e "make_decoder (head)"
  in
  let kerntbl = get_kerning_table abspath d in
  let ligtbl = get_ligature_table abspath submap d in
  let mktbl = get_mark_table abspath units_per_em d in
  let csinfo =
    match Otfm.cff d with
    | Error(_)    -> None
    | Ok(cffinfo) -> Some(cffinfo.Otfm.charstring_info)
  in
    {
      file_path           = abspath;
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
      charstring_info     = csinfo;
      units_per_em        = units_per_em;
      default_ascent      = per_mille_raw units_per_em ascent;
      default_descent     = per_mille_raw units_per_em descent;
    }


let cid_font_type_0 cidty0font fontname cmap =
  Type0(Type0.of_cid_font (CIDFontType0(cidty0font)) fontname cmap)


let cid_font_type_2 cidty2font fontname cmap =
  Type0(Type0.of_cid_font (CIDFontType2(cidty2font)) fontname cmap)


let get_font (dcdr : decoder) (fontreg : font_registration) (fontname : string) : font =
  let cmap = PredefinedCMap("Identity-H") in
  match fontreg with
  | CIDFontType0Registration(cidsysinfo, embedW) ->
      let cidty0font = CIDFontType0.of_decoder dcdr cidsysinfo in
      (cid_font_type_0 cidty0font fontname cmap)

  | CIDFontType2TTRegistration(cidsysinfo, embedW) ->
      let cidty2font = CIDFontType2.of_decoder dcdr cidsysinfo true in
      (cid_font_type_2 cidty2font fontname cmap)

  | CIDFontType2OTRegistration(cidsysinfo, embedW) ->
      let cidty2font = CIDFontType2.of_decoder dcdr cidsysinfo true (* temporary *) in
      (cid_font_type_2 cidty2font fontname cmap)


let get_decoder_single (fontname : string) (abspath : abs_path) : (decoder * font) option =
  match get_main_decoder_single abspath with
  | Error(oerr)            -> broken abspath oerr "get_decoder_single"
  | Ok(None)               -> None
  | Ok(Some((d, fontreg))) -> let dcdr = make_decoder abspath d in Some((dcdr, get_font dcdr fontreg fontname))


let get_decoder_ttc (fontname : string) (abspath :abs_path) (i : int) : (decoder * font) option =
  match get_main_decoder_ttc abspath i with
  | Error(oerr)            -> broken abspath oerr "get_decoder_ttc"
  | Ok(None)               -> None
  | Ok(Some((d, fontreg))) -> let dcdr = make_decoder abspath d in Some((dcdr, get_font dcdr fontreg fontname))


let convert_to_ligatures (dcdr : decoder) (seglst : glyph_segment list) : glyph_synthesis list =
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
        Alist.to_list acc

    | Match(gidorglig, markorginfolst, segorgrest) ->
        let markinfolst =
          markorginfolst |> List.map (fun (gidorg, v) ->
            let (w, _, _) = get_glyph_metrics dcdr (intf gidorg) in
              Mark(intf gidorg, w, v)
          )
        in
        aux (Alist.extend acc (intf gidorglig, markinfolst)) segorgrest
  in
  let segorglst = seglst |> List.map orgsegf in
  aux Alist.empty segorglst


let find_kerning (dcdr : decoder) (gidprev : glyph_id) (gid : glyph_id) : per_mille option =
  let kerntbl = dcdr.kerning_table in
  let gidorgprev = get_original_gid dcdr gidprev in
  let gidorg = get_original_gid dcdr gid in
  let open OptionMonad in
    kerntbl |> KerningTable.find_opt gidorgprev gidorg >>= fun du ->
    return (per_mille dcdr du)


module MathInfoMap = Map.Make
  (struct
    type t = original_glyph_id
    let equal = (=)
    let compare = Pervasives.compare
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
    math_constants             : Otfm.math_constants;
    math_italics_correction    : per_mille MathInfoMap.t;
    math_top_accent_attachment : per_mille MathInfoMap.t;
    math_vertical_variants     : (math_variant_glyph list) MathInfoMap.t;
    math_horizontal_variants   : (math_variant_glyph list) MathInfoMap.t;
    math_kern_info             : math_kern_info MathInfoMap.t;
    script_style_info          : Otfm.gsub_feature option;
  }


let math_base_font (md : math_decoder) : decoder =
  md.as_normal_font


let get_main_math_value ((x, _) : Otfm.math_value_record) = x


let percent n =
  (float_of_int n) /. 100.


let to_design_units (md : math_decoder) (ratio : float) : design_units =
  let upem = md.as_normal_font.units_per_em in
    int_of_float (ratio *. (float_of_int upem))


let to_ratio (md : math_decoder) (du : design_units) : float =
  let upem = md.as_normal_font.units_per_em in
    (float_of_int du) /. (float_of_int upem)


let convert_kern (mkopt : Otfm.math_kern option) : math_kern =
  let f = get_main_math_value in
  let rec aux acc mk =
    match mk with
    | ([], kernlast :: [])                       -> (Alist.to_list acc, f kernlast)
    | (hgthead :: hgttail, kernhead :: kerntail) -> aux (Alist.extend acc (f hgthead, f kernhead)) (hgttail, kerntail)
    | _                                          -> assert false  (* temporary; should report error *)
  in
  match mkopt with
  | None     -> ([], 0)
  | Some(mk) -> aux Alist.empty mk


let convert_kern_info (mkir : Otfm.math_kern_info_record) =
  {
    kernTR = convert_kern mkir.Otfm.top_right_math_kern;
    kernTL = convert_kern mkir.Otfm.top_left_math_kern;
    kernBR = convert_kern mkir.Otfm.bottom_right_math_kern;
    kernBL = convert_kern mkir.Otfm.bottom_left_math_kern;
  }


let assoc_to_map f gidassoc =
  gidassoc |> List.fold_left (fun map (gid, data) ->
    map |> MathInfoMap.add gid (f data)
  ) MathInfoMap.empty


let get_math_decoder (fontname : string) (abspath : abs_path) : (math_decoder * font) option =
  let open OptionMonad in
  get_decoder_single fontname abspath >>= fun (dcdr, font) ->
  let d = dcdr.main in
    match Otfm.math d with
    | Error(oerr) ->
        broken abspath oerr "get_math_decoder"

    | Ok(mathraw) ->
        let micmap =
          mathraw.Otfm.math_glyph_info.Otfm.math_italics_correction
            |> assoc_to_map (fun v -> per_mille dcdr (get_main_math_value v))
        in
        let mkimap =
          mathraw.Otfm.math_glyph_info.Otfm.math_kern_info
            |> assoc_to_map convert_kern_info
        in
        let mvertvarmap =
          mathraw.Otfm.math_variants.Otfm.vert_glyph_assoc
            |> assoc_to_map (fun mgconstr -> mgconstr.Otfm.math_glyph_variant_record_list)
        in
        let mhorzvarmap =
          mathraw.Otfm.math_variants.Otfm.horiz_glyph_assoc
            |> assoc_to_map (fun mgconstr -> mgconstr.Otfm.math_glyph_variant_record_list)
        in
        let sstyopt =
          let ( >>= ) = result_bind in
          let res =
            Otfm.gsub_script d >>= fun scriptlst ->
            pickup scriptlst (fun gs -> Otfm.gsub_script_tag gs = "math") `Missing_script >>= fun script_math ->
            Otfm.gsub_langsys script_math >>= fun (langsys, _) ->
            Otfm.gsub_feature langsys >>= fun (_, featurelst) ->
            pickup featurelst (fun gf -> Otfm.gsub_feature_tag gf = "ssty") `Missing_feature
          in
          match res with
          | Ok(feature_ssty) -> Some(feature_ssty)
          | Error(oerr)      -> None
        in
        let md =
          {
            as_normal_font             = dcdr;
            math_constants             = mathraw.Otfm.math_constants;
            math_italics_correction    = micmap;
            math_top_accent_attachment = MathInfoMap.empty;  (* temporary *)
            math_vertical_variants     = mvertvarmap;
            math_horizontal_variants   = mhorzvarmap;
            math_kern_info             = mkimap;
            script_style_info          = sstyopt;
          }
        in
        Some((md, font))


let get_math_script_variant (md : math_decoder) (gid : glyph_id) : glyph_id =
  match md.script_style_info with
  | None ->
    (* -- if the font does NOT have 'ssty' feature table -- *)
      gid

  | Some(feature_ssty) ->
      let dcdr = md.as_normal_font in
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
      let res = Otfm.gsub feature_ssty ~single:f_single ~alt:f_alt None in
      match res with
      | Error(oerr)          -> gid  (* temporary; maybe should emit an error *)
      | Ok(None)             -> gid
      | Ok(Some(gidorgssty)) -> intern_gid dcdr gidorgssty



let get_math_glyph_id (md : math_decoder) (uch : Uchar.t) : glyph_id option =
  let dcdr = md.as_normal_font in
  get_glyph_id dcdr uch


let truncate_negative (PerMille(x)) =
  PerMille(max 0 x)


let truncate_positive (PerMille(x)) =
  PerMille(min 0 x)


let get_math_glyph_metrics (md : math_decoder) (gid : glyph_id) : per_mille * per_mille * per_mille =
  let dcdr = md.as_normal_font in
  let (wid, _, _) = get_glyph_metrics dcdr gid in
  let gidorg = get_original_gid dcdr gid in
  let (_, _, ymin, ymax) = get_bbox md.as_normal_font gidorg in
  let hgt = truncate_negative ymax in
  let dpt = truncate_positive ymin in
  (wid, hgt, dpt)


let get_math_correction_metrics (md : math_decoder) (gid : glyph_id) : per_mille option * math_kern_info option =
  let gidorg = get_original_gid md.as_normal_font gid in
  let micopt = md.math_italics_correction |> MathInfoMap.find_opt gidorg in
  let mkiopt = md.math_kern_info |> MathInfoMap.find_opt gidorg in
  (micopt, mkiopt)


let get_math_variants (md : math_decoder) (gid : glyph_id) (map : (math_variant_glyph list) MathInfoMap.t) : (glyph_id * float) list =
  let dcdr = md.as_normal_font in
  let gidorg = get_original_gid dcdr gid in
  match map |> MathInfoMap.find_opt gidorg with
  | None        -> []
  | Some(assoc) -> assoc |> List.map (fun (gidorg, du) -> (intern_gid dcdr gidorg, to_ratio md du))


let get_math_vertical_variants (md : math_decoder) (gid : glyph_id) : (glyph_id * float) list =
  let mvertvarmap = md.math_vertical_variants in
  mvertvarmap |> get_math_variants md gid


let get_math_horizontal_variants (md : math_decoder) (gid : glyph_id) =
  let mhorzvarmap = md.math_horizontal_variants in
  mhorzvarmap |> get_math_variants md gid


type math_constants =
  {
  (* -- general -- *)
    axis_height                   : float;
  (* -- sub/superscripts -- *)
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
  (* -- fractions -- *)
    fraction_rule_thickness       : float;
    fraction_numer_d_shift_up     : float;
    fraction_numer_d_gap_min      : float;
    fraction_denom_d_shift_down   : float;
    fraction_denom_d_gap_min      : float;
  (* -- radicals -- *)
    radical_extra_ascender        : float;
    radical_rule_thickness        : float;
    radical_d_vertical_gap        : float;
  (* -- limits -- *)
    upper_limit_gap_min           : float;
    upper_limit_baseline_rise_min : float;
    lower_limit_gap_min           : float;
    lower_limit_baseline_drop_min : float;
  }


let get_main_ratio (md : math_decoder) (mvr : Otfm.math_value_record) : float =
  to_ratio md (get_main_math_value mvr)


let get_axis_height_ratio (md : math_decoder) : float =
  let mc = md.math_constants in
  get_main_ratio md mc.Otfm.axis_height


let get_math_constants (md : math_decoder) : math_constants =
  let mc = md.math_constants in
  let f = get_main_ratio md in
  {
    axis_height                   = f mc.Otfm.axis_height;

    superscript_bottom_min        = f mc.Otfm.superscript_bottom_min;
    superscript_shift_up          = f mc.Otfm.superscript_shift_up;
    superscript_baseline_drop_max = f mc.Otfm.superscript_baseline_drop_max;
    subscript_top_max             = f mc.Otfm.subscript_top_max;
    subscript_shift_down          = f mc.Otfm.subscript_shift_down;
    subscript_baseline_drop_min   = f mc.Otfm.subscript_baseline_drop_min;
    script_scale_down             = percent mc.Otfm.script_percent_scale_down;
    script_script_scale_down      = percent mc.Otfm.script_script_percent_scale_down;
    space_after_script            = f mc.Otfm.space_after_script;
    sub_superscript_gap_min       = f mc.Otfm.sub_superscript_gap_min;

    fraction_rule_thickness       = f mc.Otfm.fraction_rule_thickness;
    fraction_numer_d_shift_up     = f mc.Otfm.fraction_numerator_display_style_shift_up;
    fraction_numer_d_gap_min      = f mc.Otfm.fraction_num_display_style_gap_min;
    fraction_denom_d_shift_down   = f mc.Otfm.fraction_denominator_display_style_shift_down;
    fraction_denom_d_gap_min      = f mc.Otfm.fraction_denom_display_style_gap_min;

    radical_extra_ascender        = f mc.Otfm.radical_extra_ascender;
    radical_rule_thickness        = f mc.Otfm.radical_rule_thickness;
    radical_d_vertical_gap        = f mc.Otfm.radical_display_style_vertical_gap;

    upper_limit_gap_min           = f mc.Otfm.upper_limit_gap_min;
    upper_limit_baseline_rise_min = f mc.Otfm.upper_limit_baseline_rise_min;
    lower_limit_gap_min           = f mc.Otfm.lower_limit_gap_min;
    lower_limit_baseline_drop_min = f mc.Otfm.lower_limit_baseline_drop_min;
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
