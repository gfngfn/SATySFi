(* -*- coding: utf-8 -*- *)
(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli, and 2017-2018 Takashi Suwa. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open OtfUtils

module WideInt = WideInt

(* OpenType tags *)

type tag = wint

module Tag = struct
  type t = tag

  (* OpenType version tags. *)

  let v_wOFF = !%% 0x774F4646L
  let v_OTTO = !%% 0x4F54544FL
  let v_ttcf = !%% 0x74746366L
  let v_true = !%% 0x74727565L (* may happen in the wild. *)

  (* Required common tables tags *)

  let cmap = !%% 0x636D6170L
  let head = !%% 0x68656164L
  let hhea = !%% 0x68686561L
  let hmtx = !%% 0x686D7478L
  let maxp = !%% 0x6D617870L
  let name = !%% 0x6E616D65L
  let os2  = !%% 0x4F532F32L
  let post = !%% 0x706F7374L

  let t_common = [ cmap; head; hhea; hmtx; maxp; name; os2; post ]

  (* TTF font table tags *)

  let cvt  = !%% 0x63767420L
  let fpgm = !%% 0x6670676DL
  let glyf = !%% 0x676C7966L
  let loca = !%% 0x6C6F6361L
  let prep = !%% 0x70726570L

  (* CFF font table tags *)

  let cff  = !%% 0x43464620L
  let vorg = !%% 0x564F5247L

  (* Bitmap glyph tables *)

  let ebdt = !%% 0x45424454L
  let eblc = !%% 0x45424C43L
  let ebsc = !%% 0x45425343L

  (* Optional tables. *)

  let dsig = !%% 0x44534947L
  let gasp = !%% 0x67617370L
  let hdmx = !%% 0x68646D78L
  let kern = !%% 0x6B65726EL
  let ltsh = !%% 0x4C545348L
  let pclt = !%% 0x50434C54L
  let vdmx = !%% 0x56444D58L
  let vhea = !%% 0x76686561L
  let vmtx = !%% 0x766D7478L

  (* Advanced Open Type font layout tables *)

  let base = !%% 0x42415345L
  let gdef = !%% 0x47444546L
  let gpos = !%% 0x47504F53L
  let gsub = !%% 0x47535542L
  let jstf = !%% 0x4A535446L
  let math = !%% 0x4d415448L

  (* Functions *)

  let of_bytes s =
    let open WideInt in
      if String.length s <> 4 then
        invalid_arg (err_invalid_tag s)
      else
        let b0 = of_byte (String.get s 0) in
        let b1 = of_byte (String.get s 1) in
        let b2 = of_byte (String.get s 2) in
        let b3 = of_byte (String.get s 3) in
        (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3


  let to_bytes t =
    let (c0, c1, c2, c3) = cut_uint32_unsafe t in
    Printf.sprintf "%c%c%c%c" c0 c1 c2 c3


  let to_wide_int x = x
  let of_wide_int x = x

  let compare = Pervasives.compare
  let pp ppf t = pp ppf "'%s'" (to_bytes t)
end

(* Unicode code points *)

type cp = int
type cp_range = cp * cp
let is_cp i = 0x0000 <= i && i <= 0x10FFFF
let pp_cp ppf cp = Format.fprintf ppf "U+%04X" cp

(* Decode *)

type error_ctx = [ `Table of tag | `Offset_table | `Table_directory ]
type error =
[
  | `Unknown_flavour                  of tag
  | `Unsupported_cmap_format          of int
  | `Unsupported_glyf_matching_points
  | `Missing_required_table           of tag
  | `Unknown_version                  of error_ctx * wint
  | `Unknown_loca_format              of error_ctx * int
  | `Unknown_composite_format         of error_ctx * int
  | `Invalid_offset                   of error_ctx * int
  | `Invalid_cp                       of int
  | `Invalid_cp_range                 of int * int
  | `Invalid_postscript_name          of string
  | `Unexpected_eoi                   of error_ctx

  | `Inconsistent_length_of_coverage  of error_ctx
  | `Inconsistent_length_of_class
  | `Invalid_lookup_order             of int
  | `Invalid_feature_index            of int
  | `Invalid_feature_params           of int
  | `Invalid_extension_position
  | `Invalid_GSUB_lookup_type         of int
  | `Invalid_GPOS_lookup_type         of int
  | `Invalid_cff_not_a_quad
  | `Invalid_cff_not_an_integer
  | `Invalid_cff_not_an_element
  | `Invalid_cff_not_an_offsize       of int
  | `Invalid_cff_not_a_singleton
  | `Missing_required_dict_long_key   of int
  | `Missing_required_dict_short_key  of int
  | `Invalid_cff_inconsistent_length
  | `Invalid_cff_invalid_first_offset
  | `Invalid_cff_no_private_dict
  | `Unknown_fdselect_format          of int
  | `Invalid_fd_select                of int
  | `Invalid_fd_index                 of int
  | `Invalid_charstring_type          of int
  | `Invalid_charstring
  | `Invalid_sid                      of int
  | `Invalid_ros
  | `Layered_ttc
  | `Invalid_index_to_loc_format      of int

  | `Not_encodable_as_uint8           of int
  | `Not_encodable_as_int8            of int
  | `Not_encodable_as_uint16          of int
  | `Not_encodable_as_int16           of int
  | `Not_encodable_as_uint32          of wint
  | `Not_encodable_as_int32           of wint
  | `Not_encodable_as_time            of wint
  | `Too_many_glyphs_for_encoding     of int
  | `No_glyph_for_encoding
  | `Missing_head_table_for_encoding
]

let pp_ctx ppf = function
| `Table tag       -> pp ppf "table %a" Tag.pp tag
| `Offset_table    -> pp ppf "offset table"
| `Table_directory -> pp ppf "table directory"

let pp_error ppf = function
| `Unknown_flavour tag ->
    pp ppf "@[Unknown@ OpenType@ flavour (%a)@]" Tag.pp tag
| `Missing_required_table tag ->
    pp ppf "@[Missing@ required@ table (%a)@]" Tag.pp tag
| `Unsupported_cmap_format cmapfmt ->
    pp ppf "@[Unsupported@ cmap@ subtable@ format@ %d@]" cmapfmt
| `Unsupported_glyf_matching_points ->
    pp ppf "@[Unsupported@ glyf@ matching@ points)@]"
| `Unknown_version (ctx, v) ->
    pp ppf "@[Unknown@ version (%LX)@ in@ %a@]" (WideInt.to_int64 v) pp_ctx ctx
| `Unknown_loca_format (ctx, v) ->
    pp ppf "@[Unknown@ loca table format (%d)@ in@ %a@]" v pp_ctx ctx
| `Unknown_composite_format (ctx, v) ->
    pp ppf "@[Unknown@ composite glyph format (%d)@ in@ %a@]" v pp_ctx ctx
| `Invalid_offset (ctx, o) ->
    pp ppf "@[Invalid@ offset (%d)@ in@ %a@]" o pp_ctx ctx
| `Invalid_cp u ->
    pp ppf "@[Invalid@ Unicode@ code@ point@ (%a)@]" pp_cp u
| `Invalid_cp_range (u0, u1) ->
    pp ppf "@[Invalid@ Unicode@ code@ point@ range (%a, %a)@]" pp_cp u0 pp_cp u1
| `Invalid_postscript_name n ->
    pp ppf "@[Invalid@ PostScript@ name (%S)@]" n
| `Unexpected_eoi ctx ->
    pp ppf "@[Unexpected@ end@ of@ input@ in %a@]" pp_ctx ctx

| `Inconsistent_length_of_coverage ctx ->
    pp ppf "@[Inconsistent@ length@ of@ coverage@ in %a@]" pp_ctx ctx
| `Inconsistent_length_of_class ->
    pp ppf "@[Inconsistent@ length@ of@ class@]"
| `Missing_required_script_tag tag ->
    pp ppf "@[Missing@ required@ Script@ tag@ (%S)" tag
| `Missing_required_langsys_tag tag ->
    pp ppf "@[Missing@ required@ LangSys@ tag@ (%S)@]" tag
| `Missing_required_feature_tag tag ->
    pp ppf "@[Missing@ required@ Feature@ tag@ (%S)@]" tag
| `Invalid_lookup_order lo ->
    pp ppf "@[Invalid@ lookup@ order@ (%d)@]" lo
| `Invalid_feature_index fi ->
    pp ppf "@[Invalid@ feature@ index@ (%d)@]" fi
| `Invalid_feature_params fp ->
    pp ppf "@[Invalid@ feature@ params@ (%d)@]" fp
| `Invalid_extension_position ->
    pp ppf "@[Invalid@ extension@ position@]"
| `Invalid_GSUB_lookup_type lty ->
    pp ppf "@[Invalid@ GSUB@ LookupType@ (%d)@]" lty
| `Invalid_GPOS_lookup_type lty ->
    pp ppf "@[Invalid@ GPOS@ LookupType@ (%d)@]" lty
| `Invalid_cff_not_a_quad ->
    pp ppf "@[Invalid@ CFF@ table;@ not@ a@ quad@]"
| `Invalid_cff_not_an_integer ->
    pp ppf "@[Invalid@ CFF@ table;@ not@ an@ integer@]"
| `Invalid_cff_not_a_singleton ->
    pp ppf "@[Invalid@ CFF@ table;@ not@ a@ singleton@]"
| `Invalid_cff_not_an_element ->
    pp ppf "@[Invalid@ CFF@ table;@ not@ an@ element@]"
| `Invalid_cff_not_an_offsize(n) ->
    pp ppf "@[Invalid@ CFF@ table;@ not@ an@ offsize@ %d@]" n
| `Missing_required_dict_short_key i ->
    pp ppf "@[Missing@ required@ key@ '%d'@ in@ a@ DICT@]" i
| `Missing_required_dict_long_key i ->
    pp ppf "@[Missing@ required@ key@ '12 %d'@ in@ a@ DICT@]" i
| `Invalid_cff_inconsistent_length ->
    pp ppf "@[Invalid@ CFF@ table;@ inconsistent@ length@]"
| `Invalid_cff_invalid_first_offset ->
    pp ppf "@[Invalid@ CFF@ table;@ invalid@ first@ offset@]"
| `Invalid_cff_no_private_dict ->
    pp ppf "@[Invalid@ CFF@ table;@ no@ Private@ DICT@]"
| `Unknown_fdselect_format n ->
    pp ppf "@[Unknown@ FDSelect@ format@ (%d)@]" n
| `Invalid_fd_select gid ->
    pp ppf "@[Invalid@ FDSelect;@ it@ lacks@ a@ necessary@ glyph@ ID (%d)@]" gid
| `Invalid_fd_index fdi ->
    pp ppf "@[Invalid@ FD@ index@ (%d)]" fdi
| `Invalid_charstring_type csty ->
    pp ppf "@[Invalid@ CharString@ type@ (%d)@]" csty
| `Invalid_charstring ->
    pp ppf "@[Invalid@ CharString@]"
| `Invalid_sid sid ->
    pp ppf "@[Invalid@ SID@ (%d)@]" sid
| `Invalid_ros ->
    pp ppf "@[Invalid@ ROS@]"
| `Layered_ttc ->
    pp ppf "@[Layered@ TTC@]"
| `Invalid_index_to_loc_format locfmt ->
    pp ppf "@[Invalid@ IndexToLocFormat@ entry@ in@ the@ 'head'@ table@ (%d)@]" locfmt

| `Not_encodable_as_uint8 ui ->
    pp ppf "@[Not@ encodable@ as@ uint8@ (%d)@]" ui
| `Not_encodable_as_int8 i ->
    pp ppf "@[Not@ encodable@ as@ int8@ (%d)@]" i
| `Not_encodable_as_uint16 ui ->
    pp ppf "@[Not@ encodable@ as@ uint16@ (%d)@]" ui
| `Not_encodable_as_int16 i ->
    pp ppf "@[Not@ encodable@ as@ int16@ (%d)@]" i
| `Not_encodable_as_uint32 wui ->
    pp ppf "@[Not@ encodable@ as@ uint32@ (%a)@]" WideInt.pp wui
| `Not_encodable_as_int32 wi ->
    pp ppf "@[Not@ encodable@ as@ int32@ (%a)@]" WideInt.pp wi
| `Not_encodable_as_time wi ->
    pp ppf "@[Not@ encodable@ as@ LONGDATETIME@ (%a)@]" WideInt.pp wi
| `Too_many_glyphs_for_encoding num ->
    pp ppf "@[Too@ many@ glyphs@ for@ encoding@ (%d)@]" num
| `No_glyph_for_encoding ->
    pp ppf "@[No@ glyph@ for@ encoding@]"
| `Missing_head_table_for_encoding ->
    pp ppf "@[Missing@ 'head'@ table@ for@ encoding@]"
(* N.B. Offsets and lengths are decoded as OCaml ints. On 64 bits
   platforms they fit, on 32 bits we are limited by string size
   anyway. *)

type flavour = TTF_true | TTF_OT | CFF
type src = [ `String of string ]

(* TODO maybe it would be better not to maintain t_pos/i_pos,
   but rather pass them as arguments to decoding functions. *)

type loc_format =
  | ShortLocFormat
  | LongLocFormat

type decoder_state =
  | Fatal of error
  | Start
  | Ready

type decoder =
  {
    i                           : string;                     (* input data. *)
    i_max                       : int;                        (* input maximal position. *)
    mutable i_pos               : int;                        (* input current position. *)
    mutable state               : decoder_state;              (* decoder state. *)
    mutable ctx                 : error_ctx;                  (* the current error context. *)
    mutable flavour             : flavour;                    (* decoded flavour. *)
    mutable tables              : (tag * int * int) list;     (* decoded table records. *)
    mutable loca_pos_and_format : (int * loc_format) option;  (* for TTF fonts, lazy init. *)
    mutable glyf_pos            : int option;                 (* for TTF fonts, lazy init. *)
    mutable buf                 : Buffer.t;                   (* internal buffer. *)
  }

type ttc_element = int * decoder

type decoder_scheme =
  | SingleDecoder      of decoder
  | TrueTypeCollection of ttc_element list


let decoder_src d = `String(d.i)

let err_eoi d                = err (`Unexpected_eoi(d.ctx))
let e_version d v            = `Unknown_version(d.ctx, v)
let err_version d v          = err (e_version d v)
let err_loca_format d v      = err (`Unknown_loca_format(d.ctx, v))
let err_composite_format d v = err (`Unknown_composite_format(d.ctx, v))
let err_fatal d e            = begin d.state <- Fatal(e); err e end
let set_ctx d ctx            = begin d.ctx <- ctx; end

let miss d count = d.i_max - d.i_pos + 1 < count
let cur_pos d = d.i_pos
let seek_pos pos d =
  if pos > d.i_max then err (`Invalid_offset(d.ctx, pos)) else
    begin d.i_pos <- pos; return () end


let seek_table tag d () =
  match List.find_opt (fun (t, _, _) -> tag = t) d.tables with
  | Some((_, pos, len)) ->
      if pos > d.i_max then
        err (`Invalid_offset(`Table(tag), pos))
      else
        begin
          set_ctx d (`Table(tag));
          d.i_pos <- pos;
          return (Some(len))
        end

  | None -> return None


let seek_required_table tag d () =
  seek_table tag d () >>= function
    | Some(_) -> return ()
    | None    -> err (`Missing_required_table(tag))

let d_skip len d =
  if miss d len then err_eoi d else
    begin d.i_pos <- d.i_pos + len; Ok() end

let raw_byte d =
  let j = d.i_pos in
    begin d.i_pos <- d.i_pos + 1; unsafe_byte d.i j end

let d_bytes len d =
  if miss d len then err_eoi d else
    let start = d.i_pos in
      begin d.i_pos <- d.i_pos + len; return (String.sub d.i start len) end

let d_uint8 d = if miss d 1 then err_eoi d else return (raw_byte d)
let d_int8 d =
  d_uint8 d >>= fun i ->
  return (if i > 0x7F then i - 0x100 else i)

let d_uint16 d =
  if miss d 2 then err_eoi d else
    let b0 = raw_byte d in
    let b1 = raw_byte d in
    return ((b0 lsl 8) lor b1)

let d_int16 d =
  d_uint16 d >>= fun i ->
  return (if i > 0x7FFF then i - 0x10000 else i)

let d_uint24 d =
  if miss d 3 then err_eoi d else
    let b0 = raw_byte d in
    let b1 = raw_byte d in
    let b2 = raw_byte d in
    return ((b0 lsl 16) lor (b1 lsl 8) lor b2)

let d_uint32 d =
  let open WideInt in
    if miss d 4 then
      err_eoi d
    else
      let b0 = !% (raw_byte d) in
      let b1 = !% (raw_byte d) in
      let b2 = !% (raw_byte d) in
      let b3 = !% (raw_byte d) in
      return ((b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3)


let d_uint32_int d =
  if miss d 4 then err_eoi d else
    let b0 = raw_byte d in let b1 = raw_byte d in
    let b2 = raw_byte d in let b3 = raw_byte d in
    let s0 = (b0 lsl 8) lor b1 in
    let s1 = (b2 lsl 8) lor b3 in
    return ((s0 lsl 16) lor s1)


let d_int32 d =
  let open WideInt in
    d_uint32 d >>= fun i ->
    return (if i > of_int64 0x7FFFFFFFL then i -% of_int64 0x10000000L else i)


let d_time d =                       (* LONGDATETIME as a unix time stamp. *)
  if miss d 8 then
    err_eoi d
  else
    let b0 = raw_byte d in let b1 = raw_byte d in
    let b2 = raw_byte d in let b3 = raw_byte d in
    let b4 = raw_byte d in let b5 = raw_byte d in
    let b6 = raw_byte d in let b7 = raw_byte d in
    let s0 = !% ((b0 lsl 8) lor b1) in
    let s1 = !% ((b2 lsl 8) lor b3) in
    let s2 = !% ((b4 lsl 8) lor b5) in
    let s3 = !% ((b6 lsl 8) lor b7) in
    let v = WideInt.((s0 lsl 48) lor (s1 lsl 32) lor (s2 lsl 16) lor s3) in
    let unix_epoch = !%% 2_082_844_800L (* in seconds since 1904-01-01 00:00:00 *) in
    return (v -% unix_epoch)


let d_fixed d =
  if miss d 4 then err_eoi d else
    let b0 = raw_byte d in let b1 = raw_byte d in
    let b2 = raw_byte d in let b3 = raw_byte d in
    let s0 = Int32.of_int ((b0 lsl 8) lor b1) in
    let s1 = Int32.of_int ((b2 lsl 8) lor b3) in
    return (s0, s1)

let d_f2dot14 d =
  d_int16 d >>= fun v ->
  return ((float v) /. 16384.0)

let d_utf_16be len (* in bytes *) d =            (* returns an UTF-8 string. *)
  let rec add_utf_8 b i = function
    | `Malformed(_) -> add_utf_8 b i (`Uchar(Uutf.u_rep))
    | `Uchar(u)     -> begin Uutf.Buffer.add_utf_8 b u; b end
  in
  d_bytes len d >>= fun s ->
  Buffer.clear d.buf;
  return (Buffer.contents (Uutf.String.fold_utf_16be add_utf_8 d.buf s))


type device_table = int * int * int * int


let d_device_table d =
  d_uint16 d >>= fun startSize ->
  d_uint16 d >>= fun endSize ->
  d_uint16 d >>= fun deltaFormat ->
  d_uint16 d >>= fun deltaValue ->
  return (startSize, endSize, deltaFormat, deltaValue)


type 'a ok = ('a, error) result


let confirm b e =
  if not b then err e else return ()

let confirm_version d version versionreq =
  if version <> versionreq then err_version d versionreq else return ()

let d_repeat n df d =
  let rec aux acc i =
    if i <= 0 then return (Alist.to_list acc) else
    df d >>= fun data ->
    aux (Alist.extend acc data) (i - 1)
  in
  aux Alist.empty n


let d_list df d =
  d_uint16 d >>= fun count ->
  Format.fprintf fmtgen "(d_list) count = %d\n" count;
  d_repeat count df d


let d_list_filtered df predicate d =
  let rec aux acc imax i =
    if i >= imax then return (Alist.to_list acc) else
    df d >>= fun data ->
    if predicate i then
      aux (Alist.extend acc data) imax (i + 1)
    else
      aux acc imax (i + 1)
  in
    d_uint16 d >>= fun count ->
    Format.fprintf fmtgen "(d_list_filtered) count = %d\n" count;
    aux Alist.empty count 0


let d_list_access df ireq d =
  let rec aux imax i =
    if i >= imax then return None else
    df d >>= fun data ->
    if i = ireq then
      return (Some(data))
    else
      aux imax (i + 1)
  in
    d_uint16 d >>= fun count ->
    aux count 0


let d_offset_list (offset_origin : int) d : (int list) ok =
  d_list d_uint16 d >>= fun reloffsetlst ->
  return (reloffsetlst |> List.map (fun reloffset -> offset_origin + reloffset))


let d_offset (offset_origin : int) d : int ok =
  d_uint16 d >>= fun reloffset ->
  return (offset_origin + reloffset)


let d_long_offset_list d : (int list) ok =
  d_uint32_int d >>= fun count ->
  d_repeat count d_uint32_int d


let d_offset_opt (offset_origin : int) d : (int option) ok =
  d_uint16 d >>= fun reloffset ->
  if reloffset = 0 then
    return None
  else
    return (Some(offset_origin + reloffset))


let d_fetch offset_origin df d =
  let pos_before = cur_pos d in
  Format.fprintf fmtgen "(d_fetch) | pos_before = %d\n" pos_before;
  d_offset offset_origin d >>= fun offset ->
  Format.fprintf fmtgen "          | rel_offset = %d\n" (offset - offset_origin);
  Format.fprintf fmtgen "          | offset     = %d\n" offset;
  seek_pos offset d >>= fun () ->
  df d >>= fun res ->
  seek_pos (pos_before + 2) d >>= fun () ->
  return res


let d_fetch_opt offset_origin df d =
  let pos_before = cur_pos d in
  d_offset_opt offset_origin d >>= function
    | None ->
        Format.fprintf fmtgen "(d_fetch_opt) | pos_before = %d\n" pos_before;
        Format.fprintf fmtgen "              | NULL\n";
        seek_pos (pos_before + 2) d >>= fun () ->
        return None

    | Some(offset) ->
        Format.fprintf fmtgen "(d_fetch_opt) | pos_before = %d\n" pos_before;
        Format.fprintf fmtgen "              | non-NULL\n";
        seek_pos offset d >>= fun () ->
        df d >>= fun res ->
        seek_pos (pos_before + 2) d >>= fun () ->
        return (Some(res))


let d_fetch_list offset_origin df d =
  let pos_before = cur_pos d in
  Format.fprintf fmtgen "(d_fetch_list) | pos_before = %d\n" pos_before;
  d_offset_opt offset_origin d >>= function
    | None ->
        Format.fprintf fmtgen "               | NULL\n";
        seek_pos (pos_before + 2) d >>= fun () ->
        return []

    | Some(offset) ->
        Format.fprintf fmtgen "               | non-NULL\n";
        seek_pos offset d >>= fun () ->
        df d >>= fun lst ->
        seek_pos (pos_before + 2) d >>= fun () ->
        return lst


let d_fetch_long offset_origin df d =
  let pos_before = cur_pos d in
  d_uint32_int d >>= fun reloffset ->
  let offset = offset_origin + reloffset in
  seek_pos offset d >>= fun () ->
  df d >>= fun res ->
  seek_pos (pos_before + 4) d >>= fun () ->
  return (offset, res)


let rec d_table_records d count =
  if count = 0 then begin d.state <- Ready; return () end else
    d_uint32     d >>= fun tag ->
    d_skip 4     d >>= fun () ->
    d_uint32_int d >>= fun off ->
    d_uint32_int d >>= fun len ->
    d.tables <- (tag, off, len) :: d.tables;
    d_table_records d (count - 1)


let d_version is_ttc_element d =
  d_uint32 d >>= function
    | t  when t = Tag.v_OTTO      -> begin d.flavour <- CFF     ; return true end
    | t  when t = Tag.v_true      -> begin d.flavour <- TTF_true; return true end
    | t  when t = !%% 0x00010000L -> begin d.flavour <- TTF_OT  ; return true end
    | t  when t = Tag.v_ttcf      -> if is_ttc_element then err `Layered_ttc else return false
    | t                           -> err (`Unknown_flavour(t))


let d_structure d =                   (* offset table and table directory. *)
  d_uint16       d >>= fun count ->                          (* numTables. *)
  d_skip (3 * 2) d >>= fun () ->
  set_ctx d `Table_directory;                          (* table directory. *)
  d_table_records d count


let d_ttc_header d : (ttc_element list) ok =
  d_uint32 d >>= function
  | version_ttc  when version_ttc = !%% 0x00010000L || version_ttc = !%% 0x00020000L ->
      d_long_offset_list d >>= fun offsetlst ->
      return (offsetlst |> List.map (fun offset -> (offset, d)))
  | version_ttc ->
      err_version d version_ttc


let decoder src =
  let (i, i_pos, i_max) =
    match src with
    | `String(s) -> (s, 0, String.length s - 1)
  in
  let d =
    { i; i_pos; i_max;
      state = Start;
      ctx = `Offset_table;
      flavour = TTF_OT;    (* dummy initial value *)
      tables = [];         (* dummy initial value *)
      loca_pos_and_format = None; glyf_pos = None;
      buf = Buffer.create 253; }
  in
  d_version false d >>= fun is_single ->
  if is_single then
    return (SingleDecoder(d))
  else
    d_ttc_header d >>= fun ttc ->
    return (TrueTypeCollection(ttc))


let decoder_of_ttc_element ttcelem =
  let (offset, d) = ttcelem in
  let delem =
    { i = d.i;  i_pos = d.i_pos;  i_max = d.i_max;
      state = Start;
      ctx = `Offset_table;
      flavour = d.flavour;
      tables = d.tables;
      loca_pos_and_format = d.loca_pos_and_format;
      glyf_pos = d.glyf_pos;
      buf = Buffer.create 253; }
  in
  seek_pos offset delem >>= fun () ->
  d_version true delem >>= fun _ ->
  return delem


let init_decoder d =
  match d.state with
  | Ready    -> begin d.ctx <- `Table_directory; return () end
  | Fatal(e) -> err e
  | Start    ->
      match d_structure d with
      | Ok(()) as ok -> ok
      | Error(e)     -> err_fatal d e


let flavour d =
    init_decoder d >>= fun () -> return (d.flavour)


let table_list d =
  let tags d = List.rev_map (fun (t, _, _) -> t) d.tables in
    init_decoder d >>= fun () -> return (tags d)


let table_mem d tag =
  let exists_tag tag d = List.exists (fun (t, _, _) -> tag = t) d.tables in
    init_decoder d >>= fun () -> return (exists_tag tag d)


let table_raw d tag =
  init_decoder   d >>=
  seek_table tag d >>= function
    | None      -> return None
    | Some(len) -> d_bytes len d >>= fun bytes -> return (Some(bytes))


(* convenience *)

let glyph_count d =
  init_decoder d >>=
  seek_required_table Tag.maxp d >>= fun () ->
  d_skip 4 d >>= fun () ->
  d_uint16 d >>= fun count ->
  return count


let postscript_name d = (* rigorous postscript name lookup, see OT spec p. 39 *)
  init_decoder d >>=
  seek_required_table Tag.name d >>= fun () ->
  let pos_name = cur_pos d in
  d_uint16 d >>= fun version ->
  if version > 1 then err_version d (!% version) else
  d_uint16 d >>= fun ncount ->
  d_uint16 d >>= fun soff ->
  let rec loop ncount () =
    if ncount = 0 then return None else
    let ncount' = ncount - 1 in
    let look_for the_eid the_lid decode =
      d_uint16 d >>= fun eid ->
      if eid <> the_eid then d_skip (4 * 2) d >>= loop ncount' else
      d_uint16 d >>= fun lid ->
      if lid <> the_lid then d_skip (3 * 2) d >>= loop ncount' else
      d_uint16 d >>= fun nid ->
      if nid <> 6 then d_skip (2 * 2) d >>= loop ncount' else
      d_uint16 d >>= fun len ->
      d_uint16 d >>= fun off ->
      seek_pos (pos_name + soff + off) d >>= fun () ->
      decode len d >>= fun name ->
      let invalid name = err (`Invalid_postscript_name(name)) in
      let name_len = String.length name in
      if name_len > 63 then invalid name else
      try
        for i = 0 to name_len - 1 do
          match Char.code name.[i] with
          | d when d < 33 || d > 126                          -> raise Exit
          | 91 | 93 | 40 | 41 | 123 | 125 | 60 | 62 | 47 | 37 -> raise Exit
          | _                                                 -> ()
        done;
        return (Some(name))
      with Exit -> invalid name
    in
    d_uint16 d >>= function
      | 3 -> look_for 1 0x409 d_utf_16be
      | 1 -> look_for 0 0 d_bytes
      | _ -> d_skip (5 * 2) d >>= loop (ncount - 1)
  in
  loop ncount ()


(* cmap table *)

type glyph_id = int
type map_kind = [ `Glyph | `Glyph_range ]

let rec d_array el count i a d =
  if i = count then return a else
  el d >>= fun v ->
  a.(i) <- v;
  d_array el count (i + 1) a d

let d_cmap_4_ranges d f acc u0s u1s delta offset count =       (* ugly. *)
  let garray_pos = cur_pos d in
  let rec loop acc i =
    if i = count then return acc else
    let i' = i + 1 in
    let offset = offset.(i) in
    let delta = delta.(i) in
    let u0 = u0s.(i) in if not (is_cp u0) then err (`Invalid_cp u0) else
    let u1 = u1s.(i) in if not (is_cp u1) then err (`Invalid_cp u1) else
    if u0 > u1 then err (`Invalid_cp_range (u0, u1)) else
    if offset = 0 then begin
      (* The arithmetic must be performed mod 65536, this is problematic
         for Otfm's interface semantics. We need to split the range
         if the glyph range spans the bounds. *)
      let g0 = u0 + delta in
      let g1 = u1 + delta in
      if g0 < 0 && g1 >= 0 then
        let acc' = f acc `Glyph_range (u0, - delta - 1) (g0 land 65535) in
        loop (f acc' `Glyph_range (- delta, u1) 0) i'
      else
      if g0 <= 65535 && g1 > 65535 then
        let acc' = f acc `Glyph_range (u0, 65535 - delta) g0 in
        loop (f acc' `Glyph_range (65536 - delta, u1) 0) i'
      else (* glyph range is inside [0;65535] or completly outside *)
      loop (f acc `Glyph_range (u0, u1) (g0 land 65535)) i'
    end else begin
      let rec garray acc u u1 () =
        if u > u1 then return acc else
        d_uint16 d >>= fun gindex ->
        let g = (gindex + delta) land 65535 in
        garray (f acc `Glyph (u, u) g) (u + 1) u1 ()
      in
      let pos = garray_pos - (count - i) * 2 + offset in
      seek_pos pos d >>=
      garray acc u0 u1 >>= fun acc ->
      loop acc i'
    end
  in
  loop acc 0


let d_cmap_4 d f acc =
  (* -- now the position is set immediately AFTER the format number entry -- *)
  d_skip (2 * 2) d >>= fun () ->
  d_uint16       d >>= fun count2 ->
  let count = count2 / 2 in
  let a () = Array.make count 0 in
  d_skip (3 * 2)                  d >>= fun () ->
  d_array d_uint16 count 0 (a ()) d >>= fun u1s ->
  d_skip 2                        d >>= fun () -> (* pad *)
  d_array d_uint16 count 0 (a ()) d >>= fun u0s ->
  d_array d_int16  count 0 (a ()) d >>= fun delta ->
  d_array d_uint16 count 0 (a ()) d >>= fun offset ->
  d_cmap_4_ranges d f acc u0s u1s delta offset count


(* -- cmap Format 12: Segmented coverage
      cmap Format 13: Many-to-one range mappings -- *)

let d_cp d =
  d_uint32_int d >>= fun u ->
  if not (is_cp u) then err (`Invalid_cp(u)) else
  return u


let rec d_cmap_groups d count f kind acc =
  if count = 0 then
    return acc
  else
    d_cp d >>= fun startCharCode ->
    d_cp d >>= fun endCharCode ->
    if startCharCode > endCharCode then err (`Invalid_cp_range(startCharCode, endCharCode)) else
    d_uint32_int d >>= fun startGlyphID ->
    d_cmap_groups d (count - 1) f kind (f acc kind (startCharCode, endCharCode) startGlyphID)


let d_cmap_seg kind d f acc =
  (* -- now the position is set immediately AFTER the format number entry -- *)
  d_skip (1 * 2 + 2 * 4) d >>= fun () ->
  d_uint32_int           d >>= fun nGroups ->
  d_cmap_groups d nGroups f kind acc


let d_cmap_12 d f acc = d_cmap_seg `Glyph_range d f acc
let d_cmap_13 d f acc = d_cmap_seg `Glyph d f acc


type cmap_subtable = (decoder * int) * (int * int * int)


let rec d_encoding_record offset_cmap d : cmap_subtable ok =
  d_uint16     d            >>= fun platformID ->
  Format.fprintf debugfmt "platformID = %d\n" platformID;  (* for debug *)
  d_uint16     d            >>= fun encodingID ->
  Format.fprintf debugfmt "encodingID = %d\n" encodingID;  (* for debug *)
  d_fetch_long offset_cmap d_uint16 d >>= fun (offset, format) ->
  Format.fprintf debugfmt "offset = %d\n" offset;  (* for debug *)
  Format.fprintf debugfmt "format = %d\n" format;  (* for debug *)
  return ((d, offset), (platformID, encodingID, format))
(*
  let cur = cur_pos d in
  seek_table_pos pos d >>= fun () ->
  d_uint16           d >>= fun fmt ->
  seek_pos cur       d >>= fun () ->
  d_cmap_records d (count - 1) ((pos, pid, eid, fmt) :: acc)
*)

(*
let select_cmap cmaps =
  let rec loop f sel =
    function
    | (_, _, _, (4 | 12 | 13 as f') as c) :: cs
        when f' > f -> loop f (Some(c)) cs
    | _ :: cs       -> loop f sel cs
    | []            -> sel
  in
    loop min_int None cmaps
*)


let cmap d : (cmap_subtable list) ok =
  init_decoder d >>=
  seek_required_table Tag.cmap d >>= fun () ->
  let offset_cmap = cur_pos d in
  d_uint16 d >>= fun version ->                           (* cmap header. *)
  if version <> 0 then err_version d (!% version) else
  d_list (d_encoding_record offset_cmap) d >>= fun rawsubtbllst ->
  let subtbllst =
    rawsubtbllst |> List.filter (fun (_, (pid, eid, format)) ->
      Format.fprintf debugfmt "(pid, eid, format = %d, %d, %d)" pid eid format;  (* for debug *)
      match format with
      | (4 | 12 | 13) ->
          begin
            match (pid, eid) with
            | (0, _)   (* Unicode *)
            | (3, 1)   (* Windows, UCS-2 *)
            | (3, 10)  (* Windows, UCS-4 *)
            | (1, _)   (* Macintosh *)
                -> true

            | _ -> false
          end

      | _ -> false (* -- unsupported subtable format -- *)
    )
  in
  return subtbllst


let cmap_subtable_ids (subtbl : cmap_subtable) =
  let (_, ids) = subtbl in
    ids


let cmap_subtable (subtbl : cmap_subtable) f acc =
  let ((d, offset), _) = subtbl in
  Format.fprintf debugfmt "subtable offset = %d\n" offset;
  seek_pos offset d >>= fun () ->
  (* -- now the position is set at the beginning of the designated cmap subtable --  *)
  d_uint16 d >>= fun format ->
  Format.fprintf debugfmt "subtable format = %d\n" format;
  match format with
  | 4  -> d_cmap_4 d f acc
  | 12 -> d_cmap_12 d f acc
  | 13 -> d_cmap_13 d f acc
  | _  -> err (`Unsupported_cmap_format(format))


(*
  d_uint16 d >>= fun count ->                               (* numTables. *)
  d_cmap_records d count [] >>= fun cmaps ->
  match select_cmap cmaps with
  | None ->
      let drop_pos (_, pid, eid, fmt) = (pid, eid, fmt) in
      err (`Unsupported_cmaps (List.map drop_pos cmaps))

  | Some(pos, pid, eid, fmt) ->
      let d_cmap =
        match fmt with
        | 4  -> d_cmap_4
        | 12 -> d_cmap_12
        | 13 -> d_cmap_13
        | _  -> assert false
      in
      seek_table_pos pos d >>= d_cmap (pid, eid, fmt) d f acc
*)

(* glyf table *)

type glyf_loc = int

type glyph_simple_descr = (bool * int * int) list list

type glyph_composite_descr =
  (glyph_id * (int * int) * (float * float * float * float) option) list

type glyph_descr =
  [ `Simple of glyph_simple_descr
  | `Composite of glyph_composite_descr ] * (int * int * int * int)


let init_glyf d () =
  match d.glyf_pos with
  | Some(pos) ->
      return pos

  | None ->
      seek_required_table Tag.glyf d () >>= fun () ->
      let pos = d.i_pos in
      d.glyf_pos <- Some(pos);
      return pos


let d_rev_end_points d ccount =
  let rec loop i acc =
    if i <= 0 then return acc else
    d_uint16 d >>= fun e -> loop (i - 1) (e :: acc)
  in
  loop ccount []

let d_rev_flags d pt_count =
  let rec loop i acc =
    if i <= 0 then return acc else
    d_uint8 d >>= fun f ->
    if f land 8 = 0 then loop (i - 1) (f :: acc) else
    d_uint8 d >>= fun n ->
    let rec push n acc = if n = 0 then acc else push (n - 1) (f :: acc) in
    loop (i - 1 - n) (push (n + 1) acc)
  in
  loop pt_count []

let d_rev_coord short_mask same_mask d flags =
  let rec loop x acc = function
  | f :: fs ->
      if f land short_mask > 0 then begin
        d_uint8 d >>= fun dx ->
        let x = x + (if f land same_mask > 0 then dx else -dx) in
        loop x (x :: acc) fs
      end else begin
        if f land same_mask > 0 then loop x (x :: acc) fs else
        d_int16 d >>= fun dx ->
        let x = x + dx in
        loop x (x :: acc) fs
      end
  | [] -> return acc
  in
  loop 0 [] flags

let d_rev_xs d flags = d_rev_coord 2 16 d flags
let d_rev_ys d flags = d_rev_coord 4 32 d flags

let d_simple_glyph d ccount =
  if ccount = 0 then return [] else
  d_rev_end_points d ccount
  >>= fun rev_epts ->
  let pt_count = match rev_epts with [] -> 0 | e :: _ -> e + 1 in
  d_uint16 d
  >>= fun ins_len -> d_skip ins_len d
  >>= fun () -> d_rev_flags d pt_count
  >>= fun rev_flags ->
  let flags = List.rev rev_flags in
  d_rev_xs d flags
  >>= fun rxs -> d_rev_ys d flags
  >>= fun rys ->
  let rec combine repts flags rxs rys i acc =
    match flags with
    | []      -> acc
    | f :: fs ->
        let (new_contour, repts) =
          match repts with
          | []                 -> (false, [])
          | e :: es when e = i -> (true, es)
          | es                 -> (false, es)
        in
        match acc with
        | c :: cs ->
            let new_pt = (f land 1 > 0,  List.hd rxs, List.hd rys) in
            let acc' =
              if new_contour then [new_pt] :: c :: cs else
              (new_pt :: c) :: cs
            in
            combine repts fs (List.tl rxs) (List.tl rys) (i - 1) acc'
        | _ -> assert false
  in
  return (combine (List.tl rev_epts) rev_flags rxs rys (pt_count - 1) ([] :: []))


let d_composite_glyph d =
  let rec loop acc =
    d_uint16 d >>= fun flags ->
    d_uint16 d >>= fun gid ->
    if flags land 2 = 0 then
      err `Unsupported_glyf_matching_points
    else
      let dec = if flags land 1 > 0 then d_int16 else d_int8 in
      dec d >>= fun dx ->
      dec d >>= fun dy ->
      begin
        if flags land 8 > 0 then  (* -- scale -- *)
          d_f2dot14 d >>= fun s ->
          return (Some(s, 0., 0., s))
        else if flags land 64 > 0 then  (* -- xy scale -- *)
          d_f2dot14 d >>= fun sx ->
          d_f2dot14 d >>= fun sy ->
          return (Some(sx, 0., 0., sy))
        else if flags land 128 > 0 then  (* -- m2 -- *)
          d_f2dot14 d >>= fun a ->
          d_f2dot14 d >>= fun b ->
          d_f2dot14 d >>= fun c ->
          d_f2dot14 d >>= fun d ->
          return (Some(a, b, c, d))
        else
          return None
      end >>= fun m ->
      let accnew = Alist.extend acc (gid, (dx, dy), m) in
      if flags land 32 > 0 then
        loop accnew
      else
        return (Alist.to_list accnew)
    in
    loop Alist.empty


let glyf d loc =
  init_decoder d >>=
  init_glyf d >>= fun pos ->
  seek_pos (pos + loc) d  >>= fun () ->
  d_int16 d >>= fun ccount ->
  d_int16 d >>= fun xmin ->
  d_int16 d >>= fun ymin ->
  d_int16 d >>= fun xmax ->
  d_int16 d >>= fun ymax ->
  if ccount < -1 then
    err_composite_format d ccount
  else if ccount = -1 then
    d_composite_glyph d >>= fun components ->
    return (`Composite(components), (xmin, ymin, xmax, ymax))
  else
    d_simple_glyph d ccount >>= fun contours ->
    return (`Simple(contours), (xmin, ymin, xmax, ymax))


(* head table *)

type head = {
  head_font_revision : wint;
  head_flags : int;
  head_units_per_em : int;
  head_created : wint;
  head_modified : wint;
  head_xmin : int;
  head_ymin : int;
  head_xmax : int;
  head_ymax : int;
  head_mac_style : int;
  head_lowest_rec_ppem : int;
  head_index_to_loc_format : loc_format;
}

let head d =
  init_decoder d >>=
  seek_required_table Tag.head d >>= fun () ->
  d_uint32 d >>= fun version ->
  if version <> !%% 0x00010000L then err_version d version else
  d_uint32 d >>= fun head_font_revision ->
  d_skip 8 d >>= fun () -> (* checkSumAdjustement, magicNumber *)
  d_uint16 d >>= fun head_flags ->
  d_uint16 d >>= fun head_units_per_em ->
  d_time   d >>= fun head_created ->
  d_time   d >>= fun head_modified ->
  d_int16  d >>= fun head_xmin ->
  d_int16  d >>= fun head_ymin ->
  d_int16  d >>= fun head_xmax ->
  d_int16  d >>= fun head_ymax ->
  d_uint16 d >>= fun head_mac_style ->
  d_uint16 d >>= fun head_lowest_rec_ppem ->
  d_skip 2 d >>= fun () -> (* fontDirectionHint *)
  d_uint16 d >>= fun locfmt ->
  begin
    match locfmt with
    | 0 -> return ShortLocFormat
    | 1 -> return LongLocFormat
    | _ -> err (`Invalid_index_to_loc_format(locfmt))
  end >>= fun head_index_to_loc_format ->
  return {
    head_font_revision; head_flags; head_units_per_em; head_created;
    head_modified; head_xmin; head_ymin; head_xmax; head_ymax;
    head_mac_style; head_lowest_rec_ppem; head_index_to_loc_format;
  }

(* hhea table *)

type hhea = {
  hhea_ascender : int;
  hhea_descender : int;
  hhea_line_gap : int;
  hhea_advance_width_max : int;
  hhea_min_left_side_bearing : int;
  hhea_min_right_side_bearing : int;
  hhea_xmax_extent : int;
  hhea_caret_slope_rise : int;
  hhea_caret_slope_run : int;
  hhea_caret_offset : int;
}

let hhea d =
  init_decoder d >>=
  seek_required_table Tag.hhea d >>= fun () ->
  d_uint32 d >>= fun version ->
  if version <> !%% 0x00010000L then err_version d version else
  d_int16  d >>= fun hhea_ascender ->
  d_int16  d >>= fun hhea_descender ->
  d_int16  d >>= fun hhea_line_gap ->
  d_uint16 d >>= fun hhea_advance_width_max ->
  d_int16  d >>= fun hhea_min_left_side_bearing ->
  d_int16  d >>= fun hhea_min_right_side_bearing ->
  d_int16  d >>= fun hhea_xmax_extent ->
  d_int16  d >>= fun hhea_caret_slope_rise ->
  d_int16  d >>= fun hhea_caret_slope_run ->
  d_int16  d >>= fun hhea_caret_offset ->
  return {
    hhea_ascender; hhea_descender; hhea_line_gap; hhea_advance_width_max;
    hhea_min_left_side_bearing; hhea_min_right_side_bearing;
    hhea_xmax_extent; hhea_caret_slope_rise; hhea_caret_slope_run;
    hhea_caret_offset;
  }

(* hmtx table *)

let d_hm_count d =
  seek_required_table Tag.hhea d () >>= fun () ->
  d_skip (4 + 15 * 2) d >>= fun () ->
  d_uint16            d >>= fun hm_count ->
  return hm_count

let rec d_hmetric goffset i f acc last_adv d =
  if i = 0 then return (acc, last_adv) else
  d_uint16 d >>= fun adv ->
  d_int16  d >>= fun lsb ->
  let acc' = f acc (goffset - i) adv lsb in
  d_hmetric goffset (i - 1) f acc' adv d

let rec d_hlsb goffset i f acc adv d =
  if i = 0 then return acc else
  d_int16 d >>= fun lsb ->
  let acc' = f acc (goffset - i) adv lsb in
  d_hlsb goffset (i - 1) f acc' adv d

let hmtx d f acc =
  glyph_count d >>= fun glyph_count ->
  d_hm_count  d >>= fun hm_count ->
  seek_required_table Tag.hmtx d () >>= fun () ->
  d_hmetric hm_count hm_count f acc (-1) d >>= fun (acc, last_adv) ->
  d_hlsb glyph_count (glyph_count - hm_count) f acc last_adv d


let hmtx_single d gid =
  glyph_count d >>= fun glyph_count ->
  d_hm_count  d >>= fun hm_count ->
  seek_required_table Tag.hmtx d () >>= fun () ->
  if gid < hm_count then
    d_skip (4 * gid) d >>= fun () ->
    d_uint16 d >>= fun aw ->
    d_int16  d >>= fun lsb ->
    return (aw, lsb)
  else
    d_skip (4 * (hm_count - 1)) d >>= fun () ->
    d_uint16 d >>= fun aw ->
    d_skip 2 d >>= fun () ->
    d_skip (2 * (gid - hm_count)) d >>= fun () ->
    d_int16  d >>= fun lsb ->
    return (aw, lsb)


(* maxp table *)

type maxp = {
  maxp_num_glyphs : int;
  maxp_max_points : int;
  maxp_max_contours : int;
  maxp_max_composite_points : int;
  maxp_max_composite_contours : int;
  maxp_max_zones : int;
  maxp_max_twilight_points : int;
  maxp_max_storage : int;
  maxp_max_function_defs : int;
  maxp_max_instruction_defs : int;
  maxp_max_stack_elements : int;
  maxp_max_size_of_instructions : int;
  maxp_max_component_elements : int;
  maxp_max_component_depth : int;
}

let maxp d =
  init_decoder d >>=
  seek_required_table Tag.maxp d >>= fun () ->
  d_uint32 d >>= fun version ->
  if version <> !%% 0x00010000L then err_version d version else
  d_uint16 d >>= fun maxp_num_glyphs ->
  d_uint16 d >>= fun maxp_max_points ->
  d_uint16 d >>= fun maxp_max_contours ->
  d_uint16 d >>= fun maxp_max_composite_points ->
  d_uint16 d >>= fun maxp_max_composite_contours ->
  d_uint16 d >>= fun maxp_max_zones ->
  d_uint16 d >>= fun maxp_max_twilight_points ->
  d_uint16 d >>= fun maxp_max_storage ->
  d_uint16 d >>= fun maxp_max_function_defs ->
  d_uint16 d >>= fun maxp_max_instruction_defs ->
  d_uint16 d >>= fun maxp_max_stack_elements ->
  d_uint16 d >>= fun maxp_max_size_of_instructions ->
  d_uint16 d >>= fun maxp_max_component_elements ->
  d_uint16 d >>= fun maxp_max_component_depth ->
  return {
    maxp_num_glyphs;
    maxp_max_points;
    maxp_max_contours;
    maxp_max_composite_points;
    maxp_max_composite_contours;
    maxp_max_zones;
    maxp_max_twilight_points;
    maxp_max_storage;
    maxp_max_function_defs;
    maxp_max_instruction_defs;
    maxp_max_stack_elements;
    maxp_max_size_of_instructions;
    maxp_max_component_elements;
    maxp_max_component_depth;
  }

(* name table *)

(* Source: https://skia.googlecode.com/svn/trunk/src/sfnt/SkOTTable_name.cpp
   BSD3 licensed (c) 2011 Google Inc. *)
let lcid_to_bcp47 = [
  0x0401, "ar-sa";  0x0402, "bg-bg";  0x0403, "ca-es";  0x0404, "zh-tw";
  0x0405, "cs-cz";  0x0406, "da-dk";  0x0407, "de-de";  0x0408, "el-gr";
  0x0409, "en-us";  0x040a, "es-es_tradnl";             0x040b, "fi-fi";
  0x040c, "fr-fr";  0x040d, "he-il";  0x040d, "he";     0x040e, "hu-hu";
  0x040e, "hu";     0x040f, "is-is";  0x0410, "it-it";  0x0411, "ja-jp";
  0x0412, "ko-kr";  0x0413, "nl-nl";  0x0414, "nb-no";  0x0415, "pl-pl";
  0x0416, "pt-br";  0x0417, "rm-ch";  0x0418, "ro-ro";  0x0419, "ru-ru";
  0x041a, "hr-hr";  0x041b, "sk-sk";  0x041c, "sq-al";  0x041d, "sv-se";
  0x041e, "th-th";  0x041f, "tr-tr";  0x0420, "ur-pk";  0x0421, "id-id";
  0x0422, "uk-ua";  0x0423, "be-by";  0x0424, "sl-si";  0x0425, "et-ee";
  0x0426, "lv-lv";  0x0427, "lt-lt";  0x0428, "tg-cyrl-tj";
  0x0429, "fa-ir";  0x042a, "vi-vn";  0x042b, "hy-am";  0x042c, "az-latn-az";
  0x042d, "eu-es";  0x042e, "hsb-de"; 0x042f, "mk-mk";  0x0432, "tn-za";
  0x0434, "xh-za";  0x0435, "zu-za";  0x0436, "af-za";  0x0437, "ka-ge";
  0x0438, "fo-fo";  0x0439, "hi-in";  0x043a, "mt-mt";  0x043b, "se-no";
  0x043e, "ms-my";  0x043f, "kk-kz";  0x0440, "ky-kg";  0x0441, "sw-ke";
  0x0442, "tk-tm";  0x0443, "uz-latn-uz";               0x0443, "uz";
  0x0444, "tt-ru";  0x0445, "bn-in";  0x0446, "pa-in";  0x0447, "gu-in";
  0x0448, "or-in";  0x0449, "ta-in";  0x044a, "te-in";  0x044b, "kn-in";
  0x044c, "ml-in";  0x044d, "as-in";  0x044e, "mr-in";  0x044f, "sa-in";
  0x0450, "mn-cyrl";0x0451, "bo-cn";  0x0452, "cy-gb";  0x0453, "km-kh";
  0x0454, "lo-la";  0x0456, "gl-es";  0x0457, "kok-in"; 0x045a, "syr-sy";
  0x045b, "si-lk";  0x045d, "iu-cans-ca";               0x045e, "am-et";
  0x0461, "ne-np";  0x0462, "fy-nl";  0x0463, "ps-af";  0x0464, "fil-ph";
  0x0465, "dv-mv";  0x0468, "ha-latn-ng";               0x046a, "yo-ng";
  0x046b, "quz-bo"; 0x046c, "nso-za"; 0x046d, "ba-ru";  0x046e, "lb-lu";
  0x046f, "kl-gl";  0x0470, "ig-ng";  0x0478, "ii-cn";  0x047a, "arn-cl";
  0x047c, "moh-ca"; 0x047e, "br-fr";  0x0480, "ug-cn";  0x0481, "mi-nz";
  0x0482, "oc-fr";  0x0483, "co-fr";  0x0484, "gsw-fr"; 0x0485, "sah-ru";
  0x0486, "qut-gt"; 0x0487, "rw-rw";  0x0488, "wo-sn";  0x048c, "prs-af";
  0x0491, "gd-gb";  0x0801, "ar-iq";  0x0804, "zh-hans";0x0807, "de-ch";
  0x0809, "en-gb";  0x080a, "es-mx";  0x080c, "fr-be";  0x0810, "it-ch";
  0x0813, "nl-be";  0x0814, "nn-no";  0x0816, "pt-pt";  0x081a, "sr-latn-cs";
  0x081d, "sv-fi";  0x082c, "az-cyrl-az";               0x082e, "dsb-de";
  0x082e, "dsb";    0x083b, "se-se";  0x083c, "ga-ie";  0x083e, "ms-bn";
  0x0843, "uz-cyrl-uz";               0x0845, "bn-bd";  0x0850, "mn-mong-cn";
  0x085d, "iu-latn-ca";               0x085f, "tzm-latn-dz";
  0x086b, "quz-ec"; 0x0c01, "ar-eg";  0x0c04, "zh-hant";0x0c07, "de-at";
  0x0c09, "en-au";  0x0c0a, "es-es";  0x0c0c, "fr-ca";  0x0c1a, "sr-cyrl-cs";
  0x0c3b, "se-fi";  0x0c6b, "quz-pe"; 0x1001, "ar-ly";  0x1004, "zh-sg";
  0x1007, "de-lu";  0x1009, "en-ca";  0x100a, "es-gt";  0x100c, "fr-ch";
  0x101a, "hr-ba";  0x103b, "smj-no"; 0x1401, "ar-dz";  0x1404, "zh-mo";
  0x1407, "de-li";  0x1409, "en-nz";  0x140a, "es-cr";  0x140c, "fr-lu";
  0x141a, "bs-latn-ba";               0x141a, "bs";     0x143b, "smj-se";
  0x143b, "smj";    0x1801, "ar-ma";  0x1809, "en-ie";  0x180a, "es-pa";
  0x180c, "fr-mc";  0x181a, "sr-latn-ba";               0x183b, "sma-no";
  0x1c01, "ar-tn";  0x1c09, "en-za";  0x1c0a, "es-do";  0x1c1a, "sr-cyrl-ba";
  0x1c3b, "sma-se"; 0x1c3b, "sma";    0x2001, "ar-om";  0x2009, "en-jm";
  0x200a, "es-ve";  0x201a, "bs-cyrl-ba";               0x201a, "bs-cyrl";
  0x203b, "sms-fi"; 0x203b, "sms";    0x2401, "ar-ye";  0x2409, "en-029";
  0x240a, "es-co";  0x241a, "sr-latn-rs";               0x243b, "smn-fi";
  0x2801, "ar-sy";  0x2809, "en-bz";  0x280a, "es-pe";  0x281a, "sr-cyrl-rs";
  0x2c01, "ar-jo";  0x2c09, "en-tt";  0x2c0a, "es-ar";  0x2c1a, "sr-latn-me";
  0x3001, "ar-lb";  0x3009, "en-zw";  0x300a, "es-ec";  0x301a, "sr-cyrl-me";
  0x3401, "ar-kw";  0x3409, "en-ph";  0x340a, "es-cl";  0x3801, "ar-ae";
  0x380a, "es-uy";  0x3c01, "ar-bh";  0x3c0a, "es-py";  0x4001, "ar-qa";
  0x4009, "en-in";  0x400a, "es-bo";  0x4409, "en-my";  0x440a, "es-sv";
  0x4809, "en-sg";  0x480a, "es-hn";  0x4c0a, "es-ni";  0x500a, "es-pr";
  0x540a, "es-us"; ]


type lang = string


let rec d_name_langs pos_name soff ncount d =
  d_skip (ncount * 6 * 2) d >>= fun () ->
  d_uint16                d >>= fun lcount ->
  let rec loop i acc =
    if ncount = 0 then return acc else
    d_uint16 d >>= fun len ->
    d_uint16 d >>= fun off ->
    let cpos = cur_pos d in
    seek_pos (pos_name + soff + off) d >>= fun () ->
    d_utf_16be len d >>= fun lang ->
    seek_pos cpos d >>= fun () ->
    loop (i - 1) ((0x8000 + (ncount - i), lang) :: acc)
  in
  loop ncount []


let rec d_name_records pos_name soff ncount f acc langs seen d =
  let d_iter = d_name_records pos_name soff in
  if ncount = 0 then return acc else
  d_uint16 d >>= fun pid ->
  d_uint16 d >>= fun eid ->
  d_uint16 d >>= fun lid ->
  d_uint16 d >>= fun nid ->
  d_uint16 d >>= fun len ->
  d_uint16 d >>= fun off ->
  match (pid, eid) with
  | ((0 | 2), _) | (3, 1) ->
      let cpos = cur_pos d in
      let n = (nid, lid) in
      if List.mem n seen then
        d_iter (ncount - 1) f acc langs seen d
      else
        seek_pos (pos_name + soff + off) d >>= fun () ->
        d_utf_16be len d >>= fun v ->
        seek_pos cpos  d >>= fun () ->
        let lang = try List.assoc lid langs with Not_found -> "und" in
        let acc' = f acc nid lang v in
        d_iter (ncount - 1) f acc' langs (n :: seen) d

  | _ ->
      d_iter (ncount - 1) f acc langs seen d


let name d f acc =
  init_decoder d >>=
  seek_required_table Tag.name d >>= fun () ->
  let pos_name = cur_pos d in
  d_uint16 d >>= fun version ->
  if version < 0 || version > 1 then err_version d (!% version) else
  d_uint16 d >>= fun ncount ->
  d_uint16 d >>= fun soff ->
  let cpos = cur_pos d in
  (if version = 0 then return [] else d_name_langs pos_name soff ncount d) >>= fun langs ->
  let langs = List.rev_append langs lcid_to_bcp47 in
  seek_pos cpos d >>= fun () ->
  d_name_records pos_name soff ncount f acc langs [] d

(* OS/2 table *)

type os2 =
  { os2_x_avg_char_width : int;
    os2_us_weight_class : int;
    os2_us_width_class : int;
    os2_fs_type : int;
    os2_y_subscript_x_size : int;
    os2_y_subscript_y_size : int;
    os2_y_subscript_x_offset : int;
    os2_y_subscript_y_offset : int;
    os2_y_superscript_x_size : int;
    os2_y_superscript_y_size : int;
    os2_y_superscript_x_offset : int;
    os2_y_superscript_y_offset : int;
    os2_y_strikeout_size : int;
    os2_y_strikeout_position : int;
    os2_family_class : int;
    os2_panose : string; (* 10 bytes *)
    os2_ul_unicode_range1 : wint;
    os2_ul_unicode_range2 : wint;
    os2_ul_unicode_range3 : wint;
    os2_ul_unicode_range4 : wint;
    os2_ach_vend_id : wint;
    os2_fs_selection : int;
    os2_us_first_char_index : int;
    os2_us_last_char_index : int;
    os2_s_typo_ascender : int;
    os2_s_type_descender : int;
    os2_s_typo_linegap : int;
    os2_us_win_ascent : int;
    os2_us_win_descent : int;
    os2_ul_code_page_range_1 : wint option;
    os2_ul_code_page_range_2 : wint option;
    os2_s_x_height : int option;
    os2_s_cap_height : int option;
    os2_us_default_char : int option;
    os2_us_break_char : int option;
    os2_us_max_context : int option; }

let os2 d =
  init_decoder d >>=
  seek_required_table Tag.os2 d >>= fun () ->
  d_uint16 d >>= fun version ->
  if version > 0x0004 then err_version d (!% version) else
  let opt v dec d =
    if version < v then return None else dec d >>= fun v -> return (Some(v))
  in
  d_int16  d >>= fun os2_x_avg_char_width ->
  d_uint16 d >>= fun os2_us_weight_class ->
  d_uint16 d >>= fun os2_us_width_class ->
  d_uint16 d >>= fun os2_fs_type ->
  d_int16  d >>= fun os2_y_subscript_x_size ->
  d_int16  d >>= fun os2_y_subscript_y_size ->
  d_int16  d >>= fun os2_y_subscript_x_offset ->
  d_int16  d >>= fun os2_y_subscript_y_offset ->
  d_int16  d >>= fun os2_y_superscript_x_size ->
  d_int16  d >>= fun os2_y_superscript_y_size ->
  d_int16  d >>= fun os2_y_superscript_x_offset ->
  d_int16  d >>= fun os2_y_superscript_y_offset ->
  d_int16  d >>= fun os2_y_strikeout_size ->
  d_int16  d >>= fun os2_y_strikeout_position ->
  d_int16  d >>= fun os2_family_class ->
  d_bytes 10 d >>= fun os2_panose ->
  d_uint32 d >>= fun os2_ul_unicode_range1 ->
  d_uint32 d >>= fun os2_ul_unicode_range2 ->
  d_uint32 d >>= fun os2_ul_unicode_range3 ->
  d_uint32 d >>= fun os2_ul_unicode_range4 ->
  d_uint32 d >>= fun os2_ach_vend_id ->
  d_uint16 d >>= fun os2_fs_selection ->
  d_uint16 d >>= fun os2_us_first_char_index ->
  d_uint16 d >>= fun os2_us_last_char_index ->
  d_int16  d >>= fun os2_s_typo_ascender ->
  d_int16  d >>= fun os2_s_type_descender ->
  d_int16  d >>= fun os2_s_typo_linegap ->
  d_uint16 d >>= fun os2_us_win_ascent ->
  d_uint16 d >>= fun os2_us_win_descent ->
  opt 0x0001 d_uint32 d >>= fun os2_ul_code_page_range_1 ->
  opt 0x0001 d_uint32 d >>= fun os2_ul_code_page_range_2 ->
  opt 0x0002 d_int16  d >>= fun os2_s_x_height ->
  opt 0x0002 d_int16  d >>= fun os2_s_cap_height ->
  opt 0x0002 d_uint16 d >>= fun os2_us_default_char ->
  opt 0x0002 d_uint16 d >>= fun os2_us_break_char ->
  opt 0x0002 d_uint16 d >>= fun os2_us_max_context ->
  return {
    os2_x_avg_char_width; os2_us_weight_class; os2_us_width_class;
    os2_fs_type; os2_y_subscript_x_size; os2_y_subscript_y_size;
    os2_y_subscript_x_offset; os2_y_subscript_y_offset;
    os2_y_superscript_x_size; os2_y_superscript_y_size;
    os2_y_superscript_x_offset; os2_y_superscript_y_offset;
    os2_y_strikeout_size; os2_y_strikeout_position;
    os2_family_class; os2_panose; os2_ul_unicode_range1;
    os2_ul_unicode_range2; os2_ul_unicode_range3;
    os2_ul_unicode_range4; os2_ach_vend_id; os2_fs_selection;
    os2_us_first_char_index; os2_us_last_char_index;
    os2_s_typo_ascender; os2_s_type_descender; os2_s_typo_linegap;
    os2_us_win_ascent; os2_us_win_descent;
    os2_ul_code_page_range_1; os2_ul_code_page_range_2;
    os2_s_x_height; os2_s_cap_height; os2_us_default_char;
    os2_us_break_char; os2_us_max_context;
  }

(* kern table *)

type kern_info ={
  kern_dir : [ `H | `V ];
  kern_kind : [ `Min | `Kern ];
  kern_cross_stream : bool;
}

let kern_info c =
  {
    kern_dir = (if c land 0x1 > 0 then `H else `V);
    kern_kind = (if c land 0x2 > 0 then `Min else `Kern);
    kern_cross_stream = c land 0x4 > 0;
  }

let rec kern_tables ntables t p acc d =
  if ntables = 0 then return acc else
  d_uint16 d >>= fun version ->
  if version > 0 then err_version d (!% version) else
  d_uint16 d >>= fun len ->
  d_uint16 d >>= fun coverage ->
  let format = coverage lsr 8 in
  let skip acc =
    d_skip (len - 3 * 2) d >>= fun () ->
    kern_tables (ntables - 1) t p acc d
  in
  if format <> 0 then skip acc else
  match t acc (kern_info coverage) with
  | `Skip, acc -> skip acc
  | `Fold, acc ->
      let rec d_pairs len acc d =
        if len < 3 * 2 then d_skip len d >>= fun () -> return acc else
        d_uint16 d >>= fun left ->
        d_uint16 d >>= fun right ->
        d_int16 d >>= fun values ->
        d_pairs (len - 3 * 2) (p acc left right values) d
      in
      d_skip (4 * 2)  d >>= fun () ->
      d_pairs len acc d >>= fun acc ->
      kern_tables (ntables - 1) t p acc d

let kern d t p acc =
  init_decoder d >>=
  seek_table Tag.kern d >>= function
  | None    -> return acc
  | Some(_) ->
      d_uint16 d >>= fun version ->
      if version > 0 then err_version d (!% version) else
      d_uint16 d >>= fun ntables ->
      kern_tables ntables t p acc d

(* loca table *)

let d_loca_format d () =
  d_uint16 d >>= fun i ->
  if i > 1 then
    err_loca_format d i
  else if i = 0 then
    return ShortLocFormat
  else
    return LongLocFormat


let init_loca d () =
  match d.loca_pos_and_format with
  | Some(pair) ->
      return pair

  | None ->
      seek_required_table Tag.head d () >>= fun () ->
      d_skip 50 d >>=
      d_loca_format d >>= fun locfmt ->
      seek_required_table Tag.loca d () >>= fun () ->
      let pos = d.i_pos in
      let pair = (pos, locfmt) in
      d.loca_pos_and_format <- Some(pair);
      return pair


let loca_short pos_loca d gid =
  seek_pos (pos_loca + gid * 2) d >>= fun () ->
  d_uint16 d >>= fun o1 ->
  d_uint16 d >>= fun o2 ->
  let o1 = o1 * 2 in
  let o2 = o2 * 2 in
  if o1 = o2 then return None else return (Some(o1, o2 - o1))


let loca_long pos_loca d gid =
  seek_pos (pos_loca + gid * 4) d >>= fun () ->
  d_uint32_int d >>= fun o1 ->
  d_uint32_int d >>= fun o2 ->
  if o1 = o2 then return None else return (Some(o1, o2 - o1))


let loca_with_length d gid =
  init_decoder d >>=
  init_loca d >>= fun (pos_loca, locfmt) ->
  match locfmt with
  | ShortLocFormat -> loca_short pos_loca d gid
  | LongLocFormat  -> loca_long pos_loca d gid


let loca d gid =
  loca_with_length d gid >>= function
  | Some((loc, _)) -> return (Some(loc))
  | None           -> return None


(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)


(* -- GSUB table -- *)

type gsub_subtable =
  | SingleSubtable    of (glyph_id * glyph_id) list
      (* -- LookupType 1: Single substitution subtable [page 251] -- *)
  | AlternateSubtable of (glyph_id * (glyph_id list)) list
      (* -- LookupType 3: Alternate substitution subtable [page 253] -- *)
  | LigatureSubtable  of (glyph_id * (glyph_id list * glyph_id) list) list
      (* -- LookupType 4: Ligature substitution subtable [page 254] -- *)
  | UnsupportedGSUBSubtable
  (* temporary; should contain more lookup type *)

(*
let seek_pos_from_list origin scriptTag d =
  let rec aux i =
    if i <= 0 then return false else
    begin
      d_bytes 4 d >>= fun tag ->
      d_uint16 d  >>= fun offset ->
(*        Format.fprintf fmtgen ("| tag = '" ^ tag ^ "'");  (* for debug *) *)
        if String.equal tag scriptTag then
          seek_pos (origin + offset) d >>= fun () ->
          return true
        else
          aux (i - 1)
    end
  in
  d_uint16 d >>= fun scriptCount ->
  Format.fprintf fmtGSUB "scriptCount = %d\n" scriptCount;
  aux scriptCount >>= fun found ->
  return found
*)

let d_range_record d =
  let rec range acc i j =
    if i > j then Alist.to_list acc else
      range (Alist.extend acc i) (i + 1) j
  in
  d_uint16 d >>= fun start_gid ->
  d_uint16 d >>= fun end_gid ->
  d_uint16 d >>= fun _ -> (* -- startCoverageIndex; can be ignored -- *)
  return (range Alist.empty start_gid end_gid)


let d_coverage d : (glyph_id list) ok =
    (* -- the position is supposed to be set
          to the beginning of a Coverage table [page 139] -- *)
  d_uint16 d >>= fun coverageFormat ->
  Format.fprintf fmtgen "{Coverage format = %d\n" coverageFormat;  (* for debug *)
  let res =  (* for debug *)
    match coverageFormat with
    | 1 -> d_list d_uint16 d
    | 2 -> d_list d_range_record d >>= fun rnglst -> return (List.concat rnglst)
    | _ -> err_version d (!% coverageFormat)
  in Format.fprintf fmtgen "end Coverage table}\n"; res  (* for debug *)

(*
let d_coverage offset_origin d : (glyph_id list) ok =
    (* -- the position is supposed to be set
          just before an offset field to a coverage table -- *)
  let pos_first = cur_pos d in
  d_offset offset_origin d   >>= fun offset_Coverage ->
  seek_pos offset_Coverage d >>= fun () ->
  d_coverage d          >>= fun gidlst ->
  seek_pos (pos_first + 2) d >>= fun () ->
  return gidlst
*)

let combine_coverage d coverage lst =
  try return (List.combine coverage lst) with
  | Invalid_argument(_) -> err (`Inconsistent_length_of_coverage(d.ctx))


let d_fetch offset_origin df d =
  let pos_before = cur_pos d in
  Format.fprintf fmtgen "(d_fetch) | pos_before = %d\n" pos_before;
  d_offset offset_origin d >>= fun offset ->
  Format.fprintf fmtgen "          | rel_offset = %d\n" (offset - offset_origin);
  Format.fprintf fmtgen "          | offset     = %d\n" offset;
  seek_pos offset d >>= fun () ->
  df d >>= fun res ->
  seek_pos (pos_before + 2) d >>= fun () ->
  return res


let d_fetch_opt offset_origin df d =
  let pos_before = cur_pos d in
  d_offset_opt offset_origin d >>= function
    | None ->
        Format.fprintf fmtgen "(d_fetch_opt) | pos_before = %d\n" pos_before;
        Format.fprintf fmtgen "              | NULL";
        seek_pos (pos_before + 2) d >>= fun () ->
        return None

    | Some(offset) ->
        Format.fprintf fmtgen "(d_fetch_opt) | pos_before = %d\n" pos_before;
        Format.fprintf fmtgen "              | non-NULL\n";
        seek_pos offset d >>= fun () ->
        df d >>= fun res ->
        seek_pos (pos_before + 2) d >>= fun () ->
        return (Some(res))


let d_fetch_list offset_origin df d =
  let pos_before = cur_pos d in
  Format.fprintf fmtgen "(d_fetch_list) | pos_before = %d\n" pos_before;
  d_offset_opt offset_origin d >>= function
    | None ->
        Format.fprintf fmtgen "               | NULL\n";
        seek_pos (pos_before + 2) d >>= fun () ->
        return []

    | Some(offset) ->
        Format.fprintf fmtgen "               | non-NULL\n";
        seek_pos offset d >>= fun () ->
        df d >>= fun lst ->
        seek_pos (pos_before + 2) d >>= fun () ->
        return lst


let seek_every_pos (type a) (offsetlst : int list) (df : decoder -> a ok) (d : decoder) : (a list) ok =
  let rec aux acc offsetlst =
  match offsetlst with
  | []             -> return (Alist.to_list acc)
  | offset :: tail ->
(*      let () = Format.fprintf fmtgen ("| offset = " ^ (string_of_int offset)) in  (* for debug *) *)
      seek_pos offset d >>= fun () ->
      df d >>= fun data ->
      aux (Alist.extend acc data) tail
  in
    aux Alist.empty offsetlst


let d_with_coverage (type a) (offset_Substitution_table : int) (df : decoder -> a ok) d : ((glyph_id * a) list) ok =
    (* -- the position is supposed to be set
          just before a Coverage field and a subsequent offset list
          [page 254 etc.] -- *)
  d_fetch offset_Substitution_table d_coverage d >>= fun coverage ->
  Format.fprintf fmtgen "size of Coverage = %d\n" (List.length coverage);  (* for debug *)
  List.iter (Format.fprintf fmtgen "  *   elem = %d\n") coverage;  (* for debug *)

    (* -- the position is set just before LigSetCount field [page 254] -- *)
  d_list (d_fetch offset_Substitution_table df) d >>= fun datalst ->
  combine_coverage d coverage datalst


type gxxx_script = decoder * string * int * int * int

type gsub_script = gxxx_script

type gpos_script = gxxx_script


let gxxx_script_tag (_, scriptTag, _, _, _) = scriptTag

let gsub_script_tag = gxxx_script_tag

let gpos_script_tag = gxxx_script_tag


let d_tag_offset_record offset_Gxxx d =
  d_bytes 4 d >>= fun tag ->
  d_offset offset_Gxxx d >>= fun offset ->
  Format.fprintf fmtgen "(%s, %d)\n" tag offset;  (* for debug *)
  return (tag, offset)


let d_tag_offset_list d : ((string * int) list) ok =
  let offset_ScriptList = cur_pos d in
  d_list (d_tag_offset_record offset_ScriptList) d


let gxxx_script tag_Gxxx d : (gxxx_script list) ok =
  init_decoder d >>=
  seek_table tag_Gxxx d >>= function
    | None    -> err (`Missing_required_table(tag_Gxxx))
    | Some(_) ->
        let offset_Gxxx = cur_pos d in
        d_uint32 d >>= fun version ->
        confirm (version = !%% 0x00010000L) (e_version d version) >>= fun () ->
        d_fetch offset_Gxxx d_tag_offset_list d >>= fun scriptList ->
        d_offset offset_Gxxx d >>= fun offset_FeatureList ->
        d_offset offset_Gxxx d >>= fun offset_LookupList ->
        let scriptList_res =
          scriptList |> List.map (fun (scriptTag, offset_Script_table) ->
            (d, scriptTag, offset_Script_table, offset_FeatureList, offset_LookupList)
          )
        in
        return scriptList_res


let gsub_script = gxxx_script Tag.gsub

let gpos_script = gxxx_script Tag.gpos


type gxxx_langsys = decoder * string * int * int * int

type gsub_langsys = gxxx_langsys

type gpos_langsys = gxxx_langsys


let gxxx_langsys_tag (_, tag, _, _, _) = tag

let gsub_langsys_tag = gxxx_langsys_tag

let gpos_langsys_tag = gxxx_langsys_tag


let gxxx_langsys (script : gxxx_script) : (gxxx_langsys * gxxx_langsys list) ok =
  let (d, scriptTag, offset_Script_table, offset_FeatureList, offset_LookupList) = script in
  Format.fprintf fmtGSUB "offset_Script_table = %d\n" offset_Script_table;  (* for debug *)
  seek_pos offset_Script_table d >>= fun () ->
  d_offset offset_Script_table d >>= fun offset_DefaultLangSys ->
  d_list (d_tag_offset_record offset_Script_table) d >>= fun langSysList ->
  let defaultLangSys_res =
    (d, "DFLT", offset_DefaultLangSys, offset_FeatureList, offset_LookupList)
  in
  let langSysList_res =
    langSysList |> List.map (fun (langSysTag, offset_LangSys_table) ->
      (d, langSysTag, offset_LangSys_table, offset_FeatureList, offset_LookupList)
    )
  in
  return (defaultLangSys_res, langSysList_res)


let gsub_langsys = gxxx_langsys

let gpos_langsys = gxxx_langsys


type gxxx_feature = decoder * string * int * int

type gsub_feature = gxxx_feature

type gpos_feature = gxxx_feature


let gxxx_feature_tag (_, tag, _, _) = tag

let gsub_feature_tag = gxxx_feature_tag

let gpos_feature_tag = gxxx_feature_tag


let gxxx_feature (langsys : gxxx_langsys) : (gxxx_feature option * gxxx_feature list) ok =
  let (d, langSysTag, offset_LangSys_table, offset_FeatureList, offset_LookupList) = langsys in
  seek_pos offset_LangSys_table d >>= fun () ->
    (* -- now the position is set to the beginning of the required LangSys table *)
  d_uint16 d >>= fun offset_LookupOrder ->
  confirm (offset_LookupOrder = 0) (`Invalid_lookup_order(offset_LookupOrder)) >>= fun () ->
  d_uint16 d >>= fun requiredFeatureIndex ->
  d_list d_uint16 d >>= fun featureIndex_list ->
    (* -- now we are going to see FeatureList table -- *)
  Format.fprintf fmtGSUB "offset_FeatureList = %d\n" offset_FeatureList;  (* for debug *)
  seek_pos offset_FeatureList d >>= fun () ->
  d_list_filtered (d_tag_offset_record offset_FeatureList) (fun fi -> List.mem fi featureIndex_list) d >>= fun featureList ->
  let featureList_res =
    featureList |> List.map (fun (featureTag, offset_Feature_table) ->
      (d, featureTag, offset_Feature_table, offset_LookupList)
    )
  in
  let featurereq =
        match requiredFeatureIndex with
        | 0xFFFF -> return None
        | _      ->
            begin
              seek_pos offset_FeatureList d >>= fun () ->
              d_list_access (d_tag_offset_record offset_FeatureList) requiredFeatureIndex d >>= function
              | None           -> err (`Invalid_feature_index(requiredFeatureIndex))
              | Some(_) as opt -> return opt
            end
  in
  featurereq >>= function
  | None ->
      return (None, featureList_res)

  | Some((tagreq, offsetreq)) ->
      let featurereq = (d, tagreq, offsetreq, offset_LookupList) in
      return (Some(featurereq), featureList_res)


let gsub_feature = gxxx_feature

let gpos_feature = gxxx_feature


let gxxx_subtable_list (type a) (lookup_gxxx : decoder -> a ok) (feature : gxxx_feature) : (a list) ok =
  let (d, featureTag, offset_Feature_table, offset_LookupList) = feature in
  Format.fprintf fmtGSUB "offset_Feature_table = %d\n" offset_Feature_table;  (* for debug *)
  seek_pos offset_Feature_table d >>= fun () ->
    (* -- now the position is set to the beginning of the required Feature table -- *)
  Format.fprintf fmtGSUB "---- Feature table ----\n";  (* for debug *)
  d_uint16 d >>= fun featureParams ->
  confirm (featureParams = 0) (`Invalid_feature_params(featureParams)) >>= fun () ->
  d_list d_uint16 d >>= fun lookupListIndexList ->
  Format.fprintf fmtGSUB "offset_LookupList = %d\n" offset_LookupList;  (* for debug *)
  seek_pos offset_LookupList d >>= fun () ->
    (* -- now the position is set to the beginning of the LookupList table -- *)
  d_list_filtered (d_offset offset_LookupList) (fun l -> List.mem l lookupListIndexList) d >>= fun offsetlst_Lookup_table ->
  seek_every_pos offsetlst_Lookup_table lookup_gxxx d


let d_ligature_table d : (glyph_id list * glyph_id) ok =
    (* -- the position is supposed to be set
          to the beginning of a Ligature table [page 255] -- *)
  d_uint16 d >>= fun ligGlyph ->
  d_uint16 d >>= fun compCount ->
  Format.fprintf fmtGSUB "    [ligGlyph = %d ----> compCount = %d]\n" ligGlyph compCount;
  d_repeat (compCount - 1) d_uint16 d >>= fun component ->
  return (component, ligGlyph)


let d_ligature_set_table d : ((glyph_id list * glyph_id) list) ok =
    (* -- the position is supposed to be set
          to the beginning of a LigatureSet table [page 254] -- *)
  let offset_LigatureSet_table = cur_pos d in
  Format.fprintf fmtGSUB "offset_LigatureSet_table = %d\n" offset_LigatureSet_table;
  d_list (d_fetch offset_LigatureSet_table d_ligature_table) d


let d_ligature_substitution_subtable d : ((glyph_id * (glyph_id list * glyph_id) list) list) ok =
    (* -- the position is supposed to be set
          to the beginning of Ligature SubstFormat1 subtable [page 254] -- *)
  let offset_Substitution_table = cur_pos d in
  Format.fprintf fmtGSUB "offset_Substitution_table = %d\n" offset_Substitution_table;  (* for debug *)
  d_uint16 d >>= fun substFormat ->
  Format.fprintf fmtGSUB "substFormat = %d\n" substFormat;  (* for debug *)
  confirm (substFormat = 1) (e_version d (!% substFormat)) >>= fun () ->
  d_with_coverage offset_Substitution_table d_ligature_set_table d


let d_alternate_set_table d : (glyph_id list) ok =
    (* -- the position is supposed to be set
       to the beginning of AlternateSet table [page 253] -- *)
  d_list d_uint16 d


let d_alternate_substitution_subtable d : ((glyph_id * glyph_id list) list) ok =
    (* -- the position is supposed to be set
       to the beginning of Alternate SubstFormat1 subtable [page 253] -- *)
  let offset_Substitution_table = cur_pos d in
  Format.fprintf fmtGSUB "offset_Substitution_table = %d\n" offset_Substitution_table;  (* for debug *)
  d_uint16 d >>= fun substFormat ->
  confirm (substFormat = 1) (e_version d (!% substFormat)) >>= fun () ->
  d_with_coverage offset_Substitution_table d_alternate_set_table d


let d_single_substitution_subtable_format_1 offset_Substitution_table d =
  d_fetch offset_Substitution_table d_coverage d >>= fun coverage ->
  d_uint16 d >>= fun deltaGlyphID ->
  return (coverage |> List.map (fun gid -> (gid, gid + deltaGlyphID)))


let d_single_substitution_subtable_format_2 offset_Substitution_table d =
  d_with_coverage offset_Substitution_table d_uint16 d


let d_single_substitution_subtable d : ((glyph_id * glyph_id) list) ok =
    (* -- the position is supposed to be set
       to the beginning of Single SubstFormat1 or Single SubstFormat2 subtable [page 251] -- *)
  let offset_Substitution_table = cur_pos d in
  Format.fprintf fmtGSUB "offset_Substitution_table = %d\n" offset_Substitution_table;  (* for debug *)
  d_uint16 d >>= fun substFormat ->
  match substFormat with
  | 1 -> d_single_substitution_subtable_format_1 offset_Substitution_table d
  | 2 -> d_single_substitution_subtable_format_2 offset_Substitution_table d
  | _ -> err_version d (!% substFormat)


let lookup_gsub d : gsub_subtable ok =
    (* -- the position is supposed to be set
          to the beginning of a Lookup table [page 137] -- *)
  let offset_Lookup_table = cur_pos d in
  d_uint16 d >>= fun lookupType ->
  Format.fprintf fmtGSUB "# lookupType = %d\n" lookupType;  (* for debug *)
  d_uint16 d >>= fun lookupFlag ->
  Format.fprintf fmtGSUB "# lookupFlag = %d\n" lookupFlag;  (* for debug *)
(*
  let rightToLeft      = 0 < lookupFlag land 1 in
  let ignoreBaseGlyphs = 0 < lookupFlag land 2 in
  let ignoreLigatures  = 0 < lookupFlag land 4 in
  let ignoreMarks      = 0 < lookupFlag land 8 in
*)
  match lookupType with
  | 1 ->  (* -- single substitution -- *)
      Format.fprintf fmtGSUB "LookupType 1\n";
      d_list (d_fetch offset_Lookup_table d_single_substitution_subtable) d >>= fun gid_single_assoc_list ->
      return (SingleSubtable(List.concat gid_single_assoc_list))

  | 2 ->  (* -- multiple substitution -- *)
      return UnsupportedGSUBSubtable  (* temporary *)

  | 3 ->  (* -- alternate substitution -- *)
      Format.fprintf fmtGSUB "LookupType 3\n";
      d_list (d_fetch offset_Lookup_table d_alternate_substitution_subtable) d >>= fun gid_altset_assoc_list ->
      return (AlternateSubtable(List.concat gid_altset_assoc_list))

  | 4 ->  (* -- ligature substitution -- *)
      Format.fprintf fmtGSUB "LookupType 4\n";  (* for debug *)
      d_list (d_fetch offset_Lookup_table d_ligature_substitution_subtable) d >>= fun gidfst_ligset_assoc_list ->
      return (LigatureSubtable(List.concat gidfst_ligset_assoc_list))

  | 5 ->  (* -- contextual substitution subtable -- *)
      return UnsupportedGSUBSubtable  (* temporary *)

  | 6 ->  (* -- chaining contextual substitution subtable -- *)
      return UnsupportedGSUBSubtable  (* temporary *)

  | 7 ->  (* -- extension substitution -- *)
      return UnsupportedGSUBSubtable  (* temporary *)

  | 8 ->  (* -- reverse chaining contextual single substitution subtable -- *)
      return UnsupportedGSUBSubtable  (* temporary *)

  | _ ->
      err (`Invalid_GSUB_lookup_type(lookupType))


type 'a folding_single = 'a -> glyph_id * glyph_id -> 'a

type 'a folding_alt = 'a -> glyph_id * glyph_id list -> 'a

type 'a folding_lig = 'a -> glyph_id * (glyph_id list * glyph_id) list -> 'a


let rec fold_subtables_gsub (f_single : 'a folding_single) (f_alt : 'a folding_alt) (f_lig : 'a folding_lig) (init : 'a) (subtablelst : gsub_subtable list) : 'a =
  let iter = fold_subtables_gsub f_single f_alt f_lig in
    match subtablelst with
    | [] ->
        init

    | SingleSubtable(gid_single_assoc) :: tail ->
        let initnew = List.fold_left f_single init gid_single_assoc in
        iter initnew tail

    | AlternateSubtable(gid_altset_assoc) :: tail ->
        let initnew = List.fold_left f_alt init gid_altset_assoc in
        iter initnew tail

    | LigatureSubtable(gidfst_ligset_assoc) :: tail ->
        let initnew = List.fold_left f_lig init gidfst_ligset_assoc in
        iter initnew tail

    | UnsupportedGSUBSubtable :: tail ->
        iter init tail


let gsub feature f_single f_alt f_lig init =
  gxxx_subtable_list lookup_gsub feature >>= fun subtablelst ->
  return (fold_subtables_gsub f_single f_alt f_lig init subtablelst)


let d_if cond df d =
  if cond then
    df d >>= fun res ->
    return (Some(res))
  else
    return None


type value_format = ValueFormat of int

type value_record = {
  x_placement  : int option;
  y_placement  : int option;
  x_advance    : int option;
  y_advance    : int option;
  x_pla_device : int option;
  y_pla_device : int option;
  x_adv_device : int option;
  y_adv_device : int option;
}


let d_value_format d : value_format ok =
  d_uint16 d >>= fun raw ->
  Ok(ValueFormat(raw))


let d_value_record (ValueFormat(valfmt)) d : value_record ok =
    (* -- the position is supposed to be set
          to the beginning of a ValueRecord table [page 213] -- *)
  d_if (0 < valfmt land   1) d_int16 d >>= fun xPlacement_opt ->
  d_if (0 < valfmt land   2) d_int16 d >>= fun yPlacement_opt ->
  d_if (0 < valfmt land   4) d_int16 d >>= fun xAdvance_opt ->
  d_if (0 < valfmt land   8) d_int16 d >>= fun yAdvance_opt ->
  d_if (0 < valfmt land  16) d_int16 d >>= fun xPlaDevice_opt ->
  d_if (0 < valfmt land  32) d_int16 d >>= fun yPlaDevice_opt ->
  d_if (0 < valfmt land  64) d_int16 d >>= fun xAdvDevice_opt ->
  d_if (0 < valfmt land 128) d_int16 d >>= fun yAdvDevice_opt ->
  return {
    x_placement  = xPlacement_opt;
    y_placement  = yPlacement_opt;
    x_advance    = xAdvance_opt;
    y_advance    = yAdvance_opt;
    x_pla_device = xPlaDevice_opt;
    y_pla_device = yPlaDevice_opt;
    x_adv_device = xAdvDevice_opt;
    y_adv_device = yAdvDevice_opt;
  }

type class_value = int

type class_definition =
  | GlyphToClass      of glyph_id * class_value
  | GlyphRangeToClass of glyph_id * glyph_id * class_value

type gpos_subtable =
  | PairPosAdjustment1 of (glyph_id * (glyph_id * value_record * value_record) list) list
  | PairPosAdjustment2 of class_definition list * class_definition list * (class_value * (class_value * value_record * value_record) list) list
  | ExtensionPos       of gpos_subtable list
  | UnsupportedGPOSSubtable
  (* temporary; must contain more kinds of adjustment subtables *)


let d_class = d_uint16


let d_pair_value_record valfmt1 valfmt2 d : (glyph_id * value_record * value_record) ok =
  d_uint16 d >>= fun secondGlyph ->
  d_value_record valfmt1 d >>= fun value1 ->
  d_value_record valfmt2 d >>= fun value2 ->
  return (secondGlyph, value1, value2)

let d_pair_set valfmt1 valfmt2 d : ((glyph_id * value_record * value_record) list) ok =
  d_list (d_pair_value_record valfmt1 valfmt2) d


let d_class_2_record valfmt1 valfmt2 d : (value_record * value_record) ok =
  d_value_record valfmt1 d >>= fun valrcd1 ->
  d_value_record valfmt2 d >>= fun valrcd2 ->
  return (valrcd1, valrcd2)


let numbering lst =
  let rec aux acc i lst =
    match lst with
    | []           -> Alist.to_list acc
    | head :: tail -> aux (Alist.extend acc (i, head)) (i + 1) tail
  in
    aux Alist.empty 0 lst


let d_class_1_record class2Count valfmt1 valfmt2 d : ((class_value * value_record * value_record) list) ok =
  d_repeat class2Count (d_class_2_record valfmt1 valfmt2) d >>= fun pairlst ->
  try return (numbering pairlst |> List.map (fun (x, (y, z)) -> (x, y, z))) with
  | Invalid_argument(_) -> err `Inconsistent_length_of_class


let d_class_definition_format_1 d : (class_definition list) ok =
  let rec aux acc gidstt lst =
    match lst with
    | []          -> return (Alist.to_list acc)
    | cls :: tail -> aux (Alist.extend acc (GlyphToClass(gidstt, cls))) (gidstt + 1) tail
  in
    d_uint16 d >>= fun startGlyph ->
    d_list d_class d >>= fun classValueArray ->
    aux Alist.empty startGlyph classValueArray


let d_class_range_record d : class_definition ok =
    d_uint16 d >>= fun start_gid ->
    d_uint16 d >>= fun end_gid ->
    d_class d >>= fun cls ->
    return (GlyphRangeToClass(start_gid, end_gid, cls))


let d_class_definition_format_2 d : (class_definition list) ok =
  d_list d_class_range_record d >>= fun rangelst ->
  return rangelst


let d_class_definition d : (class_definition list) ok =
    (* -- the position is supposed to be set
          to the  beginning of a ClassDef table [page 140] -- *)
  d_uint16 d >>= fun classFormat ->
  match classFormat with
  | 1 -> d_class_definition_format_1 d
  | 2 -> d_class_definition_format_2 d
  | _ -> err_version d (!% classFormat)


let d_pair_adjustment_subtable d : gpos_subtable ok =
    (* -- the position is supposed to be set
          to the beginning of a PairPos subtable [page 194] -- *)
  let offset_PairPos = cur_pos d in
  d_uint16 d >>= fun posFormat ->
  match posFormat with
  | 1 ->
      d_fetch offset_PairPos d_coverage d >>= fun coverage ->
      d_value_format d >>= fun valueFormat1 ->
      d_value_format d >>= fun valueFormat2 ->
      d_list (d_fetch offset_PairPos (d_pair_set valueFormat1 valueFormat2)) d >>= fun pairsetlst ->
      combine_coverage d coverage pairsetlst >>= fun comb ->
      return (PairPosAdjustment1(comb))

  | 2 ->
      d_fetch offset_PairPos d_coverage d >>= fun coverage ->
      d_value_format d >>= fun valueFormat1 ->
      d_value_format d >>= fun valueFormat2 ->
      d_fetch offset_PairPos d_class_definition d >>= fun classDef1 ->
      d_fetch offset_PairPos d_class_definition d >>= fun classDef2 ->
      d_uint16 d >>= fun class1Count ->
      d_uint16 d >>= fun class2Count ->
      d_repeat class1Count (d_class_1_record class2Count valueFormat1 valueFormat2) d >>= fun pairposlst ->
      begin
        try return (PairPosAdjustment2(classDef1, classDef2, numbering pairposlst)) with
        | Invalid_argument(_) -> err `Inconsistent_length_of_class
      end

  | _ -> err_version d (!% posFormat)


let lookup_gpos_exact offsetlst_SubTable lookupType d : gpos_subtable ok =
  match lookupType with
  | 1 ->  (* -- Single adjustment -- *)
      return UnsupportedGPOSSubtable
(*
      failwith "Single adjustment; remains to be supported."  (* temporary *)
*)

  | 2 ->  (* -- Pair adjustment -- *)
      Format.fprintf fmtGSUB "number of subtables = %d\n" (List.length offsetlst_SubTable);  (* for debug *)
      seek_every_pos offsetlst_SubTable d_pair_adjustment_subtable d >>= fun subtablelst ->
      return (ExtensionPos(subtablelst))

  | 3 ->  (* -- Cursive attachment -- *)
      return UnsupportedGPOSSubtable
(*
      failwith "Cursive attachment; remains to be supported."  (* temporary *)
*)

  | 4 ->  (* -- MarkToBase attachment -- *)
      return UnsupportedGPOSSubtable
(*
      failwith "MarkToBase attachment; remains to be supported."  (* temporary *)
*)
  | 5 ->
      return UnsupportedGPOSSubtable

  | 6 ->
      return UnsupportedGPOSSubtable

  | 7 ->
      return UnsupportedGPOSSubtable

  | 8 ->
      return UnsupportedGPOSSubtable

  | 9 ->  (* -- Extension positioning [page 213] -- *)
      err `Invalid_extension_position

  | _ ->
      err (`Invalid_GPOS_lookup_type(lookupType))
(*
      failwith "lookupType other; remains to be supported (or font file broken)."  (* temporary *)
*)

let d_extension_position d : gpos_subtable ok =
    (* -- the position is supposed to be set
          to the beginning of ExtensionPosFormat1 subtable [page 213] -- *)
  let offset_ExtensionPos = cur_pos d in
  d_uint16 d >>= fun posFormat ->
  confirm (posFormat = 1) (e_version d (!% posFormat)) >>= fun () ->
  d_uint16 d >>= fun extensionLookupType ->
  d_uint32_int d >>= fun extensionOffset ->
  let offset = offset_ExtensionPos + extensionOffset in
  lookup_gpos_exact [offset] extensionLookupType d


let lookup_gpos d : gpos_subtable ok =
    (* -- the position is supposed to be set
          to the beginning of Lookup table [page 137] -- *)
  let offset_Lookup_table = cur_pos d in
  d_uint16 d >>= fun lookupType ->
  Format.fprintf fmtGSUB "# lookupType = %d\n" lookupType;  (* for debug *)
  d_uint16 d >>= fun lookupFlag ->
  Format.fprintf fmtGSUB "# lookupFlag = %d\n" lookupFlag;  (* for debug *)
(*
  let rightToLeft      = 0 < lookupFlag land 1 in
  let ignoreBaseGlyphs = 0 < lookupFlag land 2 in
  let ignoreLigatures  = 0 < lookupFlag land 4 in
  let ignoreMarks      = 0 < lookupFlag land 8 in
*)
  d_offset_list offset_Lookup_table d >>= fun offsetlst_SubTable ->
  match lookupType with
  | 9 ->
      seek_every_pos offsetlst_SubTable d_extension_position d >>= fun subtablelst ->
      return (ExtensionPos(subtablelst))

  | _ ->
      lookup_gpos_exact offsetlst_SubTable lookupType d


let rec fold_subtables_gpos (f_pair1 : 'a -> glyph_id * (glyph_id * value_record * value_record) list -> 'a) (f_pair2 : class_definition list -> class_definition list -> 'a -> (class_value * (class_value * value_record * value_record) list) list -> 'a) (init : 'a) (subtablelst : gpos_subtable list) : 'a =
  let iter = fold_subtables_gpos f_pair1 f_pair2 in
    match subtablelst with
    | [] ->
        init

    | PairPosAdjustment1(gidfst_pairposlst_assoc) :: tail ->
        let initnew = List.fold_left f_pair1 init gidfst_pairposlst_assoc in
          iter initnew tail

    | PairPosAdjustment2(clsdeflst1, clsdeflst2, cls_pairposlst_assoc) :: tail ->
        let initnew = f_pair2 clsdeflst1 clsdeflst2 init cls_pairposlst_assoc in
          iter initnew tail

    | ExtensionPos(subtablelstsub) :: tail ->
        let initnew = iter init subtablelstsub in
          iter initnew tail

    | UnsupportedGPOSSubtable :: tail ->
        iter init tail


let gpos feature f_pair1 f_pair2 init =
  gxxx_subtable_list lookup_gpos feature >>= fun subtablelst ->
  return (fold_subtables_gpos f_pair1 f_pair2 init subtablelst)


(* -- MATH table -- *)

type math_value_record = int * device_table option

type math_constants =
  {
    script_percent_scale_down                     : int;
    script_script_percent_scale_down              : int;
    delimited_sub_formula_min_height              : int;
    display_operator_min_height                   : int;
    math_leading                                  : math_value_record;
    axis_height                                   : math_value_record;
    accent_base_height                            : math_value_record;
    flattened_accent_base_height                  : math_value_record;
    subscript_shift_down                          : math_value_record;
    subscript_top_max                             : math_value_record;
    subscript_baseline_drop_min                   : math_value_record;
    superscript_shift_up                          : math_value_record;
    superscript_shift_up_cramped                  : math_value_record;
    superscript_bottom_min                        : math_value_record;
    superscript_baseline_drop_max                 : math_value_record;
    sub_superscript_gap_min                       : math_value_record;
    superscript_bottom_max_with_subscript         : math_value_record;
    space_after_script                            : math_value_record;
    upper_limit_gap_min                           : math_value_record;
    upper_limit_baseline_rise_min                 : math_value_record;
    lower_limit_gap_min                           : math_value_record;
    lower_limit_baseline_drop_min                 : math_value_record;
    stack_top_shift_up                            : math_value_record;
    stack_top_display_style_shift_up              : math_value_record;
    stack_bottom_shift_down                       : math_value_record;
    stack_bottom_display_style_shift_down         : math_value_record;
    stack_gap_min                                 : math_value_record;
    stack_display_style_gap_min                   : math_value_record;
    stretch_stack_top_shift_up                    : math_value_record;
    stretch_stack_bottom_shift_down               : math_value_record;
    stretch_stack_gap_above_min                   : math_value_record;
    stretch_stack_gap_below_min                   : math_value_record;
    fraction_numerator_shift_up                   : math_value_record;
    fraction_numerator_display_style_shift_up     : math_value_record;
    fraction_denominator_shift_down               : math_value_record;
    fraction_denominator_display_style_shift_down : math_value_record;
    fraction_numerator_gap_min                    : math_value_record;
    fraction_num_display_style_gap_min            : math_value_record;
    fraction_rule_thickness                       : math_value_record;
    fraction_denominator_gap_min                  : math_value_record;
    fraction_denom_display_style_gap_min          : math_value_record;
    skewed_fraction_horizontal_gap                : math_value_record;
    skewed_fraction_vertical_gap                  : math_value_record;
    overbar_vertical_gap                          : math_value_record;
    overbar_rule_thickness                        : math_value_record;
    overbar_extra_ascender                        : math_value_record;
    underbar_vertical_gap                         : math_value_record;
    underbar_rule_thickness                       : math_value_record;
    underbar_extra_descender                      : math_value_record;
    radical_vertical_gap                          : math_value_record;
    radical_display_style_vertical_gap            : math_value_record;
    radical_rule_thickness                        : math_value_record;
    radical_extra_ascender                        : math_value_record;
    radical_kern_before_degree                    : math_value_record;
    radical_kern_after_degree                     : math_value_record;
    radical_degree_bottom_raise_percent           : int;
  }

type math_kern = math_value_record list * math_value_record list

type math_kern_info_record =
  {
    top_right_math_kern    : math_kern option;
    top_left_math_kern     : math_kern option;
    bottom_right_math_kern : math_kern option;
    bottom_left_math_kern  : math_kern option;
  }

type math_glyph_info =
  {
    math_italics_correction    : (glyph_id * math_value_record) list;
    math_top_accent_attachment : (glyph_id * math_value_record) list;
    math_kern_info             : (glyph_id * math_kern_info_record) list;
  }

type glyph_part_record =
  {
    glyph_id_for_part      : glyph_id;
    start_connector_length : int;
    end_connector_length   : int;
    full_advance           : int;
    part_flags             : int;
  }

type math_glyph_construction =
  {
    glyph_assembly                 : (math_value_record * glyph_part_record list) option;
    math_glyph_variant_record_list : (glyph_id * int) list;
  }

type math_variants =
  {
    min_connector_overlap : int;
    vert_glyph_assoc      : (glyph_id * math_glyph_construction) list;
    horiz_glyph_assoc     : (glyph_id * math_glyph_construction) list;
  }

type math =
  {
    math_constants  : math_constants;
    math_glyph_info : math_glyph_info;
    math_variants   : math_variants;
  }


let d_math_value_record offset_origin d : math_value_record ok =
  d_int16 d >>= fun value ->
  d_fetch_opt offset_origin d_device_table d >>= fun device_table_opt ->
  return (value, device_table_opt)


let d_math_constants d : math_constants ok =
  let offset_origin = cur_pos d in
  let dm = d_math_value_record offset_origin in
  d_int16  d >>= fun script_percent_scale_down ->
  d_int16  d >>= fun script_script_percent_scale_down ->
  d_uint16 d >>= fun delimited_sub_formula_min_height ->
  d_uint16 d >>= fun display_operator_min_height ->
  dm       d >>= fun math_leading ->
  dm       d >>= fun axis_height ->
  dm       d >>= fun accent_base_height ->
  dm       d >>= fun flattened_accent_base_height ->
  dm       d >>= fun subscript_shift_down ->
  dm       d >>= fun subscript_top_max ->
  dm       d >>= fun subscript_baseline_drop_min ->
  dm       d >>= fun superscript_shift_up ->
  dm       d >>= fun superscript_shift_up_cramped ->
  dm       d >>= fun superscript_bottom_min ->
  dm       d >>= fun superscript_baseline_drop_max ->
  dm       d >>= fun sub_superscript_gap_min ->
  dm       d >>= fun superscript_bottom_max_with_subscript ->
  dm       d >>= fun space_after_script ->
  dm       d >>= fun upper_limit_gap_min ->
  dm       d >>= fun upper_limit_baseline_rise_min ->
  dm       d >>= fun lower_limit_gap_min ->
  dm       d >>= fun lower_limit_baseline_drop_min ->
  dm       d >>= fun stack_top_shift_up ->
  dm       d >>= fun stack_top_display_style_shift_up ->
  dm       d >>= fun stack_bottom_shift_down ->
  dm       d >>= fun stack_bottom_display_style_shift_down ->
  dm       d >>= fun stack_gap_min ->
  dm       d >>= fun stack_display_style_gap_min ->
  dm       d >>= fun stretch_stack_top_shift_up ->
  dm       d >>= fun stretch_stack_bottom_shift_down ->
  dm       d >>= fun stretch_stack_gap_above_min ->
  dm       d >>= fun stretch_stack_gap_below_min ->
  dm       d >>= fun fraction_numerator_shift_up ->
  dm       d >>= fun fraction_numerator_display_style_shift_up ->
  dm       d >>= fun fraction_denominator_shift_down ->
  dm       d >>= fun fraction_denominator_display_style_shift_down ->
  dm       d >>= fun fraction_numerator_gap_min ->
  dm       d >>= fun fraction_num_display_style_gap_min ->
  dm       d >>= fun fraction_rule_thickness ->
  dm       d >>= fun fraction_denominator_gap_min ->
  dm       d >>= fun fraction_denom_display_style_gap_min ->
  dm       d >>= fun skewed_fraction_horizontal_gap ->
  dm       d >>= fun skewed_fraction_vertical_gap ->
  dm       d >>= fun overbar_vertical_gap ->
  dm       d >>= fun overbar_rule_thickness ->
  dm       d >>= fun overbar_extra_ascender ->
  dm       d >>= fun underbar_vertical_gap ->
  dm       d >>= fun underbar_rule_thickness ->
  dm       d >>= fun underbar_extra_descender ->
  dm       d >>= fun radical_vertical_gap ->
  dm       d >>= fun radical_display_style_vertical_gap ->
  dm       d >>= fun radical_rule_thickness ->
  dm       d >>= fun radical_extra_ascender ->
  dm       d >>= fun radical_kern_before_degree ->
  dm       d >>= fun radical_kern_after_degree ->
  d_int16  d >>= fun radical_degree_bottom_raise_percent ->
  return {
    script_percent_scale_down                     ;
    script_script_percent_scale_down              ;
    delimited_sub_formula_min_height              ;
    display_operator_min_height                   ;
    math_leading                                  ;
    axis_height                                   ;
    accent_base_height                            ;
    flattened_accent_base_height                  ;
    subscript_shift_down                          ;
    subscript_top_max                             ;
    subscript_baseline_drop_min                   ;
    superscript_shift_up                          ;
    superscript_shift_up_cramped                  ;
    superscript_bottom_min                        ;
    superscript_baseline_drop_max                 ;
    sub_superscript_gap_min                       ;
    superscript_bottom_max_with_subscript         ;
    space_after_script                            ;
    upper_limit_gap_min                           ;
    upper_limit_baseline_rise_min                 ;
    lower_limit_gap_min                           ;
    lower_limit_baseline_drop_min                 ;
    stack_top_shift_up                            ;
    stack_top_display_style_shift_up              ;
    stack_bottom_shift_down                       ;
    stack_bottom_display_style_shift_down         ;
    stack_gap_min                                 ;
    stack_display_style_gap_min                   ;
    stretch_stack_top_shift_up                    ;
    stretch_stack_bottom_shift_down               ;
    stretch_stack_gap_above_min                   ;
    stretch_stack_gap_below_min                   ;
    fraction_numerator_shift_up                   ;
    fraction_numerator_display_style_shift_up     ;
    fraction_denominator_shift_down               ;
    fraction_denominator_display_style_shift_down ;
    fraction_numerator_gap_min                    ;
    fraction_num_display_style_gap_min            ;
    fraction_rule_thickness                       ;
    fraction_denominator_gap_min                  ;
    fraction_denom_display_style_gap_min          ;
    skewed_fraction_horizontal_gap                ;
    skewed_fraction_vertical_gap                  ;
    overbar_vertical_gap                          ;
    overbar_rule_thickness                        ;
    overbar_extra_ascender                        ;
    underbar_vertical_gap                         ;
    underbar_rule_thickness                       ;
    underbar_extra_descender                      ;
    radical_vertical_gap                          ;
    radical_display_style_vertical_gap            ;
    radical_rule_thickness                        ;
    radical_extra_ascender                        ;
    radical_kern_before_degree                    ;
    radical_kern_after_degree                     ;
    radical_degree_bottom_raise_percent           ;
  }


let d_math_italics_correction_info d : ((glyph_id * math_value_record) list) ok =
  let offset_MathItalicCollectionInfo_table = cur_pos d in
  d_fetch offset_MathItalicCollectionInfo_table d_coverage d >>= fun coverage ->
  d_list (d_math_value_record offset_MathItalicCollectionInfo_table) d >>= fun mvrlst ->
  combine_coverage d coverage mvrlst


let d_math_top_accent_attachment d : ((glyph_id * math_value_record) list) ok =
  let offset_MathTopAccentAttachment_table = cur_pos d in
  d_fetch offset_MathTopAccentAttachment_table d_coverage d >>= fun coverage ->
  d_list (d_math_value_record offset_MathTopAccentAttachment_table) d >>= fun mvrlst ->
  combine_coverage d coverage mvrlst


let d_math_kern d : math_kern ok =
  let offset_MathKern_table = cur_pos d in
  d_uint16 d >>= fun heightCount ->
  d_repeat heightCount (d_math_value_record offset_MathKern_table) d >>= fun correctionHeight_lst ->
  d_repeat (heightCount + 1) (d_math_value_record offset_MathKern_table) d >>= fun kernValue_lst ->
  return (correctionHeight_lst, kernValue_lst)


let d_math_kern_info_record offset_MathKernInfo_table d : math_kern_info_record ok =
  Format.fprintf fmtMATH "### MathKernInfoRecord[1] = %d\n" (cur_pos d);
  d_fetch_opt offset_MathKernInfo_table d_math_kern d >>= fun topRightMathKern_opt ->
  d_fetch_opt offset_MathKernInfo_table d_math_kern d >>= fun topLeftMathKern_opt ->
  d_fetch_opt offset_MathKernInfo_table d_math_kern d >>= fun bottomRightMathKern_opt ->
  d_fetch_opt offset_MathKernInfo_table d_math_kern d >>= fun bottomLeftMathKern_opt ->
  return {
    top_right_math_kern    = topRightMathKern_opt;
    top_left_math_kern     = topLeftMathKern_opt;
    bottom_right_math_kern = bottomRightMathKern_opt;
    bottom_left_math_kern  = bottomLeftMathKern_opt;
  }


let d_math_kern_info d : ((glyph_id * math_kern_info_record) list) ok =
  let offset_MathKernInfo_table = cur_pos d in
  Format.fprintf fmtMATH "## MathKernInfo[1] = %d\n" (cur_pos d);
  d_fetch offset_MathKernInfo_table d_coverage d >>= fun coverage ->
  Format.fprintf fmtMATH "## MathKernInfo[2] = %d\n" (cur_pos d);
  d_list (d_math_kern_info_record offset_MathKernInfo_table) d >>= fun mvrlst ->
  Format.fprintf fmtMATH "## MathKernInfo[3] = %d\n" (cur_pos d);
  combine_coverage d coverage mvrlst


let d_math_glyph_info d : math_glyph_info ok =
  let offset_MathGlyphInfo_table = cur_pos d in
  Format.fprintf fmtMATH "# jump to MathItalicsCorrection = %d\n" (cur_pos d);
  d_fetch offset_MathGlyphInfo_table d_math_italics_correction_info d >>= fun mathItalicsCorrection ->
  Format.fprintf fmtMATH "# jump to MathTopAccentAttachment = %d\n" (cur_pos d);
  d_fetch offset_MathGlyphInfo_table d_math_top_accent_attachment d >>= fun mathTopAccentAttachment ->
  d_fetch_opt offset_MathGlyphInfo_table d_coverage d >>= fun _ ->
  Format.fprintf fmtMATH "# jump to MathKernInfo = %d\n" (cur_pos d);
  d_fetch_list offset_MathGlyphInfo_table d_math_kern_info d >>= fun mathKernInfo ->
  Format.fprintf fmtMATH "# END MathGlyphInfo\n";
  return {
    math_italics_correction    = mathItalicsCorrection;
    math_top_accent_attachment = mathTopAccentAttachment;
    math_kern_info             = mathKernInfo;
  }


let d_math_glyph_variant_record d : (glyph_id * int) ok =
  d_uint16 d >>= fun variantGlyph ->
  d_uint16 d >>= fun advanceMeasurement ->
  return (variantGlyph, advanceMeasurement)


let d_glyph_part_record d : glyph_part_record ok =
  d_uint16 d >>= fun glyph ->
  d_uint16 d >>= fun startConnectorLength ->
  d_uint16 d >>= fun endConnectorLength ->
  d_uint16 d >>= fun fullAdvance ->
  d_uint16 d >>= fun partFlags ->
  return {
    glyph_id_for_part      = glyph;
    start_connector_length = startConnectorLength;
    end_connector_length   = endConnectorLength;
    full_advance           = fullAdvance;
    part_flags             = partFlags;
  }


let d_glyph_assembly d : (math_value_record * glyph_part_record list) ok =
  let offset_GlyphAssembly_table = cur_pos d in
  d_math_value_record offset_GlyphAssembly_table d >>= fun italicsCorrection ->
  d_list d_glyph_part_record d >>= fun partRecords_lst ->
  return (italicsCorrection, partRecords_lst)


let d_math_glyph_construction d : math_glyph_construction ok =
  let offset_MathGlyphConstruction_table = cur_pos d in
  Format.fprintf fmtMATH "| | {GlyphAssembly\n";
  d_fetch_opt offset_MathGlyphConstruction_table d_glyph_assembly d >>= fun glyphAssembly ->
  Format.fprintf fmtMATH "| | MathGlyphVariantRecord\n";
  d_list d_math_glyph_variant_record d >>= fun mathGlyphVariantRecord_lst ->
  Format.fprintf fmtMATH "| | END MathGlyphConstruction}\n";
  return {
    glyph_assembly                 = glyphAssembly;
    math_glyph_variant_record_list = mathGlyphVariantRecord_lst;
  }


let d_math_variants d : math_variants ok =
  let offset_MathVariants_table = cur_pos d in
  d_uint16 d >>= fun minConnectorOverlap ->
  Format.fprintf fmtMATH "| VertGlyphCoverage\n";
  d_fetch offset_MathVariants_table d_coverage d >>= fun vertGlyphCoverage ->
  Format.fprintf fmtMATH "| HorizGlyphCoverage\n";
  d_fetch offset_MathVariants_table d_coverage d >>= fun horizGlyphCoverage ->
  d_uint16 d >>= fun vertGlyphCount ->
  d_uint16 d >>= fun horizGlyphCount ->
  let df = d_fetch offset_MathVariants_table d_math_glyph_construction in
  Format.fprintf fmtMATH "| VertGlyphConstruction\n";
  d_repeat vertGlyphCount df d >>= fun vertGlyphConstruction_lst ->
  Format.fprintf fmtMATH "| HorizGlyphConstruction\n";
  d_repeat horizGlyphCount df d >>= fun horizGlyphConstruction_lst ->
  combine_coverage d vertGlyphCoverage vertGlyphConstruction_lst >>= fun vertcomb ->
  combine_coverage d horizGlyphCoverage horizGlyphConstruction_lst >>= fun horizcomb ->
  return {
    min_connector_overlap = minConnectorOverlap;
    vert_glyph_assoc      = vertcomb;
    horiz_glyph_assoc     = horizcomb;
  }


let math d : math ok =
  init_decoder d >>=
  seek_table Tag.math d >>= function
    | None    -> err (`Missing_required_table(Tag.math))
    | Some(_) ->
        let offset_MATH = cur_pos d in
        Format.fprintf fmtMATH "begin MATH = %d\n" offset_MATH;
        d_uint32 d >>= fun version ->
        confirm (version = !%% 0x00010000L) (e_version d version) >>= fun () ->
        Format.fprintf fmtMATH "jump to MathConstants = %d\n" (cur_pos d);
        d_fetch offset_MATH d_math_constants d >>= fun mathConstants ->
        Format.fprintf fmtMATH "jump to MathGlyphInfo = %d\n" (cur_pos d);
        d_fetch offset_MATH d_math_glyph_info d >>= fun mathGlyphInfo ->
        Format.fprintf fmtMATH "jump to MathVariants = %d\n" (cur_pos d);
        d_fetch offset_MATH d_math_variants d >>= fun mathVariants ->
        Format.fprintf fmtMATH "end MATH\n";
        return {
          math_constants  = mathConstants;
          math_glyph_info = mathGlyphInfo;
          math_variants   = mathVariants;
        }


(* -- BASE table -- *)

let base d =
  let offset_BASE = cur_pos d in
  d_uint32 d >>= fun version ->
  confirm (version = !%% 0x00010000L) (e_version d version) >>= fun () ->
  d_offset_opt offset_BASE d >>= fun offsetopt_HorizAxis ->
  d_offset_opt offset_BASE d >>= fun offsetopt_VertAxis ->
  return ()  (* temporary *)


(* -- CFF_ table -- *)

type offsize = OffSize1 | OffSize2 | OffSize3 | OffSize4

type cff_key =
  | ShortKey of int
  | LongKey  of int

type cff_value =
  | Integer of int
  | Real    of float

type dict_element =
  | Value of cff_value
  | Key   of cff_key


module DictMap = Map.Make
  (struct
    type t = cff_key
    let compare kt1 kt2 =
      match (kt1, kt2) with
      | (ShortKey(i1), ShortKey(i2)) -> Pervasives.compare i1 i2
      | (ShortKey(_), LongKey(_))    -> -1
      | (LongKey(_), ShortKey(_))    -> 1
      | (LongKey(i1), LongKey(i2))   -> Pervasives.compare i1 i2
  end)


type dict = (cff_value list) DictMap.t

type string_index = string array

type stem_argument = string  (* temporary *)

type charstring_element =
  | ArgumentInteger  of int
  | ArgumentReal     of float
  | Operator         of cff_key
  | HintMaskOperator of stem_argument
  | CntrMaskOperator of stem_argument

type charstring_data =
  | CharStringData of int * int

type subroutine_index = charstring_data array

type cff_first = string * dict * string_index * subroutine_index * int

type cff_cid_info =
  {
    registry          : string;
    ordering          : string;
    supplement        : int;
    cid_font_version  : float;
    cid_font_revision : int;
    cid_font_type     : int;
    cid_count         : int;
  }

type single_private = {
  default_width_x  : int;
  nominal_width_x  : int;
  local_subr_index : subroutine_index;
}

type fdarray = single_private array

type fdindex = int

type fdselect =
  | FDSelectFormat0 of fdindex array
  | FDSelectFormat3 of (glyph_id * fdindex) list * glyph_id

type private_info =
  | SinglePrivate of single_private
  | FontDicts     of fdarray * fdselect

type charstring_info = decoder * subroutine_index * private_info * int

type cff_info =
  {
    font_name           : string;
    is_fixed_pitch      : bool;
    italic_angle        : int;
    underline_position  : int;
    underline_thickness : int;
    paint_type          : int;
    (* font_matrix : float * float * float * float; *)
    font_bbox           : int * int * int * int;
    stroke_width        : int;
    cid_info            : cff_cid_info option;
    number_of_glyphs    : int;
    charstring_info     : charstring_info;
  }


let is_in_range a b x = (a <= x && x <= b)


let d_offsize d : offsize ok =
  d_uint8 d >>= fun i ->
    match i with
    | 1 -> return OffSize1
    | 2 -> return OffSize2
    | 3 -> return OffSize3
    | 4 -> return OffSize4
    | n -> err (`Invalid_cff_not_an_offsize(n))


let pp_offsize fmt = function
  | OffSize1 -> Format.fprintf fmt "OffSize1"
  | OffSize2 -> Format.fprintf fmt "OffSize2"
  | OffSize3 -> Format.fprintf fmt "OffSize3"
  | OffSize4 -> Format.fprintf fmt "OffSize4"


let d_cff_offset ofsz d : wint ok =
  match ofsz with
  | OffSize1 -> d_uint8  d >>= fun i -> return (!% i)
  | OffSize2 -> d_uint16 d >>= fun i -> return (!% i)
  | OffSize3 -> d_uint24 d >>= fun i -> return (!% i)
  | OffSize4 -> d_uint32 d


let d_cff_offset_singleton ofsz dl d =
  d_cff_offset ofsz d                                        >>= fun offset1 ->
  confirm (offset1 = !% 1) `Invalid_cff_invalid_first_offset >>= fun () ->
  d_cff_offset ofsz d                                        >>= fun offset2 ->
  dl (WideInt.to_int (offset2 -% offset1)) d


let d_index_singleton dl d =
  d_uint16 d                                       >>= fun count ->
  confirm (count = 1) `Invalid_cff_not_a_singleton >>= fun () ->
  d_offsize d                                      >>= fun offSize ->
  d_cff_offset_singleton offSize dl d              >>= fun v ->
  return v


let d_cff_length_list ofsz count d =
  let rec aux offsetprev acc i =
    if i >= count then
      return (Alist.to_list acc)
    else
      d_cff_offset ofsz d >>= fun offset ->
      let len = WideInt.to_int (offset -% offsetprev) in
      aux offset (Alist.extend acc len) (i + 1)
  in
  d_cff_offset ofsz d                                        >>= fun offset1 ->
  confirm (offset1 = !% 1) `Invalid_cff_invalid_first_offset >>= fun () ->
  aux (!% 1) Alist.empty 0


let d_index (type a) (dummy : a) (dl : int -> decoder -> a ok) (d : decoder) : (a array) ok =
  let rec loop_data arr i = function
    | []             -> return ()
    | len :: lentail ->
        dl len d >>= fun v ->
        arr.(i) <- v;
        loop_data arr (i + 1) lentail
  in
  d_uint16 d >>= fun count ->
(*
  Format.fprintf fmtCFF "INDEX count: %d\n" count;  (* for debug *)
*)
  if count = 0 then
    return [| |]
  else
    let arr = Array.make count dummy in
    d_offsize d                       >>= fun offSize ->
(*
    Format.fprintf fmtCFF "INDEX offSize: %a\n" pp_offsize offSize;  (* for debug *)
*)
    d_cff_length_list offSize count d >>= fun lenlst ->
    loop_data arr 0 lenlst >>= fun () ->
    return arr



let d_charstring_data (len : int) (d : decoder) : charstring_data ok =
  let offset = cur_pos d in
  seek_pos (offset + len) d >>= fun () ->
  return (CharStringData(offset, len))


let d_cff_real d =
(*
  Format.fprintf fmtCFF "d_cff_real\n";  (* for debug *)
*)
  let to_float lst =
    float_of_string (String.concat "" lst)
  in
  let nibble = function
    | d  when d |> is_in_range 0 9 -> return (string_of_int d)
    | 10                           -> return "."
    | 11                           -> return "e"
    | 12                           -> return "e-"
    | 13                           -> err `Invalid_cff_not_an_element
    | 14                           -> return "-"
    | 15                           -> return ""
    | _                            -> err `Invalid_cff_not_an_element
  in
  let rec aux step acc =
    d_uint8 d >>= fun raw ->
    let q1 = raw / 16 in
    let q2 = raw mod 16 in
    if q1 = 15 then
      if q2 = 15 then
        return (step + 1, to_float (Alist.to_list acc))
      else
        err `Invalid_cff_not_an_element
    else
      if q2 = 15 then
        nibble q1 >>= fun nb1 ->
        return (step + 1, to_float (Alist.to_list (Alist.extend acc nb1)))
      else
        nibble q2 >>= fun nb2 ->
        nibble q1 >>= fun nb1 ->
        aux (step + 1) (Alist.extend (Alist.extend acc nb1) nb2)
  in
  aux 0 Alist.empty


let d_index_access (type a) (dl : int -> decoder -> a ok) (iaccess : int) (d : decoder) : (a option) ok =
  d_uint16 d >>= fun count ->
(*
  Format.fprintf fmtCFF "count = %d\n" count;  (* for debug *)
*)
  if iaccess < 0 || count <= iaccess then
    return None
  else
    d_offsize d >>= fun offSize ->
    let ofszint =
      match offSize with
      | OffSize1 -> 1
      | OffSize2 -> 2
      | OffSize3 -> 3
      | OffSize4 -> 4
    in
(*
    Format.fprintf fmtCFF "OffSize = %a\n" pp_offsize offSize;  (* for debug *)
*)
    let offset_origin = (cur_pos d) + (count + 1) * ofszint - 1 in
    d_skip (iaccess * ofszint) d >>= fun () ->
    d_cff_offset offSize d >>= fun reloffset_access ->
    d_cff_offset offSize d >>= fun reloffset_next ->
    let offset_access = offset_origin + (WideInt.to_int reloffset_access) in
    let data_length = WideInt.to_int (reloffset_next -% reloffset_access) in
    seek_pos offset_access d >>= fun () ->
    dl data_length d >>= fun data ->
    return (Some(data))


let d_twoscompl2 d =
  d_uint8 d >>= fun b1 ->
  d_uint8 d >>= fun b2 ->
  let iraw = (b1 lsl 8) lor b2 in
  let ret =
    if iraw >= (1 lsl 15) then
      iraw - (1 lsl 16)
    else
      iraw
  in
  return ret


let d_twoscompl4 d =
  d_uint8 d >>= fun b1 ->
  d_uint8 d >>= fun b2 ->
  d_uint8 d >>= fun b3 ->
  d_uint8 d >>= fun b4 ->
  let iraw = (b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4 in
  let ret =
    if iraw >= (1 lsl 31) then
      iraw - (1 lsl 32)
    else
      iraw
  in
  return ret


let d_dict_element d : (int * dict_element) ok =
  d_uint8 d >>= function
    | k0  when k0 |> is_in_range 0 11 ->
        return (1, Key(ShortKey(k0)))

    | 12 ->
        d_uint8 d >>= fun k1 ->
        return (2, Key(LongKey(k1)))

    | k0  when k0 |> is_in_range 13 21 ->
        return (1, Key(ShortKey(k0)))

    | 28 ->
        d_twoscompl2 d >>= fun ret ->
        return (3, Value(Integer(ret)))

    | 29 ->
        d_twoscompl4 d >>= fun ret ->
        return (5, Value(Integer(ret)))

    | 30 ->
        d_cff_real d >>= fun (step, real) ->
        return (1 + step, Value(Real(real)))

    | b0  when b0 |> is_in_range 32 246 ->
        return (1, Value(Integer(b0 - 139)))

    | b0  when b0 |> is_in_range 247 250 ->
        d_uint8 d >>= fun b1 ->
        return (2, Value(Integer((b0 - 247) * 256 + b1 + 108)))

    | b0  when b0 |> is_in_range 251 254 ->
        d_uint8 d >>= fun b1 ->
        return (2, Value(Integer(-(b0 - 251) * 256 - b1 - 108)))

    | _ ->
        err `Invalid_cff_not_an_element


let pp_element ppf = function
  | Value(Integer(i)) -> Format.fprintf ppf "Integer(%d)" i
  | Value(Real(r))    -> Format.fprintf ppf "Real(%f)" r
  | Key(LongKey(x))   -> Format.fprintf ppf "LongKey(%d)" x
  | Key(ShortKey(x))  -> Format.fprintf ppf "ShortKey(%d)" x


let d_dict_keyval d : (int * cff_value list * cff_key) ok =
  let rec aux stepsum vacc =
    d_dict_element d >>= fun (step, elem) ->
(*
      Format.fprintf fmtCFF "dict element: %d, %a\n" step pp_element elem;  (* for debug *)
*)
      match elem with
      | Value(v) -> aux (stepsum + step) (Alist.extend vacc v)
      | Key(k)   -> return (stepsum + step, Alist.to_list vacc, k)
  in
    aux 0 Alist.empty


let d_dict len d : ((cff_value list) DictMap.t) ok =
  let rec loop_keyval mapacc len d =
    if len = 0 then return mapacc else
    if len < 0 then err `Invalid_cff_inconsistent_length else
      d_dict_keyval d >>= fun (step, vlst, k) ->
      loop_keyval (mapacc |> DictMap.add k vlst) (len - step) d
  in
(*
  Format.fprintf fmtCFF "length = %d\n" len;  (* for debug *)
*)
  loop_keyval DictMap.empty len d


let pp_short_key fmt key =
  let f = Format.fprintf fmt "%s" in
  match key with
  | 1  -> f "hstem"
  | 3  -> f "vstem"
  | 4  -> f "vmoveto"
  | 5  -> f "rlineto"
  | 6  -> f "hlineto"
  | 7  -> f "vlineto"
  | 8  -> f "rrcurveto"
  | 10 -> f "CALLSUBR"
  | 11 -> f "RETURN"
  | 14 -> f "endchar"
  | 18 -> f "hstemhm"
  | 19 -> f "HINTMASK"
  | 20 -> f "CNTRMASK"
  | 21 -> f "rmoveto"
  | 22 -> f "hmoveto"
  | 23 -> f "vstemhm"
  | 24 -> f "rcurveline"
  | 25 -> f "rlinecurve"
  | 26 -> f "vvcurveto"
  | 27 -> f "hhcurveto"
  | 29 -> f "CALLGSUBR"
  | 30 -> f "vhcurveto"
  | 31 -> f "hvcurveto"
  | i  -> Format.fprintf fmt "!OP(%d)" i


let pp_charstring_element fmt = function
  | ArgumentInteger(i)     -> Format.fprintf fmt "%d " i
  | ArgumentReal(r)        -> Format.fprintf fmt "%f " r
  | Operator(ShortKey(i))  -> Format.fprintf fmt "%a\n" pp_short_key i
  | Operator(LongKey(i))   -> Format.fprintf fmt "OP(12 %d)\n" i
  | HintMaskOperator(arg)  -> Format.fprintf fmt "HINTMASK(...)\n"
  | CntrMaskOperator(arg)  -> Format.fprintf fmt "CNTRMASK(...)\n"


let d_stem_argument (numstem : int) (d : decoder) : (int * stem_argument) ok =
  let arglen =
    if numstem mod 8 = 0 then
      numstem / 8
    else
      numstem / 8 + 1
  in
  d_bytes arglen d >>= fun arg ->
  return (arglen, arg)


type charstring_state = {
  numarg : int;
  numstem : int;
}


let d_charstring_element (cstate : charstring_state) (d : decoder) : (int * charstring_state * charstring_element) ok =
  let numarg = cstate.numarg in
  let numstem = cstate.numstem in
  let return_argument (step, cselem) =
(*
    Format.fprintf fmtCFF "%a" pp_charstring_element cselem;  (* for debug *)
    Format.fprintf fmtCFF "\n  # numarg = %d ---> %d\n" numarg (numarg + 1);  (* for debug *)
*)
    return (step, { numarg = numarg + 1; numstem = numstem }, cselem)
  in
  let return_flushing_operator (step, cselem) =
(*
    Format.fprintf fmtCFF "%a" pp_charstring_element cselem;  (* for debug *)
*)
    return (step, { numarg = 0; numstem = numstem }, cselem)
  in
  let return_subroutine_operator cselem =
(*
    Format.fprintf fmtCFF "%a" pp_charstring_element cselem;  (* for debug *)
    Format.fprintf fmtCFF "  # numarg = %d ---> %d\n" numarg (numarg - 1);  (* for debug *)
*)
    return (1, { numarg = numarg - 1; numstem = numstem }, cselem)

  in
  let return_stem (step, cselem) =
(*
    Format.fprintf fmtCFF "%a" pp_charstring_element cselem;  (* for debug *)
    Format.fprintf fmtCFF "  # step = %d, numarg = %d\n" step numarg;  (* for debug *)
    Format.fprintf fmtCFF "  # stem = %d ----> %d\n" numstem (numstem + numarg / 2);  (* for debug *)
*)
    return (step, { numarg = 0; numstem = numstem + numarg / 2 }, cselem)
  in
    (* -- 'numarg' may be an odd number, but it is due to the width value -- *)
  d_uint8 d >>= function
  | ( 1 | 3 | 18 | 23) as b0 ->
    (* -- stem operators -- *)
      return_stem (1, Operator(ShortKey(b0)))

  | b0  when b0 |> is_in_range 0 9 ->
      return_flushing_operator (1, Operator(ShortKey(b0)))

  | (10 | 29) as b0 ->
    (* -- callsubr / callgsubr operator -- *)
      return_subroutine_operator (Operator(ShortKey(b0)))

  | 11 ->
    (* -- return operator -- *)
      let cselem = Operator(ShortKey(11)) in
(*
      Format.fprintf fmtCFF "%a" pp_charstring_element cselem;
*)
      return (1, cstate, cselem)

  | 12 ->
      d_uint8 d >>= fun b1 ->
      return_flushing_operator (2, Operator(LongKey(b1)))

  | b0  when b0 |> is_in_range 13 18 ->
      return_flushing_operator (1, Operator(ShortKey(b0)))

  | 19 ->
    (* -- hintmask operator -- *)
(*
      Format.fprintf fmtCFF "hintmask (numstem = %d (numarg = %d) ---> %d)\n" numstem numarg (numstem + numarg / 2);  (*for debug *)
*)
        d_stem_argument (numstem + numarg / 2) d >>= fun (step, bits) ->
        return_stem (1 + step, HintMaskOperator(bits))

  | 20 ->
    (* -- cntrmask operator -- *)
(*
      Format.fprintf fmtCFF "  # cntrmask (numstem = %d (numarg = %d) ---> %d)\n" numstem numarg (numstem + numarg / 2);  (*for debug *)
*)
      d_stem_argument (numstem + numarg / 2) d >>= fun (step, bits) ->
      return_stem (1 + step, CntrMaskOperator(bits))

  | b0  when b0 |> is_in_range 21 27 ->
      return_flushing_operator (1, Operator(ShortKey(b0)))

  | 28 ->
      d_twoscompl2 d >>= fun ret ->
      return_argument (3, ArgumentInteger(ret))

  | b0  when b0 |> is_in_range 30 31 ->
      return_flushing_operator (1, Operator(ShortKey(b0)))

  | b0  when b0 |> is_in_range 32 246 ->
      return_argument (1, ArgumentInteger(b0 - 139))

  | b0  when b0 |> is_in_range 247 250 ->
      d_uint8 d >>= fun b1 ->
      return_argument (2, ArgumentInteger((b0 - 247) * 256 + b1 + 108))

  | b0  when b0 |> is_in_range 251 254 ->
      d_uint8 d >>= fun b1 ->
      return_argument (2, ArgumentInteger(- (b0 - 251) * 256 - b1 - 108))

  | 255 ->
      d_twoscompl2 d >>= fun ret1 ->
      d_twoscompl2 d >>= fun ret2 ->
      let ret = float_of_int ret1 +. (float_of_int ret2) /. (float_of_int (1 lsl 16)) in
      return_argument (5, ArgumentReal(ret))

  | _ ->
      assert false
        (* -- uint8 value must be in [0 .. 255] -- *)


let cff_first (d : decoder) : cff_first ok =
  init_decoder d >>=
  seek_required_table Tag.cff d >>= fun () ->
  let offset_CFF = cur_pos d in
  (* -- Header -- *)
(*
    Format.fprintf fmtCFF "* Header\n";  (* for debug *)
*)
    d_uint8 d              >>= fun major ->
    d_uint8 d              >>= fun minor ->
(*
    Format.fprintf fmtCFF "version = %d.%d\n" major minor;  (* for debug *)
*)
    d_uint8 d              >>= fun hdrSize ->
    d_offsize d            >>= fun offSizeGlobal ->
    d_skip (hdrSize - 4) d >>= fun () ->

  (* -- Name INDEX (which should contain only one element) -- *)
(*
    Format.fprintf fmtCFF "* Name INDEX\n";  (* for debug *)
*)
    d_index_singleton d_bytes d >>= fun name ->

  (* -- Top DICT INDEX (which should contain only one DICT) -- *)
(*
    Format.fprintf fmtCFF "* Top DICT INDEX\n";  (* for debug *)
*)
    d_index_singleton d_dict d >>= fun dictmap ->

  (* -- String INDEX -- *)
(*
    Format.fprintf fmtCFF "* String INDEX\n";  (* for debug *)
*)
    d_index "(dummy)" d_bytes d >>= fun stridx ->

  (* -- Global Subr INDEX -- *)
(*
    Format.fprintf fmtCFF "* Global Subr INDEX\n";  (* for debug *)
*)
    d_index (CharStringData(0, 0)) d_charstring_data d >>= fun gsubridx ->
      (* temporary; should be decoded *)

    return (name, dictmap, stridx, gsubridx, offset_CFF)


let err_dict_key key =
  match key with
  | ShortKey(i) -> err (`Missing_required_dict_short_key(i))
  | LongKey(i)  -> err (`Missing_required_dict_long_key(i))


let get_string stridx sid =
  let nStdString = 391 in
  if sid < nStdString then
    failwith "a standard string; remains to be supported."
  else
    try return stridx.(sid - nStdString) with
    | Invalid_argument(_) -> err (`Invalid_sid(sid))


let get_integer_opt dictmap key =
    match DictMap.find_opt key dictmap with
    | Some(Integer(i) :: []) -> return (Some(i))
    | Some(Real(fl) :: [])   -> return (Some(int_of_float fl))  (* -- rounds a float value -- *)
    | Some(_)                -> err `Invalid_cff_not_an_integer
    | None                   -> return None


let get_integer_with_default dictmap key dflt =
  get_integer_opt dictmap key >>= function
  | Some(i) -> return i
  | None    -> return dflt


let get_integer dictmap key =
  get_integer_opt dictmap key >>= function
  | Some(i) -> return i
  | None    -> err_dict_key key


let get_integer_pair_opt dictmap key =
  match DictMap.find_opt key dictmap with
  | Some(Integer(i1) :: Integer(i2) :: []) -> return (Some(i1, i2))
  | Some(Integer(i1) :: Real(fl2) :: [])   -> return (Some(i1, int_of_float fl2))
  | Some(Real(fl1) :: Integer(i2) :: [])   -> return (Some(int_of_float fl1, i2))
  | Some(Real(fl1) :: Real(fl2) :: [])     -> return (Some(int_of_float fl1, int_of_float fl2))
  | Some(_)                                -> err `Invalid_cff_not_an_integer
  | None                                   -> return None


let get_sid = get_integer


let get_real_with_default dictmap key dflt =
    match DictMap.find_opt key dictmap with
    | Some(Real(r) :: []) -> return r
    | Some(_)             -> err `Invalid_cff_not_an_integer
    | None                -> return dflt


let get_boolean_with_defautlt dictmap key dflt =
  get_integer_with_default dictmap key (if dflt then 1 else 0) >>= fun i -> return (i <> 0)


let get_iquad_opt dictmap key dflt =
  match dictmap |> DictMap.find_opt key with
  | Some(Integer(i1) :: Integer(i2) :: Integer(i3) :: Integer(i4) :: []) -> return (i1, i2, i3, i4)
  | Some(_)                                                              -> err `Invalid_cff_not_a_quad
  | None                                                                 -> return dflt


let get_ros dictmap key =
  match dictmap |> DictMap.find_opt key with
  | Some(Integer(sid1) :: Integer(sid2) :: Integer(i) :: []) -> return (sid1, sid2, i)
  | Some(_)                                                  -> err `Invalid_ros
  | None                                                     -> err `Invalid_ros


let d_single_private offset_CFF dictmap d : single_private ok =
(* -- Private DICT -- *)
  get_integer_pair_opt dictmap (ShortKey(18)) >>= fun privateopt ->
    match privateopt with
    | None ->
(*
        Format.fprintf fmtCFF "No Private DICT\n";  (* for debug *)
*)
        err `Invalid_cff_no_private_dict

    | Some(size_private, reloffset_private) ->
        let offset_private = offset_CFF + reloffset_private in
        seek_pos offset_private d >>= fun () ->
        d_dict size_private d >>= fun dictmap_private ->
        get_integer_opt          dictmap_private (ShortKey(19))   >>= fun selfoffset_lsubrs_opt ->
        get_integer_with_default dictmap_private (ShortKey(20)) 0 >>= fun default_width_x ->
        get_integer_with_default dictmap_private (ShortKey(21)) 0 >>= fun nominal_width_x ->

      (* -- Local Subr INDEX -- *)
        let lsubrsidx_res =
          match selfoffset_lsubrs_opt with
          | None ->
              return [||]

          | Some(selfoffset_lsubrs) ->
              let offset_lsubrs = offset_private + selfoffset_lsubrs in
              seek_pos offset_lsubrs d >>= fun () ->
      (*
              Format.fprintf fmtCFF "* Local Subr INDEX\n";  (* for debug *)
      *)
              d_index (CharStringData(0, 0)) d_charstring_data d
        in
        lsubrsidx_res >>= fun lsubridx ->
(*
        Format.fprintf fmtCFF "length = %d\n" (Array.length lsubridx);  (* for debug *)
*)
        return { default_width_x; nominal_width_x; local_subr_index = lsubridx }


let seek_number_of_glyphs offset_CharString_INDEX d =
  seek_pos offset_CharString_INDEX d >>= fun () ->
  d_uint16 d


exception Internal of error


let seek_fdarray offset_CFF offset_FDArray d : fdarray ok =
  seek_pos offset_FDArray d >>= fun () ->
  d_index DictMap.empty d_dict d >>= fun arrraw ->
  try
    let arr =
      arrraw |> Array.map (fun dictmap ->
        let res =
          d_single_private offset_CFF dictmap d
        in
        match res with
        | Error(e)       -> raise (Internal(e))
        | Ok(singlepriv) -> singlepriv
      )
    in
    return arr
  with
  | Internal(e) -> err e


let d_fdselect_format_0 nGlyphs d : fdselect ok =
  let idx = Array.make nGlyphs 0 in
  let rec aux i =
    if i >= nGlyphs then
      return (FDSelectFormat0(idx))
    else
      begin
        d_uint8 d >>= fun v ->
        idx.(i) <- v;
        aux (i + 1)
      end

  in
  aux 0


let d_fdselect_format_3 d : fdselect ok =
  let rec aux num i acc =
    if i >= num then
      d_uint16 d >>= fun gid_sentinel ->
      return (FDSelectFormat3(Alist.to_list acc, gid_sentinel))
    else
      d_uint16 d >>= fun gid ->
      d_uint8 d >>= fun v ->
      aux num (i + 1) (Alist.extend acc (gid, v))
  in
  d_uint16 d >>= fun nRanges ->
  aux nRanges 0 Alist.empty


let seek_fdselect nGlyphs offset_FDSelect d : fdselect ok =
  seek_pos offset_FDSelect d >>= fun () ->
  d_uint8 d >>= function
  | 0 -> d_fdselect_format_0 nGlyphs d
  | 3 -> d_fdselect_format_3 d
  | n -> err (`Unknown_fdselect_format(n))


let cff d =
  cff_first d >>= fun (font_name, dictmap, stridx, gsubridx, offset_CFF) ->
(*
  get_sid         dictmap (ShortKey(0))              >>= fun sid_version ->
  get_sid         dictmap (ShortKey(1))              >>= fun sid_notice ->
  get_sid         dictmap (LongKey(0) )              >>= fun sid_copyright ->
  get_sid         dictmap (ShortKey(2))              >>= fun sid_full_name ->
  get_sid         dictmap (ShortKey(3))              >>= fun sid_family_name ->
  get_sid         dictmap (ShortKey(4))              >>= fun sid_weight ->
*)
  get_boolean_with_defautlt dictmap (LongKey(1) ) false        >>= fun is_fixed_pitch ->
  get_integer_with_default  dictmap (LongKey(2) ) 0            >>= fun italic_angle ->
  get_integer_with_default  dictmap (LongKey(3) ) (-100)       >>= fun underline_position ->
  get_integer_with_default  dictmap (LongKey(4) ) 50           >>= fun underline_thickness ->
  get_integer_with_default  dictmap (LongKey(5) ) 0            >>= fun paint_type ->
  get_integer_with_default  dictmap (LongKey(6) ) 2            >>= fun charstring_type ->
  confirm (charstring_type = 2)
    (`Invalid_charstring_type(charstring_type))      >>= fun () ->

  (* -- have not implemented 'LongKey(7) --> font_matrix' yet; maybe it is not necessary -- *)

  get_iquad_opt        dictmap (ShortKey(5)) (0, 0, 0, 0) >>= fun font_bbox ->
  get_integer_with_default      dictmap (LongKey(8) ) 0            >>= fun stroke_width ->
  get_integer          dictmap (ShortKey(17))             >>= fun reloffset_CharString_INDEX ->
  let offset_CharString_INDEX = offset_CFF + reloffset_CharString_INDEX in
  seek_number_of_glyphs offset_CharString_INDEX d >>= fun number_of_glyphs ->
  let pairres =
    if DictMap.mem (LongKey(30)) dictmap then
    (* -- when the font is a CIDFont -- *)
      get_ros                  dictmap (LongKey(30))      >>= fun (sid_registry, sid_ordering, supplement) ->
      get_real_with_default    dictmap (LongKey(31)) 0.   >>= fun cid_font_version ->
      get_integer_with_default dictmap (LongKey(32)) 0    >>= fun cid_font_revision ->
      get_integer_with_default dictmap (LongKey(33)) 0    >>= fun cid_font_type ->
      get_integer_with_default dictmap (LongKey(34)) 8720 >>= fun cid_count ->
      get_integer     dictmap (LongKey(36))      >>= fun reloffset_FDArray ->
      get_integer     dictmap (LongKey(37))      >>= fun reloffset_FDSelect ->
      get_string stridx sid_registry >>= fun registry ->
      get_string stridx sid_ordering >>= fun ordering ->
      let offset_FDArray = offset_CFF + reloffset_FDArray in
      let offset_FDSelect = offset_CFF + reloffset_FDSelect in
      seek_fdarray offset_CFF offset_FDArray d >>= fun fdarray ->
      seek_fdselect number_of_glyphs offset_FDSelect d >>= fun fdselect ->
      return (Some{
        registry; ordering; supplement;
        cid_font_version;
        cid_font_revision;
        cid_font_type;
        cid_count;
      }, FontDicts(fdarray, fdselect))
    else
    (* -- when the font is not a CIDFont -- *)
      d_single_private offset_CFF dictmap d >>= fun singlepriv ->
      return (None, SinglePrivate(singlepriv))
  in
  pairres >>= fun (cid_info, private_info) ->
  return {
    font_name;
    is_fixed_pitch;
    italic_angle;
    underline_position;
    underline_thickness;
    paint_type;
    font_bbox;
    stroke_width;
    cid_info;
    number_of_glyphs;
    charstring_info = (d, gsubridx, private_info, offset_CharString_INDEX);
  }


let pop (stk : int Stack.t) : int ok =
  try return (Stack.pop stk) with
  | Stack.Empty -> err `Invalid_charstring


let pop_opt (stk : int Stack.t) : int option =
  try Some(Stack.pop stk) with
  | Stack.Empty -> None


let pop_all (stk : int Stack.t) : int list =
  let rec aux acc =
    if Stack.is_empty stk then acc else
      let i = Stack.pop stk in
        aux (i :: acc)
  in
    aux []


let pop_pair_opt (stk : int Stack.t) : (int * int) option =
  if Stack.length stk < 2 then None else
    let y = Stack.pop stk in
    let x = Stack.pop stk in
      Some((x, y))


let pop_4_opt (stk : int Stack.t) : (int * int * int * int) option =
  if Stack.length stk < 4 then None else
    let d4 = Stack.pop stk in
    let d3 = Stack.pop stk in
    let d2 = Stack.pop stk in
    let d1 = Stack.pop stk in
      Some((d1, d2, d3, d4))


let pop_6_opt (stk : int Stack.t) : (int * int * int * int * int * int) option =
  if Stack.length stk < 6 then None else
    let d6 = Stack.pop stk in
    let d5 = Stack.pop stk in
    let d4 = Stack.pop stk in
    let d3 = Stack.pop stk in
    let d2 = Stack.pop stk in
    let d1 = Stack.pop stk in
      Some((d1, d2, d3, d4, d5, d6))


let pop_8_opt (stk : int Stack.t) : (int * int * int * int * int * int * int * int) option =
  if Stack.length stk < 8 then None else
    let d8 = Stack.pop stk in
    let d7 = Stack.pop stk in
    let d6 = Stack.pop stk in
    let d5 = Stack.pop stk in
    let d4 = Stack.pop stk in
    let d3 = Stack.pop stk in
    let d2 = Stack.pop stk in
    let d1 = Stack.pop stk in
      Some((d1, d2, d3, d4, d5, d6, d7, d8))


let pop_iter (type a) (popf : int Stack.t -> a option) (stk : int Stack.t) : (a list) =
  let rec aux acc =
    let retopt = popf stk in
    match retopt with
    | None      -> acc  (* -- returns in the forward direction -- *)
    | Some(ret) -> aux (ret :: acc)
  in
  aux []


let is_odd_length stk =
  let len = Stack.length stk in
    len mod 2 = 1


let make_bezier (dxa, dya, dxb, dyb, dxc, dyc) = ((dxa, dya), (dxb, dyb), (dxc, dyc))


type csx = int

type csy = int

type cspoint = csx * csy

type parsed_charstring =
  | HStem of int * int * cspoint list
      (* -- hstem (1) -- *)
  | VStem of int * int * cspoint list
      (* -- vstem (3) -- *)
  | VMoveTo of int
      (* -- vmoveto (4) -- *)
  | RLineTo of cspoint list
      (* -- rlineto (5) -- *)
  | HLineTo of int list
      (* -- hlineto (6) -- *)
  | VLineTo of int list
      (* -- vlineto (7) -- *)
  | RRCurveTo of (cspoint * cspoint * cspoint) list
      (* -- rrcurveto (8) *)
  | HStemHM of int * int * cspoint list
      (* -- hstemhm (18) -- *)
  | HintMask of stem_argument
      (* -- hintmask (19) -- *)
  | CntrMask of stem_argument
      (* -- cntrmask (20) -- *)
  | RMoveTo of cspoint
      (* -- rmoveto (21) -- *)
  | HMoveTo of int
      (* -- hmoveto (22) -- )*)
  | VStemHM of int * int * cspoint list
      (* -- vstemhm (23) -- *)
  | VVCurveTo of csx option * (csy * cspoint * csy) list
      (* -- vvcurveto (26) -- *)
  | HHCurveTo of csy option * (csx * cspoint * csx) list
      (* -- hhcurveto (27) -- *)
  | VHCurveTo of (int * cspoint * int) list * int option
      (* -- vhcurveto (30) -- *)
  | HVCurveTo of (int * cspoint * int) list * int option
      (* -- hvcurveto (31) -- *)
  | Flex of cspoint * cspoint * cspoint * cspoint * cspoint * cspoint * int
      (* -- flex (12 35) -- *)
  | HFlex of int * cspoint * int * int * int * int
      (* -- hflex (12 34) -- *)
  | HFlex1 of cspoint * cspoint * int * int * cspoint * int
      (* -- hflex1 (12 36) -- *)
  | Flex1 of cspoint * cspoint * cspoint * cspoint * cspoint * int
      (* -- flex1 (12 37) -- *)


let pp_cspoint fmt (x, y) = pp fmt "(%d %d)" x y

let pp_bezier fmt (dvA, dvB, dvC) = pp fmt "%a..%a..%a" pp_cspoint dvA pp_cspoint dvB pp_cspoint dvC

let pp_int fmt x = pp fmt "%d" x

let pp_int_option fmt = function
  | None    -> pp fmt "NONE"
  | Some(i) -> pp fmt "%d" i

let pp_partial fmt (dtD, (dxE, dyE), dsF) = pp fmt "%d_(%d %d)_%d" dtD dxE dyE dsF

let pp_sep_space fmt () = pp fmt " "

let pp_cspoint_list = pp_list ~pp_sep:pp_sep_space pp_cspoint

let pp_partial_list = pp_list ~pp_sep:pp_sep_space pp_partial

let pp_parsed_charstring fmt = function
  | HStem(y, dy, csptlst)   -> pp fmt "HStem(%d %d %a)" y dy pp_cspoint_list csptlst
  | VStem(x, dx, csptlst)   -> pp fmt "VStem(%d %d %a)" x dx pp_cspoint_list csptlst
  | HStemHM(y, dy, csptlst) -> pp fmt "HStemHM(%d %d %a)" y dy pp_cspoint_list csptlst
  | VStemHM(x, dx, csptlst) -> pp fmt "VStemHM(%d %d %a)" x dx pp_cspoint_list csptlst
  | HintMask(_)             -> pp fmt "HintMask(_)"
  | CntrMask(_)             -> pp fmt "CntrMask(_)"
  | VMoveTo(dy1)            -> pp fmt "VMoveTo(%d)" dy1
  | RLineTo(csptlst)        -> pp fmt "RLineTo(%a)" pp_cspoint_list csptlst
  | HLineTo(lst)            -> pp fmt "HLineTo(%a)" (pp_list ~pp_sep:pp_sep_space pp_int) lst
  | VLineTo(lst)            -> pp fmt "VLineTo(%a)" (pp_list ~pp_sep:pp_sep_space pp_int) lst
  | RRCurveTo(bezierlst)    -> pp fmt "RRCurveTo(%a)" (pp_list ~pp_sep:pp_sep_space pp_bezier) bezierlst
  | RMoveTo((dx1, dy1))     -> pp fmt "RMoveTo(%d, %d)" dx1 dy1
  | HMoveTo(dx1)            -> pp fmt "HMoveTo(%d)" dx1
  | VVCurveTo(dtopt, lst)   -> pp fmt "VVCurveTo(%a %a)" pp_int_option dtopt pp_partial_list lst
  | HHCurveTo(dtopt, lst)   -> pp fmt "HHCurveTo(%a %a)" pp_int_option dtopt pp_partial_list lst
  | VHCurveTo(lst, dtopt)   -> pp fmt "VHCurveTo(%a %a)" pp_partial_list lst pp_int_option dtopt
  | HVCurveTo(lst, dtopt)   -> pp fmt "HVCurveTo(%a %a)" pp_partial_list lst pp_int_option dtopt
  | Flex(p1, p2, p3, p4, p5, p6, fd)   -> pp fmt "Flex(%a, %a, %a, %a, %a, %a, %d)" pp_cspoint p1 pp_cspoint p2 pp_cspoint p3 pp_cspoint p4 pp_cspoint p5 pp_cspoint p6 fd
  | HFlex(dx1, p2, dx3, dx4, dx5, dx6) -> pp fmt "HFlex(%d, %a, %d, %d, %d, %d)" dx1 pp_cspoint p2 dx3 dx4 dx5 dx6
  | HFlex1(p1, p2, dx3, dx4, p5, dx6)  -> pp fmt "HFlex1(%a, %a, %d, %d, %a, %d)" pp_cspoint p1 pp_cspoint p2 dx3 dx4 pp_cspoint p5 dx6
  | Flex1(p1, p2, p3, p4, p5, d6)      -> pp fmt "Flex1(%a, %a, %a, %a, %a, %d)" pp_cspoint p1 pp_cspoint p2 pp_cspoint p3 pp_cspoint p4 pp_cspoint p5 d6


let access_subroutine (idx : subroutine_index) (i : int) : (int * int) ok =
  let arrlen = Array.length idx in
  let bias =
    if arrlen < 1240 then 107 else
      if arrlen < 33900 then 1131 else
        32768
  in
(*
  Format.fprintf fmtCFF "  # [G/L SUBR] arrlen = %d, bias = %d, i = %d, ---> %d\n" arrlen bias i (bias + i);  (* for debug *)
*)
  try
    let CharStringData(offset, len) = idx.(bias + i) in
    return (offset, len)
  with
  | Invalid_argument(_) -> err `Invalid_charstring


let rec parse_progress (gsubridx : subroutine_index) (lsubridx : subroutine_index) (woptoptprev : (int option) option) (lenrest : int) (cstate : charstring_state) (stk : int Stack.t) (d : decoder) : (int * (int option) option * charstring_state * parsed_charstring list) ok =
  d_charstring_element cstate d >>= fun (step, cstate, cselem) ->
  let lenrest = lenrest - step in
(*
  let () =  (* for debug *)
    match woptoptprev with
    | None    -> Format.fprintf fmtCFF "# X %a\n" pp_charstring_element cselem;  (* for debug *)
    | Some(_) -> Format.fprintf fmtCFF "# O %a\n" pp_charstring_element cselem;  (* for debug *)
  in  (* for debug *)
*)
  let return_with_width ret =
    match woptoptprev with
    | None    -> let wopt = pop_opt stk in return (lenrest, Some(wopt), cstate, ret)
    | Some(_) -> return (lenrest, woptoptprev, cstate, ret)
  in

  let return_cleared ret =
    return (lenrest, woptoptprev, cstate, ret)
  in

    match cselem with
    | ArgumentInteger(i) ->
        stk |> Stack.push i; return_cleared []

    | ArgumentReal(r) ->
        stk |> Stack.push (int_of_float r); return_cleared []

    | Operator(ShortKey(1)) ->  (* -- hstem (1) -- *)
        let pairlst = pop_iter pop_pair_opt stk in
        begin
          match pairlst with
          | []                 -> err `Invalid_charstring
          | (y, dy) :: csptlst -> return_with_width [HStem(y, dy, csptlst)]
        end

    | Operator(ShortKey(3)) ->  (* -- vstem (3) -- *)
        let pairlst = pop_iter pop_pair_opt stk in
        begin
          match pairlst with
          | []                 -> err `Invalid_charstring
          | (x, dx) :: csptlst -> return_with_width [VStem(x, dx, csptlst)]
        end

    | Operator(ShortKey(4)) ->  (* -- vmoveto (4) -- *)
        pop stk >>= fun arg ->
        return_with_width [VMoveTo(arg)]

    | Operator(ShortKey(5)) ->  (* -- rlineto (5) -- *)
        let csptlst = pop_iter pop_pair_opt stk in
        return_cleared [RLineTo(csptlst)]

    | Operator(ShortKey(6)) ->  (* -- hlineto (6) -- *)
        let pairlst = pop_iter pop_pair_opt stk in
        let firstopt = pop_opt stk in
        let flatlst = pairlst |> List.map (fun (a, b) -> [a; b]) |> List.concat in
        begin
          match firstopt with
          | None        -> return_cleared [HLineTo(flatlst)]
          | Some(first) -> return_cleared [HLineTo(first :: flatlst)]
        end

    | Operator(ShortKey(7)) ->  (* -- vlineto (7) -- *)
        let pairlst = pop_iter pop_pair_opt stk in
        let firstopt = pop_opt stk in
        let flatlst = pairlst |> List.map (fun (a, b) -> [a; b]) |> List.concat in
        begin
          match firstopt with
          | None        -> return_cleared [VLineTo(flatlst)]
          | Some(first) -> return_cleared [VLineTo(first :: flatlst)]
        end

    | Operator(ShortKey(8)) ->  (* -- rrcurveto (8) -- *)
        let tuplelst = pop_iter pop_6_opt stk in
        let bezierlst = tuplelst |> List.map make_bezier in
        return_cleared [RRCurveTo(bezierlst)]

    | Operator(ShortKey(10)) ->  (* -- callsubr (10) -- *)
        pop stk >>= fun i ->
        access_subroutine lsubridx i >>= fun (offset, len) ->
        let offset_init = cur_pos d in
        seek_pos offset d >>= fun () ->
        parse_charstring len cstate d stk gsubridx lsubridx woptoptprev >>= fun (woptoptsubr, cstate, accsubr) ->
        seek_pos offset_init d >>= fun () ->
        return (lenrest, woptoptsubr, cstate, Alist.to_list accsubr)

    | Operator(ShortKey(11)) ->  (* -- return (11) -- *)
        return_cleared []

    | Operator(ShortKey(14)) ->  (* -- endchar (14) -- *)
        let lst = pop_all stk in
        begin
          match woptoptprev with
          | None ->
              begin
                match lst with
                | w :: [] -> return (lenrest, Some(Some(w)), cstate, [])
                | []      -> return (lenrest, Some(None), cstate, [])
                | _       ->
(*
                    Format.fprintf fmtCFF "!!endchar1\n";
                    Format.pp_print_list pp_int fmtCFF lst;
*)
                    err `Invalid_charstring
              end

          | Some(_) ->
              begin
                match lst with
                | [] -> return_cleared []
                | _  ->
(*
                    Format.fprintf fmtCFF "!!endchar2\n";
                    Format.pp_print_list pp_int fmtCFF lst;
*)
                    err `Invalid_charstring
              end
        end

    | Operator(ShortKey(18)) ->  (* -- hstemhm (18) -- *)
        let pairlst = pop_iter pop_pair_opt stk in
        begin
          match pairlst with
          | []                 -> err `Invalid_charstring
          | (y, dy) :: csptlst -> return_with_width [HStemHM(y, dy, csptlst)]
        end

    | HintMaskOperator(arg) ->
        if Stack.length stk = 0 then
          return_with_width [HintMask(arg)]
        else
          let pairlst = pop_iter pop_pair_opt stk in
          begin
            match pairlst with
            | []                 -> err `Invalid_charstring
            | (x, dx) :: csptlst -> return_with_width [VStemHM(x, dx, csptlst); HintMask(arg)]
          end


    | CntrMaskOperator(arg) ->
        if Stack.length stk = 0 then
          return_with_width [CntrMask(arg)]
        else
          let pairlst = pop_iter pop_pair_opt stk in
          begin
            match pairlst with
            | []                 -> err `Invalid_charstring
            | (x, dx) :: csptlst -> return_with_width [VStemHM(x, dx, csptlst); CntrMask(arg)]
          end

    | Operator(ShortKey(21)) ->  (* -- rmoveto (21) -- *)
        pop stk >>= fun dy1 ->
        pop stk >>= fun dx1 ->
        return_with_width [RMoveTo((dx1, dy1))]

    | Operator(ShortKey(22)) ->  (* -- hmoveto (22) -- *)
        pop stk >>= fun arg ->
        return_with_width [HMoveTo(arg)]

    | Operator(ShortKey(23)) ->  (* -- vstemhm (23) -- *)
        let pairlst = pop_iter pop_pair_opt stk in
        begin
          match pairlst with
          | []                 -> err `Invalid_charstring
          | (x, dx) :: csptlst -> return_with_width [VStemHM(x, dx, csptlst)]
        end

    | Operator(ShortKey(24)) ->  (* -- rcurveline (24) -- *)
        pop stk >>= fun dyd ->
        pop stk >>= fun dxd ->
        let tuplelst = pop_iter pop_6_opt stk in
        let bezierlst = tuplelst |> List.map make_bezier in
        return_cleared [RRCurveTo(bezierlst); RLineTo([(dxd, dyd)])]

    | Operator(ShortKey(25)) ->  (* -- rlinecurve (25) -- *)
        pop stk >>= fun dyd ->
        pop stk >>= fun dxd ->
        pop stk >>= fun dyc ->
        pop stk >>= fun dxc ->
        pop stk >>= fun dyb ->
        pop stk >>= fun dxb ->
        let pairlst = pop_iter pop_pair_opt stk in
        return_cleared [RLineTo(pairlst); RRCurveTo([((dxb, dyb), (dxc, dyc), (dxd, dyd))])]

    | Operator(ShortKey(26)) ->  (* -- vvcurveto (26) --*)
        let tuplelst = pop_iter pop_4_opt stk in
        let retlst = tuplelst |> List.map (fun (dya, dxb, dyb, dyc) -> (dya, (dxb, dyb), dyc)) in
        let dx1opt = pop_opt stk in
        return_cleared [VVCurveTo(dx1opt, retlst)]

    | Operator(ShortKey(27)) ->  (* -- hhcurveto (27) -- *)
        let tuplelst = pop_iter pop_4_opt stk in
        let retlst = tuplelst |> List.map (fun (dxa, dxb, dyb, dxc) -> (dxa, (dxb, dyb), dxc)) in
        let dy1opt = pop_opt stk in
        return_cleared [HHCurveTo(dy1opt, retlst)]

    | Operator(ShortKey(29)) ->  (* -- callgsubr (29) -- *)
        pop stk >>= fun i ->
        access_subroutine gsubridx i >>= fun (offset, len) ->
        let offset_init = cur_pos d in
        seek_pos offset d >>= fun () ->
        parse_charstring len cstate d stk gsubridx lsubridx woptoptprev >>= fun (woptoptsubr, cstate, accsubr) ->
        seek_pos offset_init d >>= fun () ->
        return (lenrest, woptoptsubr, cstate, Alist.to_list accsubr)

    | Operator(ShortKey(30)) ->  (* -- vhcurveto (30) -- *)
        begin
          if is_odd_length stk then
            pop stk >>= fun dxf ->
            return (Some(dxf))
          else
            return None
        end >>= fun lastopt ->
      let tuplelst = pop_iter pop_4_opt stk in
      let retlst = tuplelst |> List.map (fun (d1, d2, d3, d4) -> (d1, (d2, d3), d4)) in
      return_cleared [VHCurveTo(retlst, lastopt)]


    | Operator(ShortKey(31)) ->  (* -- hvcurveto (31) -- *)
        begin
          if is_odd_length stk then
            pop stk >>= fun dxf ->
            return (Some(dxf))
          else
            return None
        end >>= fun lastopt ->
        let tuplelst = pop_iter pop_4_opt stk in
        let retlst = tuplelst |> List.map (fun (d1, d2, d3, d4) -> (d1, (d2, d3), d4)) in
        return_cleared [HVCurveTo(retlst, lastopt)]

    | Operator(ShortKey(i)) ->
        err `Invalid_charstring

    | Operator(LongKey(i))
        when List.mem i [9; 10; 11; 12; 14; 18; 23; 24; 26; 27; 28; 29; 30] ->
        failwith (Printf.sprintf "unsupported arithmetic operator '12 %d'" i)

    | Operator(LongKey(34)) ->  (* -- hflex (12 34) -- *)
        pop stk >>= fun dx6 ->
        pop stk >>= fun dx5 ->
        pop stk >>= fun dx4 ->
        pop stk >>= fun dx3 ->
        pop stk >>= fun dy2 ->
        pop stk >>= fun dx2 ->
        pop stk >>= fun dx1 ->
        return_cleared [HFlex(dx1, (dx2, dy2), dx3, dx4, dx5, dx6)]

    | Operator(LongKey(35)) ->  (* -- flex (12 35) -- *)
        pop stk >>= fun fd ->
        pop stk >>= fun dy6 ->
        pop stk >>= fun dx6 ->
        pop stk >>= fun dy5 ->
        pop stk >>= fun dx5 ->
        pop stk >>= fun dy4 ->
        pop stk >>= fun dx4 ->
        pop stk >>= fun dy3 ->
        pop stk >>= fun dx3 ->
        pop stk >>= fun dy2 ->
        pop stk >>= fun dx2 ->
        pop stk >>= fun dy1 ->
        pop stk >>= fun dx1 ->
        return_cleared [Flex((dx1, dy1), (dx2, dy2), (dx3, dy3), (dx4, dy4), (dx5, dy5), (dx6, dy6), fd)]

    | Operator(LongKey(36)) ->  (* -- hflex1 (12 36) -- *)
        pop stk >>= fun dx6 ->
        pop stk >>= fun dy5 ->
        pop stk >>= fun dx5 ->
        pop stk >>= fun dx4 ->
        pop stk >>= fun dx3 ->
        pop stk >>= fun dy2 ->
        pop stk >>= fun dx2 ->
        pop stk >>= fun dy1 ->
        pop stk >>= fun dx1 ->
        return_cleared [HFlex1((dx1, dy1), (dx2, dy2), dx3, dx4, (dx5, dy5), dx6)]

    | Operator(LongKey(37)) ->  (* -- flex1 (12 37) -- *)
        pop stk >>= fun d6 ->
        pop stk >>= fun dy5 ->
        pop stk >>= fun dx5 ->
        pop stk >>= fun dy4 ->
        pop stk >>= fun dx4 ->
        pop stk >>= fun dy3 ->
        pop stk >>= fun dx3 ->
        pop stk >>= fun dy2 ->
        pop stk >>= fun dx2 ->
        pop stk >>= fun dy1 ->
        pop stk >>= fun dx1 ->
        return_cleared [Flex1((dx1, dy1), (dx2, dy2), (dx3, dy3), (dx4, dy4), (dx5, dy5), d6)]

    | Operator(LongKey(i)) ->
        err `Invalid_charstring


and parse_charstring (len : int) (cstate : charstring_state) (d : decoder) (stk : int Stack.t) (gsubridx : subroutine_index) (lsubridx : subroutine_index) (woptoptinit : (int option) option) : ((int option) option * charstring_state * parsed_charstring Alist.t) ok =
  let rec aux lenrest (woptoptprev, cstate, acc) =
    parse_progress gsubridx lsubridx woptoptprev lenrest cstate stk d >>= fun (lenrest, woptopt, cstate, parsed) ->
    let accnew = Alist.append acc parsed in
    if lenrest = 0 then
      return (woptopt, cstate, accnew)
    else
      if lenrest < 0 then err `Invalid_charstring else
        match woptopt with
        | None    -> aux lenrest (woptoptprev, cstate, accnew)
        | Some(_) -> aux lenrest (woptopt, cstate, accnew)
  in
    aux len (woptoptinit, cstate, Alist.empty)


let select_fd_index (fdselect : fdselect) (gid : glyph_id) : fdindex ok =
  match fdselect with
  | FDSelectFormat0(arr) ->
      begin
        try return arr.(gid) with
        | Invalid_argument(_) -> err (`Invalid_fd_select(gid))
      end

  | FDSelectFormat3(lst, gid_sentinel) ->
      if gid >= gid_sentinel then
        err (`Invalid_fd_select(gid))
      else
        let opt =
          lst |> List.fold_left (fun opt (gidc, fdi) ->
            if gidc <= gid then
              Some(fdi)
            else
              opt
          ) None
        in
        begin
          match opt with
          | None      -> err (`Invalid_fd_select(gid))
          | Some(fdi) -> return fdi
        end


let select_local_subr_index (privinfo : private_info) (gid : glyph_id) : subroutine_index ok =
  match privinfo with
  | SinglePrivate(singlepriv) -> return singlepriv.local_subr_index
  | FontDicts(fdarray, fdselect) ->
      select_fd_index fdselect gid >>= fun fdindex ->
      try
        let singlepriv = fdarray.(fdindex) in
        return singlepriv.local_subr_index
      with
      | Invalid_argument(_) -> err (`Invalid_fd_index(fdindex))


let charstring ((d, gsubridx, privinfo, offset_CharString_INDEX) : charstring_info) (gid : glyph_id) : ((int option * parsed_charstring list) option) ok =
  let cstate = { numarg = 0; numstem = 0 } in
  seek_pos offset_CharString_INDEX d >>= fun () ->
  d_index_access d_charstring_data gid d >>= function
  | None ->
      return None

  | Some(CharStringData(offset, len)) ->
      let stk : int Stack.t = Stack.create () in
      seek_pos offset d >>= fun () ->
      select_local_subr_index privinfo gid >>= fun lsubridx ->
      parse_charstring len cstate d stk gsubridx lsubridx None >>= function
      | (None, _, acc)       -> err `Invalid_charstring
      | (Some(wopt), _, acc) -> return (Some((wopt, Alist.to_list acc)))


type path_element =
  | LineTo         of cspoint
  | BezierTo       of cspoint * cspoint * cspoint


let ( +@ ) (x, y) (dx, dy) = (x + dx, y + dy)

let ( +@- ) (x, y) dx = (x + dx, y)

let ( +@| ) (x, y) dy = (x, y + dy)


let line_parity (is_horizontal_init : bool) (peacc : path_element Alist.t) lst curv =
  let rec aux is_horizontal peacc lst curv =
    match lst with
    | [] ->
        (curv, peacc)

    | dt :: tail ->
        if is_horizontal then
          let curvnew = curv +@- dt in
          aux (not is_horizontal) (Alist.extend peacc (LineTo(curvnew))) tail curvnew
        else
          let curvnew = curv +@| dt in
          aux (not is_horizontal) (Alist.extend peacc (LineTo(curvnew))) tail curvnew
  in
  aux is_horizontal_init peacc lst curv


let curve_parity (is_horizontal : bool) (peacc : path_element Alist.t) lst (dtD, dvE, dsF) dtFopt curv =
  let rec aux  is_horizontal peacc lst curv =
    match lst with
    | [] ->
        if is_horizontal then
        (* -- dtD is x-directed and dsF is y-directed -- *)
          let vD = curv +@- dtD in
          let vE = vD +@ dvE in
          let vF =
            match dtFopt with
            | None      -> vE +@| dsF
            | Some(dtF) -> vE +@ (dtF, dsF)
          in
            (vF, Alist.extend peacc (BezierTo(vD, vE, vF)))
        else
          let vD = curv +@| dtD in
          let vE = vD +@ dvE in
          let vF =
            match dtFopt with
            | None      -> vE +@- dsF
            | Some(dtF) -> vE +@ (dsF, dtF)
          in
            (vF, Alist.extend peacc (BezierTo(vD, vE, vF)))

    | (dtA, dvB, dsC) :: tail ->
        if is_horizontal then
          let vA = curv +@- dtA in
          let vB = vA +@ dvB in
          let vC = vB +@| dsC in
            aux (not is_horizontal) (Alist.extend peacc (BezierTo(vA, vB, vC))) tail vC
        else
          let vA = curv +@| dtA in
          let vB = vA +@ dvB in
          let vC = vB +@- dsC in
            aux (not is_horizontal) (Alist.extend peacc (BezierTo(vA, vB, vC))) tail vC

  in
  aux is_horizontal peacc lst curv


let flex_path curv pt1 pt2 pt3 pt4 pt5 pt6 =
  let abspt1 = curv +@ pt1 in
  let abspt2 = abspt1 +@ pt2 in
  let abspt3 = abspt2 +@ pt3 in
  let abspt4 = abspt3 +@ pt4 in
  let abspt5 = abspt4 +@ pt5 in
  let abspt6 = abspt5 +@ pt6 in
  let curvnew = abspt6 in
    (curvnew, [BezierTo(abspt1, abspt2, abspt3); BezierTo(abspt4, abspt5, abspt6)])


type path = cspoint * path_element list


let charstring_absolute (csinfo : charstring_info) (gid : glyph_id) =
  charstring csinfo gid >>= function
  | None ->
      return None

  | Some((_, pcs)) ->
      pcs |> List.fold_left (fun prevres pcselem ->
        prevres >>= fun (curv, accopt) ->
(*
        Format.fprintf fmtCFF "%a\n" pp_parsed_charstring pcselem;  (* for debug *)
*)
        match pcselem with
        | HintMask(_)
        | CntrMask(_)
        | HStem(_, _, _)
        | VStem(_, _, _)
        | HStemHM(_, _, _)
        | VStemHM(_, _, _)
            -> return (curv, accopt)

        | VMoveTo(dy) ->
            let curvnew = curv +@| dy in
            begin
              match accopt with
              | None ->
                  return (curvnew, Some((curvnew, Alist.empty), Alist.empty))

              | Some(((cspt, peacc), pathacc)) ->
                  return (curvnew, Some((curvnew, Alist.empty), Alist.extend pathacc (cspt, Alist.to_list peacc)))
            end

        | HMoveTo(dx) ->
            let curvnew = curv +@- dx in
            begin
              match accopt with
              | None ->
                  return (curvnew, Some((curvnew, Alist.empty), Alist.empty))

              | Some(((cspt, peacc), pathacc)) ->
                  return (curvnew, Some((curvnew, Alist.empty), Alist.extend pathacc (cspt, Alist.to_list peacc)))
            end

        | RMoveTo(dv) ->
            let curvnew = curv +@ dv in
            begin
              match accopt with
              | None ->
                  return (curvnew, Some((curvnew, Alist.empty), Alist.empty))

              | Some(((cspt, peacc), pathacc)) ->
                  return (curvnew, Some((curvnew, Alist.empty), Alist.extend pathacc (cspt, Alist.to_list peacc)))
            end

        | RLineTo(csptlst) ->
            begin
              match accopt with
              | None -> err `Invalid_charstring
              | Some(((cspt, peacc), pathacc)) ->
                  let (curvnew, peaccnew) =
                    csptlst |> List.fold_left (fun (curv, peacc) dv ->
                      (curv +@ dv, Alist.extend peacc (LineTo(curv +@ dv)))
                    ) (curv, peacc)
                  in
                  return (curvnew, Some(((cspt, peaccnew), pathacc)))
            end

        | HLineTo(lst) ->
            begin
              match accopt with
              | None -> err `Invalid_charstring
              | Some(((cspt, peacc), pathacc)) ->
                  let (curvnew, peaccnew) = line_parity true peacc lst curv in
                  return (curvnew, Some(((cspt, peaccnew), pathacc)))
            end

        | VLineTo(lst) ->
            begin
              match accopt with
              | None -> err `Invalid_charstring
              | Some(((cspt, peacc), pathacc)) ->
                  let (curvnew, peaccnew) = line_parity false peacc lst curv in
                  return (curvnew, Some(((cspt, peaccnew), pathacc)))
            end

        | RRCurveTo(tricsptlst) ->
            begin
              match accopt with
              | None -> err `Invalid_charstring
              | Some(((cspt, peacc), pathacc)) ->
                  let (curvnew, peaccnew) =
                    tricsptlst |> List.fold_left (fun (curv, peacc) (dvA, dvB, dvC) ->
                      let vA = curv +@ dvA in
                      let vB = vA +@ dvB in
                      let vC = vB +@ dvC in
                        (vC, Alist.extend peacc (BezierTo(vA, vB, vC)))
                    ) (curv, peacc)
                  in
                  return (curvnew, Some(((cspt, peaccnew), pathacc)))
            end

        | VVCurveTo(dx1opt, (dy1, dv2, dy3) :: vvlst) ->
            begin
              match accopt with
              | None -> err `Invalid_charstring
              | Some(((cspt, peacc), pathacc)) ->
                  let v1 =
                    match dx1opt with
                    | None      -> curv +@| dy1
                    | Some(dx1) -> curv +@ (dx1, dy1)
                  in
                  let v2 = v1 +@ dv2 in
                  let v3 = v2 +@| dy3 in
                  let (curvnew, peaccnew) =
                    vvlst |> List.fold_left (fun (curv, peacc) (dyA, dvB, dyC) ->
                      let vA = curv +@| dyA in
                      let vB = vA +@ dvB in
                      let vC = vB +@| dyC in
                      (vC, Alist.extend peacc (BezierTo(vA, vB, vC)))
                    ) (v3, Alist.extend peacc (BezierTo(v1, v2, v3)))
                  in
                  return (curvnew, Some(((cspt, peaccnew), pathacc)))
            end

        | VVCurveTo(_, []) ->
            err `Invalid_charstring

        | HHCurveTo(dy1opt, (dx1, dv2, dx3) :: hhlst) ->
            let v1 =
              match dy1opt with
              | None      -> curv +@- dx1
              | Some(dy1) -> curv +@ (dx1, dy1)
            in
            let v2 = v1 +@ dv2 in
            let v3 = v2 +@- dx3 in
            begin
              match accopt with
              | None -> err `Invalid_charstring
              | Some(((cspt, peacc), pathacc)) ->
                let (curvnew, peaccnew) =
                  hhlst |> List.fold_left (fun (curv, peacc) (dxA, dvB, dxC) ->
                    let vA = curv +@- dxA in
                    let vB = vA +@ dvB in
                    let vC = vB +@- dxC in
                    (vC, Alist.extend peacc (BezierTo(vA, vB, vC)))
                  ) (v3, Alist.extend peacc (BezierTo(v1, v2, v3)))
                in
                return (curvnew, Some(((cspt, peaccnew), pathacc)))
            end

        | HHCurveTo(_, []) ->
            err `Invalid_charstring

        | HVCurveTo(hvlst, lastopt) ->
            begin
              match (List.rev hvlst, accopt) with
              | ([], _)   -> err `Invalid_charstring
              | (_, None) -> err `Invalid_charstring
              | (last :: revmain, Some(((cspt, peacc), pathacc))) ->
                  let hvlstmain = List.rev revmain in
                  let (curvnew, peaccnew) = curve_parity true peacc hvlstmain last lastopt curv in
                  return (curvnew, Some(((cspt, peaccnew), pathacc)))
            end

        | VHCurveTo(vhlst, lastopt) ->
            begin
              match (List.rev vhlst, accopt) with
              | ([], _)   -> err `Invalid_charstring
              | (_, None) -> err `Invalid_charstring
              | (last :: revmain, Some(((cspt, peacc), pathacc))) ->
                  let vhlstmain = List.rev revmain in
                  let (curvnew, peaccnew) = curve_parity false peacc vhlstmain last lastopt curv in
                  return (curvnew, Some(((cspt, peaccnew), pathacc)))
            end

        | Flex(pt1, pt2, pt3, pt4, pt5, pt6, _) ->
            begin
              match accopt with
              | None -> err `Invalid_charstring
              | Some(((cspt, peacc), pathacc)) ->
                  let (curvnew, pelstflex) = flex_path curv pt1 pt2 pt3 pt4 pt5 pt6 in
                  let peaccnew = Alist.append peacc pelstflex in
                  return (curvnew, Some((cspt, peaccnew), pathacc))
            end

        | HFlex(dx1, (dx2, dy2), dx3, dx4, dx5, dx6) ->
            begin
              match accopt with
              | None -> err `Invalid_charstring
              | Some(((cspt, peacc), pathacc)) ->
                  let (curvnew, pelstflex) = flex_path curv (dx1, 0) (dx2, dy2) (dx3, 0) (dx4, 0) (dx5, -dy2) (dx6, 0) in
                  let peaccnew = Alist.append peacc pelstflex in
                  return (curvnew, Some(((cspt, peaccnew), pathacc)))
            end

        | HFlex1((dx1, dy1), (dx2, dy2), dx3, dx4, (dx5, dy5), dx6) ->
            begin
              match accopt with
              | None -> err `Invalid_charstring
              | Some(((cspt, peacc), pathacc)) ->
                  let dy6 = - (dy1 + dy2 + dy5) in
                  let (curvnew, pelstflex) = flex_path curv (dx1, dy1) (dx2, dy2) (dx3, 0) (dx4, 0) (dx5, dy5) (dx6, dy6) in
                  let peaccnew = Alist.append peacc pelstflex in
                  return (curvnew, Some(((cspt, peaccnew), pathacc)))
            end

        | Flex1(pt1, pt2, pt3, pt4, pt5, d6) ->
            begin
              match accopt with
              | None -> err `Invalid_charstring
              | Some(((cspt, peacc)), pathacc) ->
                  let (dxsum, dysum) = pt1 +@ pt2 +@ pt3 +@ pt4 +@ pt5 in
                  let (xstart, ystart) = curv in
                  let abspt1 = curv +@ pt1 in
                  let abspt2 = abspt1 +@ pt2 in
                  let abspt3 = abspt2 +@ pt3 in
                  let abspt4 = abspt3 +@ pt4 in
                  let abspt5 = abspt4 +@ pt5 in
                  let (absx5, absy5) = abspt5 in
                  let abspt6 =
                    if abs dxsum > abs dysum then
                      (absx5 + d6, ystart)
                    else
                      (xstart, absy5 + d6)
                  in
                  let curvnew = abspt6 in
                  let pelstflex = [BezierTo(abspt1, abspt2, abspt3); BezierTo(abspt4, abspt5, abspt6)] in
                  let peaccnew = Alist.append peacc pelstflex in
                  return (curvnew, Some(((cspt, peaccnew), pathacc)))
            end

      ) (return ((0, 0), None))
    >>= function
    | (_, None)                           -> return (Some([]))
    | (_, Some(((cspt, peacc), pathacc))) -> return (Some(Alist.to_list (Alist.extend pathacc (cspt, Alist.to_list peacc))))


type bbox =
  | BBoxInital
  | BBox       of csx * csx * csy * csy


let update_bbox bbox (x1, y1) (x2, y2) =
  let xminnew = min x1 x2 in
  let xmaxnew = max x1 x2 in
  let yminnew = min y1 y2 in
  let ymaxnew = max y1 y2 in
    match bbox with
    | BBox(xmin, xmax, ymin, ymax) -> BBox(min xmin xminnew, max xmax xmaxnew, min ymin yminnew, max ymax ymaxnew)
    | BBoxInital                   -> BBox(xminnew, xmaxnew, yminnew, ymaxnew)


let bezier_bbox (x0, y0) (x1, y1) (x2, y2) (x3, y3) =
  let ( ~$ ) = float_of_int in

  let bezier_point t r0 r1 r2 r3 =
    if t < 0. then r0 else
      if 1. < t then r3 else
        let c1 = ~$ (3 * (-r0 + r1)) in
        let c2 = ~$ (3 * (r0 - 2 * r1 + r2)) in
        let c3 = ~$ (-r0 + 3 * (r1 - r2) + r3) in
          int_of_float @@ (~$ r0) +. t *. (c1 +. t *. (c2 +. t *. c3))
  in

  let aux r0 r1 r2 r3 =
    let a = -r0 + 3 * (r1 - r2) + r3 in
    let b = 2 * (r0 - 2 * r1 + r2) in
    let c = -r0 + r1 in
    if a = 0 then
      [r0; r3]
    else
      let det = b * b - 4 * a * c in
      if det < 0 then
        [r0; r3]
      else
        let delta = sqrt (~$ det) in
        let t_plus  = (-. (~$ b) +. delta) /. (~$ (2 * a)) in
        let t_minus = (-. (~$ b) -. delta) /. (~$ (2 * a)) in
        [r0; r3; bezier_point t_plus r0 r1 r2 r3; bezier_point t_minus r0 r1 r2 r3]
  in

  let xoptlst = aux x0 x1 x2 x3 in
  let xmax = xoptlst |> List.fold_left max x0 in
  let xmin = xoptlst |> List.fold_left min x0 in
  let yoptlst = aux y0 y1 y2 y3 in
  let ymax = yoptlst |> List.fold_left max y0 in
  let ymin = yoptlst |> List.fold_left min y0 in
  ((xmin, ymin), (xmax, ymax))


let charstring_bbox (pathlst : path list) =
  let bbox =
    pathlst |> List.fold_left (fun bboxprev (cspt, pelst) ->
      let (_, bbox) =
        pelst |> List.fold_left (fun (v0, bboxprev) pe ->
          match pe with
          | LineTo(v1)           -> (v1, update_bbox bboxprev v0 v1)
          | BezierTo(v1, v2, v3) -> let (vb1, vb2) = bezier_bbox v0 v1 v2 v3 in (v3, update_bbox bboxprev vb1 vb2)
        ) (cspt, bboxprev)
      in
        bbox
    ) BBoxInital
  in
  match bbox with
  | BBoxInital                   -> None  (* needs reconsideration *)
  | BBox(xmin, xmax, ymin, ymax) -> (Some((xmin, xmax, ymin, ymax)))


type raw_glyph = {
  old_glyph_id            : glyph_id;
  glyph_aw                : int;
  glyph_lsb               : int;
  glyph_bbox              : int * int * int * int;
  glyph_data              : string;
  glyph_data_length       : int;
  glyph_composite_offsets : (glyph_id * int) list;
}


let get_uint16 data offset =
  let b0 = Char.code (String.get data offset) in
  let b1 = Char.code (String.get data (offset + 1)) in
  (b0 lsl 8) lor b1


let get_int16 data i =
  let ui = get_uint16 data i in
  if ui >= 0x8000 then ui - 0x10000 else ui


let set_uint16 bytes offset ui =
  let b0 = ui / 256 in
  let b1 = ui mod 256 in
  begin
    Bytes.set bytes offset       (Char.chr b0);
    Bytes.set bytes (offset + 1) (Char.chr b1);
  end


let get_composite_offsets (data : string) : ((glyph_id * int) list, error) result =

  let rec loop i acc =
    let flags = get_uint16 data i in
    let i = i + 2 in
    let gid = get_uint16 data i in
    let accnew = Alist.extend acc (gid, i) in
    let i = i + 2 in
    let i = i + (if flags land 1 > 0 then 4 else 2) in
    let d =
      if flags land 8 > 0 then  (* -- scale -- *)
        2
      else if flags land 64 > 0 then  (* -- xy scale -- *)
        4
      else if flags land 128 > 0 then  (* -- m2 -- *)
        8
      else
        0
    in
    if flags land 32 > 0 then
      loop (i + d) accnew
    else
      return (Alist.to_list accnew)
  in

  let numberOfContours = get_int16 data 0 in
  if numberOfContours >= 0 then
    return []
  else
    loop 10 Alist.empty


let get_raw_glyph d gid =
  loca_with_length d gid >>= function
  | None ->
      return {
        old_glyph_id = gid;
        glyph_aw = 0;
        glyph_lsb = 0;
        glyph_bbox = (0, 0, 0, 0);
        glyph_data = "";
        glyph_data_length = 0;
        glyph_composite_offsets = [];
      }

  | Some((loc, len)) ->
      init_decoder d >>=
      init_glyf d >>= fun pos_glyf ->
      let pos = pos_glyf + loc in
      seek_pos pos d >>= fun () ->
      d_int16 d >>= fun _ ->
      d_int16 d >>= fun xmin ->
      d_int16 d >>= fun ymin ->
      d_int16 d >>= fun xmax ->
      d_int16 d >>= fun ymax ->
      seek_pos pos d >>= fun () ->
      d_bytes len d >>= fun data ->
      get_composite_offsets data >>= fun pairlst ->
      hmtx_single d gid >>= fun (aw, lsb) ->
      return {
        old_glyph_id = gid;
        glyph_aw = aw;
        glyph_lsb = lsb;
        glyph_bbox = (xmin, ymin, xmax, ymax);
        glyph_data = data;
        glyph_data_length = len;
        glyph_composite_offsets = pairlst;
      }


module Encode = struct

  type raw_table = {
    table_tag            : tag;
    table_content_length : int;
    table_padded_length  : int;
    table_checksum       : wint;
    table_data           : string;
  }

  type encoder = {
    buffer : Buffer.t;
  }

  let create_encoder () =
    { buffer = Buffer.create 0x10000; }
      (* -- the initial size is an arbitrary positive number -- *)

(*
  let make_raw_glyph aw lsb bbox data =
    let len = String.length data in
      {
        glyph_aw          = aw;
        glyph_lsb         = lsb;
        glyph_bbox        = bbox;
        glyph_data        = data;
        glyph_data_length = len;
      }
*)
  let pad_data s len =
    let r = (4 - len mod 4) mod 4 in
    let sp = s ^ (String.make r (Char.chr 0)) in
    (sp, len + r)


  let add_checksum (x : wint) (y : wint) : wint =
    let open WideInt in
      let q = (of_int 1) lsl 32 in
      (x +% y) mod q


  let table_checksum (sp : string) (lenp : int) : wint =
    let open WideInt in
      let rec aux acc i =
        if i >= lenp then acc else
          let b0 = of_byte (String.get sp i) in
          let b1 = of_byte (String.get sp (i + 1)) in
          let b2 = of_byte (String.get sp (i + 2)) in
          let b3 = of_byte (String.get sp (i + 3)) in
          let ui = (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3 in
          let accnew = add_checksum acc ui in
          aux accnew (i + 4)
      in
      aux (of_int 0) 0


  let to_raw_table tag enc =
    let buf = enc.buffer in
    let len = Buffer.length buf in
    let s = Buffer.contents buf in
    let (sp, lenp) = pad_data s len in
    let chksum = table_checksum sp lenp in
(*
    Printf.printf "table checksum: %d\n" chksum;
*)
      {
        table_tag            = tag;
        table_content_length = len;
        table_padded_length  = lenp;
        table_checksum       = chksum;
        table_data           = sp;
      }


  let compare_table rawtbl1 rawtbl2 =
    Tag.compare rawtbl1.table_tag rawtbl2.table_tag


  let enc_byte enc ch =
      Buffer.add_char enc.buffer ch


  let enc_uint16_unsafe enc ui =
    let b0 = ui lsr 8 in
    let b1 = ui - (b0 lsl 8) in
(*
    Printf.printf "uint16 %d --> (%d, %d)\n" ui b0 b1;
*)
    begin
      enc_byte enc (Char.chr b0);
      enc_byte enc (Char.chr b1);
    end


  let enc_uint32_unsafe enc (ui : wint) =
    let (b0, b1, b2, b3) = cut_uint32_unsafe ui in
    begin
      enc_byte enc b0;
      enc_byte enc b1;
      enc_byte enc b2;
      enc_byte enc b3;
    end


  let enc_uint8 enc (ui : int) =
    if not (0 <= ui && ui < 256) then
      err (`Not_encodable_as_uint8(ui))
    else
      begin
        enc_byte enc (Char.chr ui);
        return ()
      end


  let enc_uint16 enc ui =
    if not (0 <= ui && ui < 0x10000) then
      err (`Not_encodable_as_uint16(ui))
    else
      begin
        enc_uint16_unsafe enc ui;
        return ()
      end


  let enc_int16 enc i =
    if not (-0x8000 <= i && i < 0x8000) then
      err (`Not_encodable_as_int16(i))
    else
      let ui = if i < 0 then i + 0x10000 else i in
      begin
        enc_uint16_unsafe enc ui;
        return ()
      end


  let enc_uint32 enc ui =
    let open WideInt in
      if not (is_in_uint32 ui) then
        err (`Not_encodable_as_uint32(ui))
      else
        begin
          enc_uint32_unsafe enc ui;
          return ()
        end


  let enc_int32 enc i =
    let open WideInt in
    if not (is_in_int32 i) then
      err (`Not_encodable_as_int32(i))
    else
      let ui = if is_neg i then i +% (!%% 0x100000000L) else i in
(*
      Printf.printf "i32 -> u32 (%d ---> %d)\n" i ui;
*)
      begin
        enc_uint32_unsafe enc ui;
        return ()
      end


  let enc_time enc itime =
    let open WideInt in
      if not (is_in_int64 itime) then
        (* -- does NOT allow negative value for clarity -- *)
        err (`Not_encodable_as_time(itime))
      else
        let wi = if is_neg itime then !% 0 else itime in  (* temporary *)
        let q0 = wi lsr 32 in
        let q1 = wi -% (q0 lsl 32) in
(*
      Printf.printf "time %s ---> (%d, %d)\n" (Int64.to_string ui64) q0 q1;
*)
      begin
        enc_uint32_unsafe enc q0;
        enc_uint32_unsafe enc q1;
        return ()
      end


  let enc_direct enc s =
    begin
(*
      Printf.printf "direct \"%s\"\n" s;
*)
      Buffer.add_string enc.buffer s;
      return ()
    end


  let calculate_header_constants numTables =
    let rec aux n a =
      let anext = a * 2 in
      if anext <= numTables then
        aux (n + 1) anext
      else
        (a * 16, n)
    in
    aux 0 1


  let make_font_file rawtbllst =
    let sfntVersion = !% 0x00010000 in
    let numTables = List.length rawtbllst in
    let (searchRange, entrySelector) = calculate_header_constants numTables in
    let rangeShift = numTables * 16 - searchRange in
    let rawtbllst = List.sort compare_table rawtbllst in

(*
    Printf.printf "# make_font_file: header\n";
*)
  (* -- outputs the header -- *)
    let enc_header = create_encoder () in
    enc_uint32 enc_header sfntVersion   >>= fun () ->
    enc_uint16 enc_header numTables     >>= fun () ->
    enc_uint16 enc_header searchRange   >>= fun () ->
    enc_uint16 enc_header entrySelector >>= fun () ->
    enc_uint16 enc_header rangeShift    >>= fun () ->
    let data_header = Buffer.contents enc_header.buffer in
    let chksum_init = table_checksum data_header 12 in

    let enc = create_encoder () in
    enc_direct enc data_header >>= fun () ->

(*
    Printf.printf "# make_font_file: table directories\n";
*)
  (* -- outputs all table directories -- *)
    let offset_checkSumAdjustment_ref = ref None in
    let offset_init = 12 + numTables * 16 in
    rawtbllst |> List.fold_left (fun res rawtbl ->
      res >>= fun (offset, chksum) ->
      let strtag = Tag.to_bytes rawtbl.table_tag in
(*
      Printf.printf "## for '%s'\n" strtag;
*)
      enc_direct enc strtag                             >>= fun () ->
      enc_uint32 enc rawtbl.table_checksum              >>= fun () ->
      enc_uint32 enc (!% offset)                        >>= fun () ->
      enc_uint32 enc (!% (rawtbl.table_content_length)) >>= fun () ->
      if strtag = "head" then
        offset_checkSumAdjustment_ref := Some(offset + 8)
      else
        ();
      let offsetnew = offset + rawtbl.table_padded_length in
      let chksumnew = add_checksum chksum rawtbl.table_checksum in
(*
      Printf.printf "  $ checksum %d + %d ---> %d\n" chksum rawtbl.table_checksum chksumnew;
*)
      return (offsetnew, chksumnew)
    ) (return (offset_init, chksum_init)) >>= fun (_, chksum) ->

(*
    Printf.printf "# make_font_file: tables\n";
*)
  (* -- outputs all tables -- *)
    rawtbllst |> List.fold_left (fun res rawtbl ->
      res >>= fun () ->
      enc_direct enc rawtbl.table_data
    ) (return ()) >>= fun () ->

(*
    Printf.printf "# make_font_file: update 'checkSumAdjustment'\n";
*)
    let checkSumAdjustment =
      let temp = (!% 0xB1B0AFBA) -% chksum in
        if WideInt.is_neg temp then temp +% (!% (1 lsl 32)) else temp
    in
    match !offset_checkSumAdjustment_ref with
    | None ->
        err `Missing_head_table_for_encoding

    | Some(offset) ->
        let bytes = Buffer.to_bytes enc.buffer in
        let (b0, b1, b2, b3) = cut_uint32_unsafe checkSumAdjustment in
        Bytes.set bytes offset       b0;
        Bytes.set bytes (offset + 1) b1;
        Bytes.set bytes (offset + 2) b2;
        Bytes.set bytes (offset + 3) b3;
        let data = Bytes.to_string bytes in
        return data


  let empty_cmap () : raw_table ok =
    let enc = create_encoder () in
    enc_uint16 enc 0 >>= fun () ->  (* -- 'Version' -- *)
    enc_uint16 enc 0 >>= fun () ->  (* -- 'numTables' -- *)
    let rawtbl = to_raw_table Tag.cmap enc in
    return rawtbl


  let head (h : head) : raw_table ok =
(*
    Printf.printf "# 'head' table\n";
*)
    let enc = create_encoder () in
    let fontRevision = h.head_font_revision in
    let ilocfmt =
      match h.head_index_to_loc_format with
      | ShortLocFormat -> 0
      | LongLocFormat  -> 1
    in
    enc_uint32 enc (!% 0x00010000)     >>= fun () ->  (* -- Table Version Number -- *)
    enc_uint32 enc fontRevision        >>= fun () ->
    enc_uint32 enc (!% 0)              >>= fun () ->  (* -- 'checkSumAdjustment': will be updated afterwards -- *)
    enc_uint32 enc (!% 0x5F0F3CF5)     >>= fun () ->  (* -- 'magicNumber' -- *)
    enc_uint16 enc h.head_flags        >>= fun () ->
    enc_uint16 enc h.head_units_per_em >>= fun () ->
    enc_time   enc h.head_created      >>= fun () ->
    enc_time   enc h.head_modified     >>= fun () ->
    enc_int16  enc h.head_xmin         >>= fun () ->
    enc_int16  enc h.head_ymin         >>= fun () ->
    enc_int16  enc h.head_xmax         >>= fun () ->
    enc_int16  enc h.head_ymax         >>= fun () ->
    enc_uint16 enc h.head_mac_style    >>= fun () ->
    enc_uint16 enc h.head_lowest_rec_ppem     >>= fun () ->
    enc_int16  enc 0                          >>= fun () ->  (* -- 'fontDirectionHint' -- *)
    enc_int16  enc ilocfmt                    >>= fun () ->
    enc_int16  enc 0                          >>= fun () ->  (* -- 'glyphDataFormat' -- *)
    let rawtbl = to_raw_table Tag.head enc in
    return rawtbl


  let hhea (numberOfHMetrics : int) (h : hhea) : raw_table ok =
(*
    Printf.printf "# 'hhea' table\n";
*)
    let enc = create_encoder () in
    enc_uint32 enc (!% 0x00010000)               >>= fun () ->
    enc_int16  enc h.hhea_ascender               >>= fun () ->
    enc_int16  enc h.hhea_descender              >>= fun () ->
    enc_int16  enc h.hhea_line_gap               >>= fun () ->
    enc_uint16 enc h.hhea_advance_width_max      >>= fun () ->
    enc_int16  enc h.hhea_min_left_side_bearing  >>= fun () ->
    enc_int16  enc h.hhea_min_right_side_bearing >>= fun () ->
    enc_int16  enc h.hhea_xmax_extent            >>= fun () ->
    enc_int16  enc h.hhea_caret_slope_rise       >>= fun () ->
    enc_int16  enc h.hhea_caret_slope_run        >>= fun () ->
    enc_int16  enc h.hhea_caret_offset           >>= fun () ->
    enc_int16  enc 0                             >>= fun () ->
    enc_int16  enc 0                             >>= fun () ->
    enc_int16  enc 0                             >>= fun () ->
    enc_int16  enc 0                             >>= fun () ->
    enc_int16  enc 0                             >>= fun () ->  (* -- 'metricDataFormat' -- *)
    enc_uint16 enc numberOfHMetrics              >>= fun () ->
(*
    Printf.printf "# end 'hhea' table\n";
*)
    let rawtbl = to_raw_table Tag.hhea enc in
    return rawtbl


  let maxp (m : maxp) : raw_table ok =
(*
    Printf.printf "# 'maxp' table\n";
*)
    let enc = create_encoder () in
    enc_uint32 enc (!% 0x00010000)                 >>= fun () ->  (* -- Table version number -- *)
    enc_uint16 enc m.maxp_num_glyphs               >>= fun () ->
    enc_uint16 enc m.maxp_max_points               >>= fun () ->
    enc_uint16 enc m.maxp_max_contours             >>= fun () ->
    enc_uint16 enc m.maxp_max_composite_points     >>= fun () ->
    enc_uint16 enc m.maxp_max_composite_contours   >>= fun () ->
    enc_uint16 enc m.maxp_max_zones                >>= fun () ->
    enc_uint16 enc m.maxp_max_twilight_points      >>= fun () ->
    enc_uint16 enc m.maxp_max_storage              >>= fun () ->
    enc_uint16 enc m.maxp_max_function_defs        >>= fun () ->
    enc_uint16 enc m.maxp_max_instruction_defs     >>= fun () ->
    enc_uint16 enc m.maxp_max_stack_elements       >>= fun () ->
    enc_uint16 enc m.maxp_max_size_of_instructions >>= fun () ->
    enc_uint16 enc m.maxp_max_component_elements   >>= fun () ->
    enc_uint16 enc m.maxp_max_component_depth      >>= fun () ->
(*
    Printf.printf "# end 'maxp' table\n";
*)
    let rawtbl = to_raw_table Tag.maxp enc in
    return rawtbl


  type glyph_output_info = {

  (* -- main table data -- *)
    hmtx : raw_table;
    glyf : raw_table;
    loca : raw_table;

  (* -- for 'maxp' table -- *)
    number_of_glyphs : int;

  (* -- for 'head' table -- *)
    xmin                : int;
    ymin                : int;
    xmax                : int;
    ymax                : int;
    index_to_loc_format : loc_format;

  (* -- for 'hhea' table -- *)
    advance_width_max      : int;
    min_left_side_bearing  : int;
    min_right_side_bearing : int;
    x_max_extent           : int;
    number_of_h_metrics    : int;
  }


  let update_bbox (xMin0, yMin0, xMax0, yMax0) (xMin1, yMin1, xMax1, yMax1) =
    (min xMin0 xMin1, min yMin0 yMin1, max xMax0 xMax1, max yMax0 yMax1)


  let get_right_side_bearing g =
    let (xmin, _, xmax, _) = g.glyph_bbox in
    g.glyph_aw - (g.glyph_lsb + xmax - xmin)


  let get_x_extent g =
    let (xmin, _, xmax, _) = g.glyph_bbox in
    g.glyph_lsb + (xmax - xmin)


  module OldToNewGlyphID = Hashtbl.Make
    (struct
      type t = glyph_id
      let equal = ( = )
      let hash = Hashtbl.hash
    end)


  let fix_composite_element_glyph_id (ht : glyph_id OldToNewGlyphID.t) (g : raw_glyph) : string =
    let bytes = Bytes.of_string g.glyph_data in
    let pairlst = g.glyph_composite_offsets in
    pairlst |> List.iter (fun (oldgid, offset) ->
      match OldToNewGlyphID.find_opt ht oldgid with
      | None         -> set_uint16 bytes offset 0
      | Some(newgid) -> set_uint16 bytes offset newgid
    );
    Bytes.to_string bytes



  let truetype_outline_tables (glyphlst : raw_glyph list) =
(*
    Printf.printf "# 'hmtx', 'glyf', and 'loca' table\n";
*)

    let numGlyphs = List.length glyphlst in

    let ht = OldToNewGlyphID.create (numGlyphs * 2) in
    glyphlst |> List.iteri (fun gidnew rg ->
      OldToNewGlyphID.add ht rg.old_glyph_id gidnew
    );

    if numGlyphs > 65536 then
      err (`Too_many_glyphs_for_encoding(numGlyphs))
    else
      begin
        match glyphlst with
        | []          -> err `No_glyph_for_encoding
        | gfirst :: _ -> return gfirst
      end >>= fun gfirst ->
      let lsb_init = gfirst.glyph_lsb in
      let rsb_init = get_right_side_bearing gfirst in
      let xext_init = get_x_extent gfirst in

      let numberOfHMetrics = numGlyphs in

    (* -- outputs 'hmtx' table and calculates (xMin, yMin, xMax, yMax) -- *)
      let enc_hmtx = create_encoder () in
      let bbox_init = (0, 0, 0, 0) in
      let aw_init = 0 in
      glyphlst |> List.fold_left (fun res g ->
        res >>= fun (bbox, aw, lsb, rsb, xext) ->
        enc_uint16 enc_hmtx g.glyph_aw  >>= fun () ->
        enc_int16  enc_hmtx g.glyph_lsb >>= fun () ->
        let bboxnew = update_bbox bbox g.glyph_bbox in
        let awnew = max aw g.glyph_aw in
        let lsbnew = min lsb g.glyph_lsb in
        let rsbnew = min rsb (get_right_side_bearing g) in
        let xextnew = max xext (get_x_extent g) in
        return (bboxnew, awnew, lsbnew, rsbnew, xextnew)
      ) (return (bbox_init, aw_init, lsb_init, rsb_init, xext_init))
        >>= fun ((xMin, yMin, xMax, yMax), awmax, minlsb, minrsb, xmaxext) ->
      let rawtbl_hmtx = to_raw_table Tag.hmtx enc_hmtx in

    (* -- outputs 'glyf' table and 'loca' table -- *)
      let enc_glyf = create_encoder () in
      let enc_loca = create_encoder () in
      let lenwhole =
        glyphlst |> List.fold_left (fun acc g -> acc + g.glyph_data_length) 0
      in
      let (locfmt, enc_for_loca) =
        if lenwhole <= 2 * (1 lsl 16) then
          (ShortLocFormat, (fun offset -> enc_uint16 enc_loca (offset / 2)))
        else
          (LongLocFormat, (fun offset -> enc_uint32 enc_loca (!% offset)))
      in
      let offset_init = 0 in
      glyphlst |> List.fold_left (fun res g ->
        res >>= fun offset ->
        let data = fix_composite_element_glyph_id ht g in
        enc_direct enc_glyf data >>= fun () ->
        enc_for_loca offset >>= fun () ->
        let len = g.glyph_data_length in
        return (offset + len)
      ) (return offset_init) >>= fun offset_last ->
      enc_for_loca offset_last >>= fun () ->
      let rawtbl_glyf = to_raw_table Tag.glyf enc_glyf in
      let rawtbl_loca = to_raw_table Tag.loca enc_loca in

      return {
        hmtx = rawtbl_hmtx;
        glyf = rawtbl_glyf;
        loca = rawtbl_loca;

        number_of_glyphs = numGlyphs;

        xmin = xMin;
        ymin = yMin;
        xmax = xMax;
        ymax = yMax;
        index_to_loc_format = locfmt;

        advance_width_max      = awmax;
        min_left_side_bearing  = minlsb;
        min_right_side_bearing = minrsb;
        x_max_extent           = xmaxext;
        number_of_h_metrics    = numberOfHMetrics;
      }

end
