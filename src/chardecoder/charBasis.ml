
type uchar_segment = Uchar.t * Uchar.t list

type code_point = int

type code_point_kind =
  | CodePoint      of code_point
  | CodePointRange of code_point * code_point

type script =
  | CommonNarrow        (*   --  ; Zyyy; Common *)
  | CommonWide          (*   --  ; Zyyy; Common *)
  | Inherited           (*   --  ; Zinh; Inherited *)
  | HanIdeographic      (* 'hani'; Hani; Han *)
  | HiraganaOrKatakana  (* 'kana'; Hrkt; Hiragana_Or_Katakana *)
  | Latin               (* 'latn'; Latn; Latin *)
(* temporary; should add more scripts *)
  | OtherScript


module ScriptSchemeMap = Map.Make
  (struct
    type t = script
    let compare = Stdlib.compare
  end)


module ScriptSpaceMap = Map.Make
  (struct
    type t = script * script
    let compare = Stdlib.compare
  end)


type east_asian_width =
  | EAWHalfWidth
  | EAWFullWidth
  | EAWWide
  | EAWNarrow
  | EAWAmbiguous
  | EAWNeutral


let show_script = function
  | CommonNarrow       -> "Common (narrow)"
  | CommonWide         -> "Common (wide)"
  | Inherited          -> "Inherited"
  | HanIdeographic     -> "Han"
  | HiraganaOrKatakana -> "Kana"
  | Latin              -> "Latin"
  | OtherScript        -> "Other"


let pp_script fmt script =
  Format.fprintf fmt "%s" (show_script script)


type language_system =
  | Japanese
  | English
(* temporary; should add more language systems *)
  | NoLanguageSystem


type break_opportunity =
  | AllowBreak
  | PreventBreak

(* -- line breaking classes [UAX #14 Table 1] -- *)
type line_break_class =
(* -- non-tailorable line breaking classes -- *)
  | CM  (* combining marks *)
  | SG  (* surrogates *)
  | WJ  (* word joiner *)
  | ZW  (* zero width space *)
  | GL  (* non-breaking; glue *)
  | SP  (* space *)
  | ZWJ (* zero width joiner *)
  | B2  (* break opportunity *)
  | BA  (* break after *)
  | BB  (* break before *)
  | HY  (* hyphen *)
  | CB  (* contingent break opportunity *)
(* -- characters prohibiting certain breaks -- *)
  | CL  (* close punctuation *)
  | CP  (* close parenthesis *)
  | EX  (* exclamation *)
  | IN  (* inseparable *)
  | NS  (* nonstarter *)
  | OP  (* open punctuation *)
  | QU  (* quotation *) (* maybe not necessary *)
(* -- numeric context -- *)
  | IS  (* infix numeric separator *)
  | NU  (* numeric *)
  | PO  (* postfix numeric *)
  | PR  (* prefix numeric *)
  | SY  (* symbols allowing break after *)
(* -- other characters -- *)
  | AI  (* ambiguous (alphabetic or ideographic) *)
  | AL  (* alphabetic *)
  | CJ  (* conditional Japanese starter *)
  | EB  (* emoji base *)  (* maybe not necessary *)
  | EM  (* empji modifier *)  (* maybe not necessary *)
  | H2  (* Hangul LV syllable *)
  | H3  (* Hangul LVT syllable *)
  | HL  (* Hebrew letter *)
  | ID  (* ideographic *)
  | JL  (* Hangul L Jamo *)
  | JV  (* Hangul V Jamo *)
  | JT  (* Hangul T Jamo *)
  | RI  (* regional indicator *)
  | SA  (* complex context dependent; south east Asian *)
  | XX  (* unknown *)
(* -- original classes -- *)
  | INBR  (* input break *)
  | JLOP  (* JLreq cl-01; fullwidth open punctuation *)
  | JLCP  (* JLreq cl-02; fullwidth close punctuation *)
  | JLHY  (* JLreq cl-03; hyphens *)
  | JLNS  (* JLreq cl-04; kugiri yakumonos (fullwidth nonstarter) *)
  | JLMD  (* JLreq cl-05; nakatens (fullwidth middle dot, fullwidth semicolon, etc.) *)
  | JLFS  (* JLreq cl-06; kuten (fullwidth full stops) *)
  | JLCM  (* JLreq cl-07; touten (fullwidth commas) *)
  | JLPL  (* JLreq cl-10; prolonged sound mark *)
  | JLSM  (* JLreq cl-11; small kanas *)


let is_ideographic_class = function
  | CJ | ID | JLOP | JLCP | JLHY | JLNS | JLMD | JLFS | JLCM | JLPL | JLSM
      -> true
  | _ -> false


let is_open_punctuation = function
  | OP | QU | JLOP
      -> true
  | _ -> false


let is_close_punctuation = function
  | CL | CP | QU | NS | JLCP | JLNS | JLCM | JLFS
      -> true
  | _ -> false


type line_break_regexp_element =
  | LBRESet       of line_break_class list   (* [a ... a] *)
  | LBRENotOf     of line_break_class list   (* [^ a ... a] *)
  | LBREStar      of line_break_regexp       (* e* *)

and line_break_regexp = line_break_regexp_element list


let script_equal = (=)


let add_to_map cp scr umap =
  umap |> BatIMap.add cp scr


let add_range_to_map cp1 cp2 scr umap =
  umap |> BatIMap.add_range cp1 cp2 scr


let map_of_list (type a) (readf : int -> string -> a) (lst : (code_point_kind * string) list) =
  lst |> List.fold_left (fun accmap elem ->
    match elem with
    | (CodePoint(cp), data)            -> accmap |> add_to_map cp (readf cp data)
    | (CodePointRange(cp1, cp2), data) -> accmap |> add_range_to_map cp1 cp2 (readf cp1 data)
        (* -- needs reconsideration: 'readf' receives the first code point of the range -- *)
  ) (BatIMap.empty ~eq:(=))


(* for debug *)
let show_lb_class lbc =
  match lbc with
  | CM  -> "CM"
  | SG  -> "SG"
  | WJ  -> "WJ"
  | ZW  -> "ZW"
  | GL  -> "GL"
  | SP  -> "SP"
  | ZWJ -> "ZWJ"
  | B2  -> "B2"
  | BA  -> "BA"
  | BB  -> "BB"
  | HY  -> "HY"
  | CB  -> "CB"
  | CL  -> "CL"
  | CP  -> "CP"
  | EX  -> "EX"
  | IN  -> "IN"
  | NS  -> "NS"
  | OP  -> "OP"
  | QU  -> "QU"  (* maybe not necessary *)
  | IS  -> "IS"
  | NU  -> "NU"
  | PO  -> "PO"
  | PR  -> "PR"
  | SY  -> "SY"
  | AI  -> "AI"
  | AL  -> "AL"
  | CJ  -> "CJ"
  | EB  -> "EB"  (* maybe not necessary *)
  | EM  -> "EM"  (* maybe not necessary *)
  | H2  -> "H2"
  | H3  -> "H3"
  | HL  -> "HL"
  | ID  -> "ID"
  | JL  -> "JL"
  | JV  -> "JV"
  | JT  -> "JT"
  | RI  -> "RI"
  | SA  -> "SA"
  | XX  -> "XX"
  | INBR -> "INBR!"
  | JLOP -> "JLOP!"
  | JLCP -> "JLCP!"
  | JLHY -> "JLHY!"
  | JLNS -> "JLNS!"
  | JLMD -> "JLMD!"
  | JLFS -> "JLFS!"
  | JLCM -> "JLCM!"
  | JLPL -> "JLPL!"
  | JLSM -> "JLSM!"


(* for debug *)
let rec show_lregexp lregexp =
  lregexp |> List.map (function
  | LBRESet(lbclst)      -> "[" ^ (String.concat "|" (List.map show_lb_class lbclst)) ^ "]"
  | LBRENotOf(lbclst)    -> "[^" ^ (String.concat "|" (List.map show_lb_class lbclst)) ^ "]"
  | LBREStar(lregexpsub) -> "(" ^ (show_lregexp lregexpsub) ^ ")*"
  ) |> String.concat " "


type line_break_element = uchar_segment * line_break_class * break_opportunity
