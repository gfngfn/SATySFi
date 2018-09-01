(* Convert a CFF Type 1 Font to a Type 3 font. *)
open Pdfutil
open Pdfio

type dictop =
  | IntOp of int32
  | RealOp of float
  | Operator of int
  | Operator2 of int

let read_card8 b =
  i32toi (getval_32 b 8)

let read_card16 b =
  i32toi (getval_32 b 16)

let discard_bytes b n =
  for x = 1 to n do ignore (getval_32 b 8) done

let read_string b l =
  let chars = ref [] in
    for x = 1 to l do chars =| char_of_int (i32toi (getval_32 b 8)) done;
    implode (rev !chars)

let read_index b =
  let count = read_card16 b in
    if count = 0 then [] else
      let offsize = read_card8 b * 8
      and offsets = ref [] in
        for x = 1 to count + 1 do offsets =| i32toi (getval_32 b offsize) done;
        let offsets = rev !offsets in
          map (fun (x, y) -> y - x) (pairs offsets)

(* Read a dictionary, creating a dictop list, and parse to (key, value) pairs. l
is the length of the dictionary. *)
let rec read_float_dict_item prev n_read b =
  match i32toi (getval_32 b 4) with
  | 0xF ->
      align b;
      ((n_read + 2) / 2), Scanf.sscanf (implode (rev prev)) "%f" ident
  | 0x0 -> read_float_dict_item ('0'::prev) (n_read + 1) b
  | 0x1 -> read_float_dict_item ('1'::prev) (n_read + 1) b
  | 0x2 -> read_float_dict_item ('2'::prev) (n_read + 1) b
  | 0x3 -> read_float_dict_item ('3'::prev) (n_read + 1) b
  | 0x4 -> read_float_dict_item ('4'::prev) (n_read + 1) b
  | 0x5 -> read_float_dict_item ('5'::prev) (n_read + 1) b
  | 0x6 -> read_float_dict_item ('6'::prev) (n_read + 1) b
  | 0x7 -> read_float_dict_item ('7'::prev) (n_read + 1) b
  | 0x8 -> read_float_dict_item ('8'::prev) (n_read + 1) b
  | 0x9 -> read_float_dict_item ('9'::prev) (n_read + 1) b
  | 0xA -> read_float_dict_item ('.'::prev) (n_read + 1) b
  | 0xB -> read_float_dict_item ('E'::prev) (n_read + 1) b
  | 0xC -> read_float_dict_item ('-'::'E'::prev) (n_read + 1) b
  | 0xD -> read_float_dict_item prev (n_read + 1) b
  | 0xE -> read_float_dict_item ('-'::prev) (n_read + 1) b
  | _ -> assert false

let read_dict_item b =
  let b1 = read_card8 b in
    if b1 >= 0 && b1 <= 21 then
      if b1 = 12 then
        let b2 = read_card8 b in
          2, Operator2 b2
      else
        1, Operator b1
    else if b1 = 28 then
      let ob1 = read_card8 b in
      let ob2 = read_card8 b in
        3, IntOp (i32ofi ((ob1 lsl 8) lor ob2))
    else if b1 = 29 then
      let ob1 = getval_32 b 8 in
      let ob2 = getval_32 b 8 in
      let ob3 = getval_32 b 8 in
      let ob4 = getval_32 b 8 in
      let v =
        lor32
          (lor32 (lsl32 ob1 24) (lsl32 ob2 16))
          (lor32 (lsl32 ob3 8) ob4)
      in
        5, IntOp v
    else if b1 >= 32 && b1 <= 246 then
      1, IntOp (i32ofi (b1 - 139))
    else if b1 >= 247 && b1 <= 250 then
      let ob1 = read_card8 b in
        2, IntOp (i32ofi ((b1 - 247) * 256 + ob1 + 108))
    else if b1 >= 251 && b1 <= 254 then
      let ob1 = read_card8 b in
        2, IntOp (i32ofi ((~- (b1 - 251)) * 256 - ob1 - 108))
    else if b1 = 30 then
        let bytes_read, num = read_float_dict_item [] 0 b in
          bytes_read + 1, RealOp num
    else
      raise (Pdf.PDFError "bad operator/operand in read_dict_item")
    
let rec read_dict prev b l =
  if l <= 0 then rev prev else
    let bytes_read, op = read_dict_item b in
      read_dict (op::prev) b (l - bytes_read)

(* In code, out code *)
let standard_encoding =
  [(0, 0); (1, 0); (2, 0); (3, 0); (4, 0); (5, 0); (6, 0); (7, 0); (8, 0);
   (9, 0); (10, 0); (11, 0); (12, 0); (13, 0); (14, 0); (15, 0); (16, 0);
   (17, 0); (18, 0); (19, 0); (20, 0); (21, 0); (22, 0); (23, 0); (24, 0);
   (25, 0); (26, 0); (27, 0); (28, 0); (29, 0); (30, 0); (31, 0); (32, 1);
   (33, 2); (34, 3); (35, 4); (36, 5); (37, 6); (38, 7); (39, 8); (40, 9);
   (41, 10); (42, 11); (43, 12); (44, 13); (45, 14); (46, 15); (47, 16);
   (48, 17); (49, 18); (50, 19); (51, 20); (52, 21); (53, 22); (54, 23);
   (55, 24); (56, 25); (57, 26); (58, 27); (59, 28); (60, 29); (61, 30);
   (62, 31); (63, 32); (64, 33); (65, 34); (66, 35); (67, 36); (68, 37);
   (69, 38); (70, 39); (71, 40); (72, 41); (73, 42); (74, 43); (75, 44);
   (76, 45); (77, 46); (78, 47); (79, 48); (80, 49); (81, 50); (82, 51);
   (83, 52); (84, 53); (85, 54); (86, 55); (87, 56); (88, 57); (89, 58);
   (90, 59); (91, 60); (92, 61); (93, 62); (94, 63); (95, 64); (96, 65);
   (97, 66); (98, 67); (99, 68); (100, 69); (101, 70); (102, 71); (103, 72);
   (104, 73); (105, 74); (106, 75); (107, 76); (108, 77); (109, 78); (110, 79);
   (111, 80); (112, 81); (113, 82); (114, 83); (115, 84); (116, 85); (117, 86);
   (118, 87); (119, 88); (120, 89); (121, 90); (122, 91); (123, 92); (124, 93);
   (125, 94); (126, 95); (127, 0); (128, 0); (129, 0); (130, 0); (131, 0);
   (132, 0); (133, 0); (134, 0); (135, 0); (136, 0); (137, 0); (138, 0);
   (139, 0); (140, 0); (141, 0); (142, 0); (143, 0); (144, 0); (145, 0);
   (146, 0); (147, 0); (148, 0); (149, 0); (150, 0); (151, 0); (152, 0);
   (153, 0); (154, 0); (155, 0); (156, 0); (157, 0); (158, 0); (159, 0);
   (160, 0); (161, 96); (162, 97); (163, 98); (164, 99); (165, 100); (166, 101);
   (167, 102); (168, 103); (169, 104); (170, 105); (171, 106); (172, 107);
   (173, 108); (174, 109); (175, 110); (176, 0); (177, 111); (178, 112);
   (179, 113); (180, 114); (181, 0); (182, 115); (183, 116); (184, 117);
   (185, 118); (186, 119); (187, 120); (188, 121); (189, 122); (190, 0);
   (191, 123); (192, 0); (193, 124); (194, 125); (195, 126); (196, 127);
   (197, 128); (198, 129); (199, 130); (200, 131); (201, 0); (202, 132);
   (203, 133); (204, 0); (205, 134); (206, 135); (207, 136); (208, 137);
   (209, 0); (210, 0); (211, 0); (212, 0); (213, 0); (214, 0); (215, 0);
   (216, 0); (217, 0); (218, 0); (219, 0); (220, 0); (221, 0); (222, 0);
   (223, 0); (224, 0); (225, 138); (226, 0); (227, 139); (228, 0); (229, 0);
   (230, 0); (231, 0); (232, 140); (233, 141); (234, 142); (235, 143);
   (236, 0); (237, 0); (238, 0); (239, 0); (240, 0); (241, 144); (242, 0);
   (243, 0); (244, 0); (245, 145); (246, 0); (247, 0); (248, 146); (249, 147);
   (250, 148); (251, 149); (252, 0); (253, 0); (254, 0); (255, 0)]

let expert_encoding =
  [(0, 0); (1, 0); (2, 0); (3, 0); (4, 0); (5, 0); (6, 0); (7, 0); (8, 0);
   (9, 0); (10, 0); (11, 0); (12, 0); (13, 0); (14, 0); (15, 0); (16, 0);
   (17, 0); (18, 0); (19, 0); (20, 0); (21, 0); (22, 0); (23, 0); (24, 0);
   (25, 0); (26, 0); (27, 0); (28, 0); (29, 0); (30, 0); (31, 0); (32, 1);
   (33, 229); (34, 230); (35, 0); (36, 231); (37, 232); (38, 233); (39, 234);
   (40, 235); (41, 236); (42, 237); (43, 238); (44, 13); (45, 14); (46, 15);
   (47, 99); (48, 239); (49, 240); (50, 241); (51, 242); (52, 243); (53, 244);
   (54, 245); (55, 246); (56, 247); (57, 248); (58, 27); (59, 28); (60, 249);
   (61, 250); (62, 251); (63, 252); (64, 0); (65, 253); (66, 254); (67, 255);
   (68, 256); (69, 257); (70, 0); (71, 0); (72, 0); (73, 258); (74, 0); (75, 0);
   (76, 259); (77, 260); (78, 261); (79, 262); (80, 0); (81, 0); (82, 263);
   (83, 264); (84, 265); (85, 0); (86, 266); (87, 109); (88, 110); (89, 267);
   (90, 268); (91, 269); (92, 0); (93, 270); (94, 271); (95, 272); (96, 273);
   (97, 274); (98, 275); (99, 276); (100, 277); (101, 278); (102, 279);
   (103, 280); (104, 281); (105, 282); (106, 283); (107, 284); (108, 285);
   (109, 286); (110, 287); (111, 288); (112, 289); (113, 290); (114, 291);
   (115, 292); (116, 293); (117, 294); (118, 295); (119, 296); (120, 297);
   (121, 298); (122, 299); (123, 300); (124, 301); (125, 302); (126, 303);
   (127, 0); (128, 0); (129, 0); (130, 0); (131, 0); (132, 0); (133, 0);
   (134, 0); (135, 0); (136, 0); (137, 0); (138, 0); (139, 0); (140, 0);
   (141, 0); (142, 0); (143, 0); (144, 0); (145, 0); (146, 0); (147, 0);
   (148, 0); (149, 0); (150, 0); (151, 0); (152, 0); (153, 0); (154, 0);
   (155, 0); (156, 0); (157, 0); (158, 0); (159, 0); (160, 0); (161, 304);
   (162, 305); (163, 306); (164, 0); (165, 0); (166, 307); (167, 308);
   (168, 309); (169, 310); (170, 311); (171, 0); (172, 312); (173, 0);
   (174, 0); (175, 313); (176, 0); (177, 0); (178, 314); (179, 315);
   (180, 0); (181, 0); (182, 316); (183, 317); (184, 318); (185, 0); (186, 0);
   (187, 0); (188, 158); (189, 155); (190, 163); (191, 319); (192, 320);
   (193, 321); (194, 322); (195, 323); (196, 324); (197, 325); (198, 0);
   (199, 0); (200, 326); (201, 150); (202, 164); (203, 169); (204, 327);
   (205, 328); (206, 329); (207, 330); (208, 331); (208, 332); (209, 332);
   (210, 333); (211, 334); (212, 335); (213, 336); (214, 337); (215, 338);
   (216, 339); (217, 340); (218, 341); (219, 342); (220, 343); (221, 344);
   (222, 345); (223, 346); (224, 347); (225, 348); (226, 349); (227, 350);
   (228, 351); (229, 352); (230, 353); (231, 354); (232, 355); (233, 356);
   (234, 357); (235, 358); (236, 359); (237, 360); (238, 361); (239, 362);
   (240, 363); (241, 364); (242, 365); (243, 366); (244, 367); (245, 268);
   (246, 369); (247, 370); (248, 371); (249, 372); (250, 373); (251, 374);
   (252, 375); (253, 376); (254, 377); (255, 378)]

let sids =
  [(0, ".notdef"); (1, "space"); (2, "exclam"); (3, "quotedbl"); (4, "numbersign");
   (5, "dollar"); (6, "percent"); (7, "ampersand"); (8, "quoteright"); (9, "parenleft");
   (10, "parenright"); (11, "asterisk"); (12, "plus"); (13, "comma"); (14, "hyphen");
   (15, "period"); (16, "slash"); (17, "zero"); (18, "one"); (19, "two"); (20, "three");
   (21, "four"); (22, "five"); (23, "six"); (24, "seven"); (25, "eight"); (26, "nine");
   (27, "colon"); (28, "semicolon"); (29, "less"); (30, "equal"); (31, "greater");
   (32, "question"); (33, "at"); (34, "A"); (35, "B"); (36, "C"); (37, "D"); (38, "E");
   (39, "F"); (40, "G"); (41, "H"); (42, "I"); (43, "J"); (44, "K"); (45, "L"); (46, "M");
   (47, "N"); (48, "O"); (49, "P"); (50, "Q"); (51, "R"); (52, "S"); (53, "T"); (54, "U");
   (55, "V"); (56, "W"); (57, "X"); (58, "Y"); (59, "Z"); (60, "bracketleft");
   (61, "backslash"); (62, "bracketright"); (63, "asciicircum"); (64, "underscore");
   (65, "quoteleft"); (66, "a"); (67, "b"); (68, "c"); (69, "d"); (70, "e"); (71, "f");
   (72, "g"); (73, "h"); (74, "i"); (75, "j"); (76, "k"); (77, "l"); (78, "m"); (79, "n");
   (80, "o"); (81, "p"); (82, "q"); (83, "r"); (84, "s"); (85, "t"); (86, "u"); (87, "v");
   (88, "w"); (89, "x"); (90, "y"); (91, "z"); (92, "braceleft"); (93, "bar");
   (94, "braceright"); (95, "asciitilde"); (96, "exclamdown"); (97, "cent"); (98, "sterling");
   (99, "fraction"); (100, "yen"); (101, "florin"); (102, "section"); (103, "currency");
   (104, "quotesingle"); (105, "quotedblleft"); (106, "guillemotleft"); (107, "guilsinglleft");
   (108, "guilsinglright"); (109, "fi"); (110, "fl"); (111, "endash"); (112, "dagger");
   (113, "daggerdbl"); (114, "periodcentered"); (115, "paragraph"); (116, "bullet");
   (117, "quotesinglbase"); (118, "quotedblbase"); (119, "quotedblright");
   (120, "guillemotright"); (121, "ellipsis"); (122, "perthousand"); (123, "questiondown");
   (124, "grave"); (125, "acute"); (126, "circumflex"); (127, "tilde"); (128, "macron");
   (129, "breve"); (130, "dotaccent"); (131, "dieresis"); (132, "ring"); (133, "cedilla");
   (134, "hungarumlaut"); (135, "ogonek"); (136, "caron"); (137, "emdash"); (138, "AE");
   (139, "ordfeminine"); (140, "Lslash"); (141, "Oslash"); (142, "OE"); (143, "ordmasculine");
   (144, "ae"); (145, "dotlessi"); (146, "lslash"); (147, "oslash"); (148, "oe");
   (149, "germandbls"); (150, "onesuperior"); (151, "logicalnot"); (152, "mu");
   (153, "trademark"); (154, "Eth"); (155, "onehalf"); (156, "plusminus"); (157, "Thorn");
   (158, "onequarter"); (159, "divide"); (160, "brokenbar"); (161, "degree"); (162, "thorn");
   (163, "threequarters"); (164, "twosuperior"); (165, "registered"); (166, "minus");
   (167, "eth"); (168, "multiply"); (169, "threesuperior"); (170, "copyright");
   (171, "Aacute"); (172, "Acircumflex"); (173, "Adieresis"); (174, "Agrave"); (175, "Aring");
   (176, "Atilde"); (177, "Ccedilla"); (178, "Eacute"); (179, "Ecircumflex"); (180, "Edieresis");
   (181, "Egrave"); (182, "Iacute"); (183, "Icircumflex"); (184, "Idieresis"); (185, "Igrave");
   (186, "Ntilde"); (187, "Oacute"); (188, "Ocircumflex"); (189, "Odieresis"); (190, "Ograve");
   (191, "Otilde"); (192, "Scaron"); (193, "Uacute"); (194, "Ucircumflex"); (195, "Udieresis");
   (196, "Ugrave"); (197, "Yacute"); (198, "Ydieresis"); (199, "Zcaron"); (200, "aacute");
   (201, "acircumflex"); (202, "adieresis"); (203, "agrave"); (204, "aring"); (205, "atilde");
   (206, "ccedilla"); (207, "eacute"); (208, "ecircumflex"); (209, "edieresis"); (210, "egrave");
   (211, "iacute"); (212, "icircumflex"); (213, "idieresis"); (214, "igrave"); (215, "ntilde");
   (216, "oacute"); (217, "ocircumflex"); (218, "odieresis"); (219, "ograve"); (220, "otilde");
   (221, "scaron"); (222, "uacute"); (223, "ucircumflex"); (224, "udieresis"); (225, "ugrave");
   (226, "yacute"); (227, "ydieresis"); (228, "zcaron");
   (229, "exclamsmall"); (230, "Hungarumlautsmall"); (231, "dollaroldstyle"); (232, "dollarsuperior");
   (233, "ampersandsmall"); (234, "Acutesmall"); (235, "parenleftsuperior");
   (236, "parenrightsuperior"); (237, "twodotenleader"); (238, "onedotenleader");
   (239, "zerooldstyle"); (240, "oneoldstyle"); (241, "twooldstyle");
   (242, "threeoldstyle"); (243, "fouroldstyle"); (244, "fiveoldstyle"); (245, "sixoldstyle");
   (246, "sevenoldstyle"); (247, "eightoldstyle"); (248, "nineoldstyle"); (249, "commasuperior");
   (250, "threequartersemdash"); (251, "periodsuperior"); (252, "questionsmall");
   (253, "asuperior"); (254, "bsuperior"); (255, "centsuperior"); (256, "dsuperior");
   (257, "esuperior"); (258, "isuperior"); (259, "lsuperior"); (260, "msuperior"); (261, "nsuperior");
   (262, "osuperior"); (263, "rsuperior"); (264, "ssuperior"); (265, "tsuperior");
   (266, "ff"); (267, "ffi"); (268, "ffl"); (269, "parenleftinferior"); (270, "parenrightinferior");
   (271, "Circumflexsmall"); (272, "hyphensuperior"); (273, "Gravesmall"); (274, "Asmall");
   (275, "Bsmall"); (276, "Csmall"); (277, "Dsmall"); (278, "Esmall"); (279, "Fsmall"); (280, "Gsmall");
   (281, "Hsmall"); (282, "Ismall"); (283, "Jsmall"); (284, "Ksmall"); (285, "Lsmall"); (286, "Msmall");
   (287, "Nsmall"); (288, "Osmall"); (289, "Psmall"); (290, "Qsmall"); (291, "Rsmall"); (292, "Ssmall");
   (293, "Tsmall"); (294, "Usmall"); (295, "Vsmall"); (296, "Wsmall"); (297, "Xsmall"); (298, "Ysmall");
   (299, "Zsmall"); (300, "colonmonetary"); (301, "onefitted"); (302, "rupiah"); (303, "Tildesmall");
   (304, "exclamdownsmall"); (305, "centoldstyle"); (306, "Lslashsmall"); (307, "Scaronsmall");
   (308, "Zcaronsmall"); (309, "Dieresissmall"); (310, "Brevesmall"); (311, "Caronsmall");
   (312, "Dotaccentsmall"); (313, "Macronsmall"); (314, "figuredash"); (315, "hypheninferior");
   (316, "Ogoneksmall"); (317, "Ringsmall"); (318, "Cedillasmall"); (319, "questionsmalldown");
   (320, "oneeighth"); (321, "threeeighths"); (322, "fiveeighths"); (323, "seveneighths");
   (324, "onethird"); (325, "twothirds"); (326, "zerosuperior"); (327, "foursuperior");
   (328, "fivesuperior"); (329, "sixsuperior"); (330, "sevensuperior"); (331, "eightsuperior");
   (332, "ninesuperior"); (333, "zeroinferior"); (334, "oneinferior"); (335, "twoinferior");
   (336, "threeinferior"); (337, "fourinferior"); (338, "fiveinferior"); (339, "sixinferior");
   (340, "seveninferior"); (341, "eightinferior"); (342, "nineinferior"); (343, "centinferior");
   (344, "dollarinferior"); (345, "periodinferior"); (346, "commainferior"); (347, "Agravesmall");
   (348, "Aacutesmall"); (349, "Acircumflexsmall"); (350, "Atildesmall"); (351, "Adieresissmall");
   (352, "Aringsmall"); (353, "AEsmall"); (354, "Ccedillasmall"); (355, "Egravesmall");
   (356, "Eacutesmall"); (357, "Ecircumflexsmall"); (358, "Edieresissmall"); (359, "Igravesmall");
   (360, "Iacutesmall"); (361, "Icircumflexsmall"); (362, "Idieresissmall"); (363, "Ethsmall");
   (364, "Ntildesmall"); (365, "Ogravesmall"); (366, "Oacutesmall"); (367, "Ocircumflexsmall");
   (368, "Otildesmall"); (369, "Odieresissmall"); (370, "OEsmall"); (371, "Oslashsmall");
   (372, "Ugravesmall"); (373, "Uacutesmall"); (374, "Ucircumflexsmall"); (375, "Udieresissmall");
   (376, "Yacutesmall"); (377, "Thornsmall"); (378, "Ydieresissmall"); (379, "001.000");
   (380, "001.001"); (381, "001.002"); (382, "001.003"); (383, "Black"); (384, "Bold");
   (385, "Book"); (386, "Light"); (387, "Medium"); (388, "Regular"); (389, "Roman");
   (390, "Semibold")]

(* Read entries from the topdict, returning a list of the data therein. At the
moment, we ignore anything we're not interested in, substituting defaults as
appropriate, returning a list. *)
type topdict_entry =
  | CharStringType of int
  | CharSetOffset of int
  | EncodingOffset of int
  | CharStringOffset of int
  | PrivateDict of int * int (*i (offset, length) i*)
  | FontMatrix of Pdftransform.transform_matrix

let parse_topdict_entry = function
  | Operator2 6, [IntOp x] -> Some (CharStringType (i32toi x))
  | Operator 15, [IntOp x] -> Some (CharSetOffset (i32toi x))
  | Operator 16, [IntOp x] -> Some (EncodingOffset (i32toi x))
  | Operator 17, [IntOp x] -> Some (CharStringOffset (i32toi x))
  | Operator 18, [IntOp size; IntOp offset] -> Some (PrivateDict (i32toi offset, i32toi size))
  | Operator2 7, [a; b; c; d; e; f] ->
      let getnum = function
        | IntOp i -> Int32.to_float i
        | RealOp r -> r
        | _ -> Printf.eprintf "cff.ml: bad mat"; 0.
      in
        Some
          (FontMatrix
             {Pdftransform.a = getnum a; Pdftransform.b = getnum b;
              Pdftransform.c = getnum c; Pdftransform.d = getnum d;
              Pdftransform.e = getnum e; Pdftransform.f = getnum f})
  | _ -> None

let rec read_dict_sections prev ops =
  match
    let operands, more =
      cleavewhile (function Operator _ | Operator2 _ -> false | _ -> true) ops
    in
      match more with
      | Operator x::rest -> Some (Operator x, operands, rest)
      | Operator2 x::rest -> Some (Operator2 x, operands, rest)
      | _ -> None
  with
  | Some (Operator x, ops, more) ->
      read_dict_sections ((Operator x, ops)::prev) more
  | Some (Operator2 x, ops, more) ->
      read_dict_sections ((Operator2 x, ops)::prev) more
  | _ -> rev prev

let parse_topdict ops =
  option_map parse_topdict_entry (read_dict_sections [] ops)

let string_of_topdict_entry = function
  | CharStringType i -> "CharStringType is " ^ string_of_int i ^ "\n"
  | CharSetOffset i -> "CharSetOffset is " ^ string_of_int i ^ "\n"
  | EncodingOffset i -> "EncodingOffset is " ^ string_of_int i ^ "\n"
  | CharStringOffset i -> "CharStringOffset is " ^ string_of_int i ^ "\n"
  | PrivateDict (offset, size) -> "PrivateDict offset is " ^ string_of_int offset ^ " and size " ^ string_of_int size ^ "\n"
  | FontMatrix m -> "FontMatrix is " ^ Pdftransform.string_of_matrix m ^ "\n"

(* Same for private dictionary *)
type privatedict_entry =
  | Subrs of int

let parse_privatedict_entry = function
  | Operator 19, [IntOp offset] -> Some (Subrs (i32toi offset))
  | _ -> None

let parse_privatedict ops =
  option_map parse_privatedict_entry (read_dict_sections [] ops)

let string_of_privatedict_entry = function
  | Subrs offset -> "Subrs: offset is " ^ string_of_int offset ^ "\n"

let dbg = ref false

(* Read the encoding in b at offset off *)
let read_encoding b = function
  | 0 ->
     if !dbg then flprint "read_encoding: This font uses the standard encoding\n";
     standard_encoding, false
  | 1 ->
     if !dbg then flprint "read_encoding: This font uses the expert encoding\n";
     expert_encoding, false
  | off ->
      if !dbg then flprint "read_encoding: This font uses its own encoding\n";
      discard_bytes b off;
      match read_card8 b with
      | 0 ->
          if !dbg then flprint "read_encoding: Format type 0\n";
          let numglyphs = read_card8 b in
            if !dbg then Printf.printf "there are %i encoding entries\n" numglyphs;
            let codes = ref [] in
              for x = 1 to numglyphs do codes =| read_card8 b done;
              if !codes = []
                then [], true
                else combine (rev !codes) (ilist 1 numglyphs), true
      | 1 ->
          if !dbg then flprint "read_encoding: Format type 1\n";
          let numranges = read_card8 b in
            let encoding = ref []
            and n = ref 1 in
              for x = 1 to numranges do
                let first = read_card8 b in
                  let nLeft = read_card8 b in
                    encoding =@ rev (combine (ilist first (first + nLeft)) (ilist !n (!n + nLeft)));
                    n := !n + nLeft + 1
              done;
              rev !encoding, true
      | n -> Printf.eprintf "Unknown encoding format %i (CFF)" n; [], false

let read_charset b off numglyphs =
  assert (numglyphs >= 0);
  discard_bytes b off;
  let fmt = read_card8 b in
    match fmt with
    | 0 ->
       if !dbg then flprint "Format 0\n";
       let sids = ref []
       and glyphsleft = ref numglyphs in
         while !glyphsleft > 0 do
           sids =| read_card16 b;
           decr glyphsleft
         done;
         rev !sids
    | 1 ->
       if !dbg then flprint "Format 1\n";
       let sids = ref []
       and glyphsleft = ref numglyphs in
         while !glyphsleft > 0 do
           let first = read_card16 b in
             let left = read_card8 b in
               sids =| ilist first (first + left);
               glyphsleft -= (left + 1)
         done;
         take (flatten (rev !sids)) numglyphs
    | 2 ->
       if !dbg then flprint "format 2\n";
       let sids = ref []
       and glyphsleft = ref numglyphs in
         while !glyphsleft > 0 do
           let first = read_card16 b in
             let left = read_card16 b in
               sids =| ilist first (first + left);
               glyphsleft -= (left + 1)
         done;
         take (flatten (rev !sids)) numglyphs
    | _ -> flprint "bad charset format\n"; raise (Pdf.PDFError "CFF: Bad charset format")

let read_charstrings b charstringtype off =
  discard_bytes b off;
  map (read_string b) (read_index b)

type charstring_elt =
  | CSOperator of int
  | CSOperator2 of int
  | CSInt of int32
  | CSEndHere

let string_of_charstring_elt = function
  | CSOperator i -> Printf.sprintf "CSOperator: %i\n" i
  | CSOperator2 i -> Printf.sprintf "CSOperator2: %i\n" i
  | CSInt i -> Printf.sprintf "CSInt %li\n" i
  | CSEndHere -> "CSEndHere\n"

let string_of_charstring elts =
  fold_left ( ^ ) "" (map string_of_charstring_elt elts)

(* Parse a charstring from an input *)
let lex_charstring_elt hintbits i =
  let b = i.input_byte () in
    if b = Pdfio.no_more then raise End_of_file else
    if b < 11 then
      CSOperator b
    else if b = 12 then
      let b1 = i.input_byte () in
        CSOperator2 b1
    else if b <= 18 then
      CSOperator b
    else if b <= 20 then
      if hintbits = -1 then
        CSEndHere
      else
      begin
        (* For now, just ignore hint data *)
        for x = 1 to (hintbits + 7) / 8 do nudge i done;
        CSOperator b 
      end
    else if b <= 27 then
      CSOperator b
    else if b <= 28 then
      let b1 = i.input_byte () in
      let b2 = i.input_byte () in
        let num = ((b1 land 0b01111111) lsl 8) lor b2 in
          if (b1 land 0b10000000) <> 0
            then CSInt (i32ofi (num - 32768))
            else CSInt (i32ofi num)
    else if b <= 31 then
      CSOperator b
    else if b <= 246 then
      CSInt (i32ofi (b - 139))
    else if b <= 250 then
      let b1 = i.input_byte () in
        CSInt (i32ofi ((b - 247) * 256 + b1 + 108))
    else if b <= 254 then
      let b1 = i.input_byte () in
        CSInt (i32ofi ((~-((b - 251) * 256)) - b1 - 108))
    else (* 255 *)
      let ob1 = i32ofi (i.input_byte ()) in
      let ob2 = i32ofi (i.input_byte ()) in
      let ob3 = i32ofi (i.input_byte ()) in
      let ob4 = i32ofi (i.input_byte ()) in
        CSInt
          (lor32
            (lor32 (lsl32 ob1 24) (lsl32 ob2 16))
            (lor32 (lsl32 ob3 8) ob4))

(* Put elements on the stack. On a hint operator, count up what's on the stack,
and add one hintbit for each pair of operators. If another operator, clear
the stack. On moveto, we're done. *)
let count_hintbits i =
  let hintbits = ref 0
  and stk = ref []
  and hint_ops = ref [] in
    try
      while true do
        match lex_charstring_elt ~-1 i with
        | CSOperator ((19 | 1 | 3 | 18 | 23) as p) ->
            hint_ops =| p;
            hintbits += length !stk / 2;
            stk := []
        | CSEndHere when
            (mem 1 !hint_ops || mem 18 !hint_ops) &&
            (not (mem 3 !hint_ops)) && not (mem 23 !hint_ops)
          ->
            hintbits += length !stk / 2;
            raise End_of_file
        | CSOperator n | CSOperator2 n ->
            raise End_of_file
        | x ->
            stk =| x
      done;
      0
    with _ -> !hintbits

let lex_charstring s =
  let elts = ref []
  and i = input_of_string s in
    let hintbits = count_hintbits i in
    let i = input_of_string s in
      try
        while true do elts =| lex_charstring_elt hintbits i done;
        []
      with
        _ -> rev !elts

let mflprint x = () (*i flprint x i*)

(* Parse one or more paths into a [Pdfops.op list]*)
let rec parse_charstring_inner (cx, cy) stk prev thing =
  (*i Printf.printf "parse_charstring_inner: %f, %f\n" cx cy; i*)
  match thing with
  (* endchar *)
  | CSOperator 14::_ | [] -> mflprint "op: endchar\n";
     if prev = [] then [] else rev (Pdfops.Op_f::prev)
  (* rmoveto *)
  | CSOperator 21::t -> mflprint "op: rmoveto\n";
      begin match stk with
      | CSInt y::CSInt x::CSInt _::_
      | CSInt y::CSInt x::_ ->
          let cx = i32tof x +. cx
          and cy = i32tof y +. cy in
            parse_charstring_inner (cx, cy) [] (Pdfops.Op_m (cx, cy)::prev) t
      | _ -> Printf.eprintf "bad rmoveto"; raise (Pdf.PDFError "bad rmoveto")
      end
  | CSOperator 4::t -> mflprint "op: vmoveto\n";
      begin match stk with
      | CSInt y::CSInt _::_
      | CSInt y::_ ->
          let cy = i32tof y +. cy in
            parse_charstring_inner (cx, cy) [] (Pdfops.Op_m (cx, cy)::prev) t
      | _ -> Printf.eprintf "bad vmoveto"; raise (Pdf.PDFError "bad vmoveto")
      end
  | CSOperator 22::t -> mflprint "op: hmoveto\n";
      begin match stk with
      | CSInt x::CSInt _::_
      | CSInt x::_ ->
          let cx = i32tof x +. cx in
            parse_charstring_inner (cx, cy) [] (Pdfops.Op_m (cx, cy)::prev) t
      | _ -> Printf.eprintf "bad hmoveto"; raise (Pdf.PDFError "bad hmoveto")
      end
  (* vvcurveto *)
  | CSOperator 26::t -> mflprint "op: vvcurveto\n";
      let stk = ref (rev stk)
      and cx = ref cx
      and cy = ref cy
      and ops = ref [] in
        if length !stk mod 4 > 0 then
          begin match !stk with
          | CSInt dx1::CSInt dya::CSInt dxb::CSInt dyb::CSInt dyc::more ->
              let xa = i32tof dx1 +. !cx and ya = i32tof dya +. !cy in
              let xb = i32tof dxb +. xa and yb = i32tof dyb +. ya in
              let xc = i32tof dxb +. xa and yc = i32tof dyc +. yb in
                ops =| Pdfops.Op_c (xa, ya, xb, yb, xc, yc);
                cx := xc; cy := yc; stk := more
          | _ -> raise (Pdf.PDFError "CSOperator 26: bad operands")
          end;
        while !stk <> [] do
          match !stk with
          | CSInt dya::CSInt dxb::CSInt dyb::CSInt dyc::more ->
              let xa = !cx and ya = i32tof dya +. !cy in
              let xb = i32tof dxb +. xa and yb = i32tof dyb +. ya in
              let xc = i32tof dxb +. xa and yc = i32tof dyc +. yb in
                ops =| Pdfops.Op_c (xa, ya, xb, yb, xc, yc);
                cx := xc; cy := yc; stk := more
          | _ -> raise (Pdf.PDFError "CSOperator 26: bad operands")
        done;
        parse_charstring_inner (!cx, !cy) [] (!ops @ prev) t
  (* rlinecurve *)
  | CSOperator 25::t -> mflprint "op: rlinecurve\n";
      let stk = ref (rev stk)
      and cx = ref cx
      and cy = ref cy
      and ops = ref [] in
        (* rlinetos *)
        for x = 1 to (length !stk - 6) / 2 do
          match !stk with
          | CSInt dxa::CSInt dya::more ->
              stk := more;
              let cx' = !cx +. i32tof dxa and cy' = !cy +. i32tof dya in
                ops =| Pdfops.Op_l (cx', cy');
                cx := cx'; cy := cy'
          | _ ->
              raise (Pdf.PDFError "bad: rlinecurve / rlineto")
        done;
        (* rrcurvetos *)
        begin match !stk with
        | CSInt dxb::CSInt dyb::CSInt dxc::CSInt dyc::CSInt dxd::CSInt dyd::more ->
            let xb = i32tof dxb +. !cx and yb = i32tof dyb +. !cy in
            let xc = i32tof dxc +. xb and yc = i32tof dyc +. yb in
            let xd = i32tof dxd +. xc and yd = i32tof dyd +. yc in
              ops =| Pdfops.Op_c (xb, yb, xc, yc, xd, yd);
              cx := xd; cy := yd
        | _ -> raise (Pdf.PDFError "bad: rlinecurve")
        end;
        parse_charstring_inner (!cx, !cy) [] (!ops @ prev) t
  (* rcurveline *)
  | CSOperator 24::t -> mflprint "op: rcurveline";
      let stk = ref (rev stk)
      and cx = ref cx
      and cy = ref cy
      and ops = ref [] in
        (* rrcurvetos *)
        for x = 1 to (length !stk - 2) / 6 do
          match !stk with
          | CSInt dxb::CSInt dyb::CSInt dxc::CSInt dyc::CSInt dxd::CSInt dyd::more ->
              let xb = i32tof dxb +. !cx and yb = i32tof dyb +. !cy in
              let xc = i32tof dxc +. xb and yc = i32tof dyc +. yb in
              let xd = i32tof dxd +. xc and yd = i32tof dyd +. yc in
                ops =| Pdfops.Op_c (xb, yb, xc, yc, xd, yd);
                cx := xd; cy := yd;
                stk := more
          | _ -> raise (Pdf.PDFError "bad: rlinecurve")
        done;
        (* rlineto *)
        begin match !stk with
        | CSInt dxa::CSInt dya::more ->
            stk := more;
            let cx' = !cx +. i32tof dxa and cy' = !cy +. i32tof dya in
              ops =| Pdfops.Op_l (cx', cy');
              cx := cx'; cy := cy'
        | _ ->
            raise (Pdf.PDFError "bad: rlinecurve / rlineto")
        end;
        parse_charstring_inner (!cx, !cy) [] (!ops @ prev) t
  (* hhcurveto *)
  | CSOperator 27::t ->
      mflprint "op: hhcurveto\n";
      let ops = ref []
      and stk = ref (rev stk)
      and cx = ref cx
      and cy = ref cy in
        if odd (length !stk) then
          begin
            match !stk with
            | CSInt dy1::CSInt dxa::CSInt dxb::CSInt dyb::CSInt dxc::more ->
                let xa = i32tof dxa +. !cx and ya = i32tof dy1 +. !cy in
                let xb = i32tof dxb +. xa and yb = i32tof dyb +. ya in
                let xc = i32tof dxc +. xb and yc = yb in
                  stk := more;
                  ops =| Pdfops.Op_c (xa, ya, xb, yb, xc, yc);
                  cx := xc; cy := yc
            | _ -> raise (Pdf.PDFError "hhcurveto: operand problem")
          end;
        while !stk <> [] do
          match !stk with
          | CSInt dxa::CSInt dxb::CSInt dyb::CSInt dxc::more ->
              let xa = i32tof dxa +. !cx and ya = !cy in
              let xb = xa +. i32tof dxb and yb = ya +. i32tof dyb in
              let xc = xb +. i32tof dxc and yc = yb in
                stk := more;
                ops =| Pdfops.Op_c (xa, ya, xb, yb, xc, yc);
                cx := xc; cy := yc
          | _ -> raise (Pdf.PDFError "hhcurveto: operand problem")
        done;
        parse_charstring_inner (!cx, !cy) [] (!ops @ prev) t
  (* vhcurveto *)
  | CSOperator 30::t -> mflprint "vhcurveto:\n";
      let ops = ref []
      and stk = ref (rev stk)
      and cx = ref cx
      and cy = ref cy
      and starts_horz = ref false in
        (*i Printf.printf "At start of vhcurveto, cx, cy = %f, %f\n" !cx !cy;
         * i*)
        if length !stk mod 8 > 0 && (length !stk - 1) mod 8 > 0 then
          (* dx1 dx2 dy2 dy3 {dya dxb dyb dxc dxd dxe dye dyf}* dxf? *)
          begin match !stk with
          | [CSInt dy1; CSInt dx2; CSInt dy2; CSInt dx3; CSInt dyf] ->
              let xa = !cx and ya = !cy +. i32tof dy1 in
              let xb = xa +. i32tof dx2 and yb = ya +. i32tof dy2 in
              let xc = xb +. i32tof dx3 and yc = yb +. i32tof dyf in
                (*i flprint "vhcurveto-A\n"; i*)
                ops =| Pdfops.Op_c (xa, ya, xb, yb, xc, yc);
                stk := []; flip starts_horz; cx := xc; cy := yc
          | CSInt dy1::CSInt dx2::CSInt dy2::CSInt dx3::more ->
              let xa = !cx and ya = !cy +. i32tof dy1 in
              let xb = i32tof dx2 +. xa and yb = i32tof dy2 +. ya in
              let xc = xb +. i32tof dx3 and yc = yb in
                (*i flprint "vhcurveto-B\n";
                Printf.printf "a = %f, %f - b = %f, %f - c = %f, %f\n" xa ya xb
                yb xc yc; i*)
                ops =| Pdfops.Op_c (xa, ya, xb, yb, xc, yc);
                stk := more; flip starts_horz; cx := xc; cy := yc
          | _ -> raise (Pdf.PDFError "vhcurveto: bad operands")
          end;
        (* {dxa dxb dyb dyc dyd dxe dye dxf}+ dyf? vhcurveto *)
        let add_horz_start p q r s t =
          let xa = i32tof p +. !cx and ya = !cy in
          let xb = i32tof q +. xa and yb = i32tof r +. ya in
          let xc = i32tof t +. xb and yc = i32tof s +. yb in
            (*i Printf.printf "H %li %li %li %li %li\n" p q r s t;
            Printf.printf "xa, ya, xb, yb, xc, yc = %f, %f %f, %f %f, %f\n" xa
            ya xb yb xc yc; i*)
            ops =| Pdfops.Op_c (xa, ya, xb, yb, xc, yc);
            cx := xc; cy := yc;
            (*i Printf.printf "Point is %f, %f\n" !cx !cy; i*)
        and add_vert_start p q r s t =
          let xa = !cx and ya = i32tof p +. !cy in
          let xb = i32tof q +. xa and yb = i32tof r +. ya in
          let xc = i32tof s +. xb and yc = i32tof t +. yb in
            (*i Printf.printf "V %li %li %li %li %li\n" p q r s t;
            Printf.printf "xa, ya, xb, yb, xc, yc = %f, %f %f, %f %f, %f\n" xa
            ya xb yb xc yc; i*)
            ops =| Pdfops.Op_c (xa, ya, xb, yb, xc, yc);
            cx := xc; cy := yc;
            (*i Printf.printf "Point is %f, %f\n" !cx !cy; i*)
        in
        while !stk <> [] do
          match !stk with
          | [CSInt dxa; CSInt dxb; CSInt dyb; CSInt dyc;
             CSInt dyd; CSInt dxe; CSInt dye; CSInt dxf; CSInt dyf] ->
               (* Two curves, last one free ending *)
               if !starts_horz then
                 begin
                   add_horz_start dxa dxb dyb dyc 0l;
                   add_vert_start dyd dxe dye dxf dyf
                 end
               else
                 begin
                   add_vert_start dxa dxb dyb dyc 0l;
                   add_horz_start dyd dxe dye dxf dyf
                 end;
               stk := [];
          | CSInt dya::CSInt dxb::CSInt dyb::CSInt dyc::
            CSInt dyd::CSInt dxe::CSInt dye::CSInt dxf::more ->
               (* Two curves, entirely unfree *)
               if !starts_horz then
                 begin
                   add_horz_start dya dxb dyb dyc 0l;
                   add_vert_start dyd dxe dye dxf 0l
                 end
               else
                 begin
                   add_vert_start dya dxb dyb dyc 0l;
                   add_horz_start dyd dxe dye dxf 0l
                 end;
               stk := more;
          | _ -> raise (Pdf.PDFError "vhcurveto: bad operands")
        done;
        parse_charstring_inner (!cx, !cy) [] (!ops @ prev) t
  (* hvcurveto *)
  | CSOperator 31::t -> mflprint "hvcurveto\n";
      (*i Printf.printf "cx, cy = %f, %f\n" cx cy; i*)
      let ops = ref []
      and stk = ref (rev stk)
      and cx = ref cx
      and cy = ref cy
      and starts_horz = ref true in
        if length !stk mod 8 > 0 && (length !stk - 1) mod 8 > 0 then
          (* dx1 dx2 dy2 dy3 {dya dxb dyb dxc dxd dxe dye dyf}* dxf? *)
          begin match !stk with
          | [CSInt dx1; CSInt dx2; CSInt dy2; CSInt dy3; CSInt dxf] ->
              let xa = i32tof dx1 +. !cx and ya = !cy in
              let xb = i32tof dx2 +. xa and yb = i32tof dy2 +. ya in
              let xc = i32tof dxf +. xb and yc = i32tof dy3 +. yb in
                ops =| Pdfops.Op_c (xa, ya, xb, yb, xc, yc);
                stk := []; flip starts_horz; cx := xc; cy := yc
          | CSInt dx1::CSInt dx2::CSInt dy2::CSInt dy3::more ->
              let xa = i32tof dx1 +. !cx and ya = !cy in
              let xb = i32tof dx2 +. xa and yb = i32tof dy2 +. ya in
              let xc = xb and yc = i32tof dy3 +. yb in
                ops =| Pdfops.Op_c (xa, ya, xb, yb, xc, yc);
                stk := more; flip starts_horz; cx := xc; cy := yc
          | _ -> raise (Pdf.PDFError "hvcurveto: bad operands")
          end;
        (* {dxa dxb dyb dyc dyd dxe dye dxf}+ dyf? hvcurveto *)
        let add_horz_start p q r s t =
          let xa = i32tof p +. !cx and ya = !cy in
          let xb = i32tof q +. xa and yb = i32tof r +. ya in
          let xc = i32tof s +. xb and yc = i32tof t +. yb in
          (*i Printf.printf "add_horz_start: (%f, %f)\n(%f, %f)\n(%f,%f)\n" xa
           * ya xb yb xc yc; i*)
            ops =| Pdfops.Op_c (xa, ya, xb, yb, xc, yc);
            cx := xc; cy := yc;
        and add_vert_start p q r s t =
          let xa = !cx and ya = i32tof p +. !cy in
          let xb = i32tof q +. xa and yb = i32tof r +. ya in
          let xc = i32tof s +. xb and yc = i32tof t +. yb in
          (*i Printf.printf "add_vert_start: (%f, %f)\n(%f, %f)\n(%f,%f)\n" xa
           * ya xb yb xc yc; i*)
            ops =| Pdfops.Op_c (xa, ya, xb, yb, xc, yc);
            cx := xc; cy := yc;
        in
        while !stk <> [] do
          match !stk with
          | [CSInt dxa; CSInt dxb; CSInt dyb; CSInt dyc;
             CSInt dyd; CSInt dxe; CSInt dye; CSInt dxf; CSInt dyf] ->
               (* Two curves, last one free ending *)
               if !starts_horz then
                 begin
                   add_horz_start dxa dxb dyb 0l dyc;
                   add_vert_start dyd dxe dye dxf dyf
                 end
               else
                 begin
                   add_vert_start dxa dxb dyb dyc 0l;
                   add_horz_start dyd dxe dye dyf dxf
                 end;
               stk := []
          | CSInt dya::CSInt dxb::CSInt dyb::CSInt dyc::
            CSInt dyd::CSInt dxe::CSInt dye::CSInt dxf::more ->
               (* Two curves, entirely unfree *)
               if !starts_horz then
                 begin
                   add_horz_start dya dxb dyb 0l dyc;
                   add_vert_start dyd dxe dye dxf 0l
                 end
               else
                 begin
                   add_vert_start dya dxb dyb dyc 0l;
                   add_horz_start dyd dxe dye 0l dxf
                 end;
               stk := more
          | _ -> raise (Pdf.PDFError "hvcurveto: bad operands")
        done;
        parse_charstring_inner (!cx, !cy) [] (!ops @ prev) t
  (* rrcurveto *)
  | CSOperator 8::t -> mflprint "rrcurveto\n";
      let ops = ref []
      and stk = ref (rev stk)
      and cx = ref cx
      and cy = ref cy in
        while !stk <> [] do
          match !stk with
          | CSInt dxa::CSInt dya::CSInt dxb::CSInt dyb::CSInt dxc::CSInt dyc::more ->
              let xa = !cx +. i32tof dxa and ya = !cy +. i32tof dya in
              let xb = xa +. i32tof dxb and yb = ya +. i32tof dyb in
              let xc = xb +. i32tof dxc and yc = yb +. i32tof dyc in
                stk := more;
                ops =| Pdfops.Op_c (xa, ya, xb, yb, xc, yc);
                cx := xc; cy := yc
          | _ -> raise (Pdf.PDFError "rrcurveto: args not multiple of 6\n")
        done;
        parse_charstring_inner (!cx, !cy) [] (!ops @ prev) t
  (* vlineto *)
  | CSOperator 7::t -> mflprint "vlineto\n";
     let stk = ref (rev stk)
     and first_is_horz = ref false
     and ops = ref []
     and cx = ref cx
     and cy = ref cy in
       (* 1. If odd, do the first one *)
       if odd (length !stk) then
         begin
           begin match !stk with
           | CSInt dy1::more ->
               stk := more;
               ops =| Pdfops.Op_l (!cx, i32tof dy1 +. !cy);
               cy := i32tof dy1 +. !cy
           | _ -> raise (Pdf.PDFError "vlineto/odd fail")
           end;
           set first_is_horz
         end;
       (* 2. Do the rest *)
       while !stk <> [] do
         match !stk with
         | CSInt p::CSInt q::more ->
             stk := more;
             if !first_is_horz then
               begin
                 ops =| Pdfops.Op_l (i32tof p +. !cx, !cy);
                 cx := i32tof p +. !cx;
                 ops =| Pdfops.Op_l (!cx, i32tof q +. !cy);
                 cy := i32tof q +. !cy
               end
             else
               begin
                 ops =| Pdfops.Op_l (!cx, i32tof p +. !cy);
                 cy := i32tof p +. !cy;
                 ops =| Pdfops.Op_l (i32tof q +. !cx, !cy);
                 cx := i32tof q +. !cx
               end
         | _ -> raise (Pdf.PDFError "vlineto/odd fail")
       done;
       parse_charstring_inner (!cx, !cy) [] (!ops @ prev) t
  (* rlineto *)
  | CSOperator 5::t -> mflprint "rlineto\n";
      let stk = ref (rev stk)
      and ops = ref []
      and cx = ref cx
      and cy = ref cy in
        while !stk <> [] do
           match !stk with
           | CSInt dxa::CSInt dyb::more ->
               stk := more;
               ops =| Pdfops.Op_l (!cx +. i32tof dxa, !cy +. i32tof dyb);
               cx := !cx +. i32tof dxa;
               cy := !cy +. i32tof dyb
           | _ -> raise (Pdf.PDFError "rlineto")
        done;
        parse_charstring_inner (!cx, !cy) [] (!ops @ prev) t
  (* hlineto *)
  | CSOperator 6::t -> mflprint "hlineto\n";
      if even (length stk) then
        (* Even: alternating horizontal and vertical segments *)
        let ops = ref []
        and stk = ref (rev stk)
        and cx = ref cx
        and cy = ref cy in
          while !stk <> [] do
            match !stk with
            | CSInt dxa::CSInt dyb::more ->
                stk := more;
                ops =| Pdfops.Op_l (!cx +. i32tof dxa, !cy);
                cx := !cx +. i32tof dxa;
                ops =| Pdfops.Op_l (!cx, !cy +. i32tof dyb);
                cy := !cy +. i32tof dyb
            | _ -> Printf.eprintf "%i items on stack\n" (length !stk); assert false
          done;
          parse_charstring_inner (!cx, !cy) [] (!ops @ prev) t
      else
        (* Odd: Horizontal line of length dx1 then alternating vertical and
        horizontal segments. *)
        let ops, cx', cy' =
          match hd (rev stk) with
          | CSInt dx1 ->
              let ops = ref [Pdfops.Op_l (cx +. i32tof dx1, cy)]
              and stk = ref (tl (rev stk))
              and cx = ref (cx +. i32tof dx1)
              and cy = ref cy in
                while !stk <> [] do
                  match !stk with
                  | CSInt dya::CSInt dxb::more ->
                      stk := more;
                      ops =| Pdfops.Op_l (!cx, !cy +. i32tof dya);
                      cy := !cy +. i32tof dya;
                      ops =| Pdfops.Op_l (!cx +. i32tof dxb, !cy);
                      cx := !cx +. i32tof dxb
                  | _ -> assert false
                done;
                !ops, !cx, !cy
          | _ -> raise (Pdf.PDFError "bad csoperator 6")
        in
          parse_charstring_inner (cx', cy') [] (ops @ prev) t
  | CSOperator2 34::t ->
      mflprint "hflex\n";
      let ops = ref [] in
        begin match rev stk with
        | CSInt dx1::CSInt dx2::CSInt dy2::CSInt dx3::CSInt dx4::CSInt dx5::CSInt dx6::more ->
            let xa = cx +. i32tof dx1 and ya = cy in
            let xb = xa +. i32tof dx2 and yb = ya +. i32tof dy2 in
            let xc = xb +. i32tof dx3 and yc = yb in
              ops =| Pdfops.Op_c (xa, ya, xb, yb, xc, yc);
              let xa = xc +. i32tof dx4 and ya = yc in
              let xb = xa +. i32tof dx5 and yb = yc in
              let xc = xb +. i32tof dx6 and yc = yc in
                ops =| Pdfops.Op_c (xa, ya, xb, yb, xc, yc);
                parse_charstring_inner (xc, yc) [] (!ops @ prev) t
        | _ -> raise (Pdf.PDFError "hflex: bad arguments")
        end;
  | CSOperator2 35::t ->
      mflprint "flex";
      let ops = ref [] in
        begin match rev stk with
        | CSInt dx1::CSInt dy1::CSInt dx2::CSInt dy2::CSInt dx3::CSInt dy3::CSInt dx4::CSInt dy4::
          CSInt dx5::CSInt dy5::CSInt dx6::CSInt dy6::CSInt _::more ->
            let xa = cx +. i32tof dx1 and ya = cy +. i32tof dy1 in
            let xb = xa +. i32tof dx2 and yb = ya +. i32tof dy2 in
            let xc = xb +. i32tof dx3 and yc = yb +. i32tof dy3 in
              ops =| Pdfops.Op_c (xa, ya, xb, yb, xc, yc);
              let xa = xc +. i32tof dx4 and ya = yc +. i32tof dy4 in
              let xb = xa +. i32tof dx5 and yb = ya +. i32tof dy5 in
              let xc = xb +. i32tof dx6 and yc = yb +. i32tof dy6 in
               ops =| Pdfops.Op_c (xa, ya, xb, yb, xc, yc);
               parse_charstring_inner (xc, yc) [] (!ops @ prev) t
        | _ -> raise (Pdf.PDFError "flex: bad arguments.")
        end
  | CSOperator2 36::t ->
      mflprint "hflex1\n";
      (**i Printf.printf "cx = %f, cy = %f\n" cx cy; i*)
      let ops = ref [] in
        begin match rev stk with
        | CSInt dx1::CSInt dy1::CSInt dx2::CSInt dy2::CSInt dx3::CSInt dx4::CSInt dx5::CSInt dy5::CSInt dx6::more ->
            let xa = cx +. i32tof dx1 and ya = cy +. i32tof dy1 in
            let xb = xa +. i32tof dx2 and yb = ya +. i32tof dy2 in
            let xc = xb +. i32tof dx3 and yc = yb in
              (*i Printf.printf "Curve one: %f %f %f %f %f %f\n" xa ya xb yb xc
               * yc; i*)
              ops =| Pdfops.Op_c (xa, ya, xb, yb, xc, yc);
              let xa = xc +. i32tof dx4 and ya = yc in
              let xb = xa +. i32tof dx5 and yb = ya +. i32tof dy5 in
              let xc = xb +. i32tof dx6 and yc = yb in
                ops =| Pdfops.Op_c (xa, ya, xb, yb, xc, yc);
                (*i Printf.printf "Curve two: %f %f %f %f %f %f\n" xa ya xb yb
                 * xc yc; i*)
                parse_charstring_inner (xc, yc) [] (!ops @ prev) t
        | _ -> raise (Pdf.PDFError "hflex1: bad arguments")
        end
  | CSOperator2 37::t ->
      mflprint "flex1\n";
      let ops = ref [] in
        begin match rev stk with
        | CSInt dx1::CSInt dy1::CSInt dx2::CSInt dy2::CSInt dx3::
          CSInt dy3::CSInt dx4::CSInt dy4::CSInt dx5::CSInt dy5::CSInt d6::more ->
            let xa = cx +. i32tof dx1 and ya = cy +. i32tof dy1 in
            let xb = xa +. i32tof dx2 and yb = ya +. i32tof dy2 in
            let xc = xb +. i32tof dx3 and yc = yb +. i32tof dy3 in
              ops =| Pdfops.Op_c (xa, ya, xb, yb, xc, yc);
              let newdx, newdy =
                let dx =
                  i32tof dx1 +. i32tof dx2 +. i32tof dx3 +. i32tof dx4 +. i32tof dx5
                and dy =
                  i32tof dy1 +. i32tof dy2 +. i32tof dy3 +. i32tof dy4 +. i32tof dy5
                in
                  if fabs dx > fabs dy then d6, 0l else 0l, d6
              in
                let xa = xc +. i32tof dx4 and ya = yc +. i32tof dy4 in
                let xb = xa +. i32tof dx5 and yb = ya +. i32tof dy5 in
                let xc = xb +. i32tof newdx and yc = yb +. i32tof newdy in
                  ops =| Pdfops.Op_c (xa, ya, xb, yb, xc, yc);
                  parse_charstring_inner (xc, yc) [] (!ops @ prev) t
        | _ -> raise (Pdf.PDFError "flex1: bad arguments")
        end
  (* hintmask / cntrmask *)
  | CSOperator (19 | 20)::t ->
      parse_charstring_inner (cx, cy) stk prev t
  (* hint building *)
  | CSOperator (1 | 3 | 18 | 23)::t ->
      parse_charstring_inner (cx, cy) [] prev t
  (* Obsolete: dotsection *)
  | CSOperator2 0::t ->
      parse_charstring_inner (cx, cy) stk prev t
  (* anything else: stick it on the stack *)
  | (CSOperator n | CSOperator2 n)::t ->
      Printf.eprintf "CFF: operator %i not understood\n" n;
      parse_charstring_inner (cx, cy) [] prev t
  | h::t -> parse_charstring_inner (cx, cy) (h::stk) prev t

let parse_charstring lexemes =
  try parse_charstring_inner (0., 0.) [] [] lexemes with
    e -> (*i Printf.eprintf (Printexc.to_string e); i*) raise e

(* Given a op list, debug to letter.pdf *)
let debug_letter ops =
  let page = Pdfpage.blankpage Pdfpaper.a4 in
    let page =
     {page with
       Pdfpage.content =
         let trop =
           Pdfops.Op_cm
             (Pdftransform.matrix_of_op (Pdftransform.Scale ((0., 0.), 0.1, 0.1)))
         in
           [Pdfops.stream_of_ops (trop::ops)]}
    in
      let pdf, pageroot = Pdfpage.add_pagetree [page] (Pdf.empty ()) in
        let pdf = Pdfpage.add_root pageroot [] pdf in
          Pdfwrite.pdf_to_file pdf "letter.pdf"

(* The undefined glyph *)
let notdef =
  {Pdfgraphics.elements = [];
   Pdfgraphics.fonts = [];
   Pdfgraphics.resources = Pdf.Dictionary []}

let parse_cff_font data =
  let get_bitstream () =
    bitbytes_of_input (input_of_bytes data)
  in
    let b = get_bitstream () in
      (* Read the header *)
      let _ = read_card8 b in (* MAJOR *)
        let _ = read_card8 b in (* MINOR *)
          let header_size = read_card8 b in
            let _ = read_card8 b * 4 in (* OFFSIZE*)
              (* Wind forward past the possible rest of the header *)
              discard_bytes b (header_size - 4);
              (* Read the name INDEX *)
              if !dbg then Printf.printf "Reading the name INDEX\n";
              let fontnames = map (read_string b) (read_index b) in
                if !dbg then
                  begin
                    Printf.printf "There are %i fonts in this CFF:\n" (length fontnames);
                    iter (Printf.printf "%s\n") fontnames;
                  end;
                  (* Read the Top DICT INDEX *)
                  let topdicts = map (read_dict [] b) (read_index b) in
                    (* Read the STRING INDEX. - strings for SIDs *)
                    let strings =
                      let stringnames = map (read_string b) (read_index b)
                      and fst = length sids in
                        if stringnames = []
                          then []
                          else combine (ilist fst (fst + length stringnames - 1)) stringnames
                   in
                     if !dbg then
                       begin
                         Printf.printf "have read %i non-standard strings\n" (length strings);
                         iter (function (sid, str) -> Printf.printf "SID, string = %i, %s \n" sid str) strings
                       end;
                     let parsed = parse_topdict (hd topdicts) in
                       if !dbg then flprint "Have read the first font's topdict\n";
         (* A. Read encoding (codes to glpyh codes) *)
         let encodingoffset =
           match keep (function EncodingOffset _ -> true | _ -> false) parsed with
           | [EncodingOffset i] ->
             if !dbg then Printf.printf "An Encoding was found at EncodingOffset %i\n" i; i
           | [] -> if !dbg then Printf.printf "This fonts uses the standard encoding"; 0(* Standard encoding *)
           | _ -> raise (Pdf.PDFError "no encoding offset")
         in
           let encoding, encoding_was_custom =
             read_encoding (get_bitstream ()) encodingoffset
           in
             if !dbg then
               begin
                 Printf.printf "Read %i encoding entries\n" (length encoding);
                 iter (function (x, y) -> Printf.printf "%i -> %i\n" x y) encoding;
                 flprint "\n"
               end;
         (* C. Read char strings *)
         let charstringtype =
           match keep (function CharStringType _ -> true | _ -> false) parsed with
           | [CharStringType i] -> i
           | _ -> 2
         and charstringoffset =
           match keep (function CharStringOffset _ -> true | _ -> false) parsed with
           | [CharStringOffset i] -> i
           | _ -> raise (Pdf.PDFError "no encoding offset")
         in
           if !dbg then Printf.printf "Charstring format is %i\n" charstringtype;
           let charstrings =
             read_charstrings (get_bitstream ()) charstringtype charstringoffset
           in
             if !dbg then
               begin
                 Printf.printf "Read %i glyph descriptions\n" (length charstrings);
                 Printf.printf "numglyphs = %i\n" (length charstrings - 1);
               end;
         (* B. Read charset (glpyh codes to glyph names) *)
         let charsetoffset =
           match keep (function CharSetOffset _ -> true | _ -> false) parsed with
           | [CharSetOffset i] -> i
           | _ -> 0 (* No charset offset = ISOAdobe = 0 *)
         in
           (*Printf.printf "Charset offset is %i\n" charsetoffset;*)
           let fontmatrix =
             match keep (function FontMatrix _ -> true | _ -> false) parsed with
             | [FontMatrix m] -> m
             | _ -> Pdftransform.matrix_of_op (Pdftransform.Scale ((0., 0.), 0.001, 0.001))
           in
           let charset =
             match charsetoffset with
             | 0 | 1 | 2 -> if !dbg then flprint "***BUILTIN charset"; map fst (sids @ strings)
             | o -> if !dbg then flprint "***READING charset manually"; read_charset (get_bitstream ()) o (length charstrings - 1)
           in
             let gids =
               match charsetoffset with 0 | 1 | 2 -> indx0 charset | _ -> indx charset
             in
             let allids = sids @ strings in
               let names = map (function c -> lookup_failnull c allids) charset in
             if !dbg then
               begin
                 Printf.printf "Read %i (gid, charset SID, glyphname) triples:" (length charset);
                 iter3
                   (fun gid charset glyphname -> Printf.printf "%i -> %i -> %s, " gid charset glyphname)
                   gids
                   charset
                   names
              end;
             let lexed_charstrings =
               map
                 (fun (_, c) -> lex_charstring c)
                 (combine (ilist 1 (length charstrings)) charstrings)
             in
               if !dbg then
                 begin
                   Printf.printf "lexed %i charstrings\n" (length lexed_charstrings);
                   iter2
                     (fun i s -> Printf.printf "GID %i -> a charstring, " i)
                     (indx lexed_charstrings)
                     (map string_of_charstring lexed_charstrings);
                 end;
               let parsed_charstrings =
                 map
                   (fun (_, c) -> parse_charstring c)
                   (combine (ilist 1 (length lexed_charstrings)) lexed_charstrings)
               in
               if !dbg then flprint "Parsed all charstrings..\n";
                 let paths = map Pdfgraphics.graphic_of_ops parsed_charstrings in
                   if !dbg then flprint "Built graphic paths from parsed_charstrings\n";
                   if !dbg then Printf.printf "WE HAVE %i names and %i paths\n" (length names) (length paths);
                   (* Now can build paths from names *)
                   (* To do this, for each thing in "names" with .notdef added, at same position in charset,
                   at same position in parsed_charstrings. *)
                   let path_name_pairs =
                     let charset_gid_pairs = combine charset gids in
                     map2
                       (fun name charsetchar ->
                          name,
                          (if !dbg then Printf.printf "Starting with name %s -> charset SID %i -> " name charsetchar;
                              match lookup charsetchar charset_gid_pairs with
                              | None -> 
                                  (if !dbg then flprint "not defined - not in charset\n";
                                    {Pdfgraphics.elements = [];
                                     Pdfgraphics.fonts = [];
                                     Pdfgraphics.resources = Pdf.Dictionary []})
                              | Some gid ->
                                   if gid + 1 <= length paths && gid + 1 > 0 then
                                      (if !dbg then Printf.printf "converted to gid %i\n" gid; select (gid + 1) paths)
                                   else
                                     (if !dbg then flprint "not defined - gid found but no charstring defined\n";
                                       {Pdfgraphics.elements = [];
                                        Pdfgraphics.fonts = [];
                                        Pdfgraphics.resources = Pdf.Dictionary []})))
                       names
                       charset
                   in
                       if !dbg then flprint "Total:\n";
                       let differences =
                         let gidmap = combine gids charset
                         and charsetmap = combine charset names in
                           option_map
                             (fun incode ->
                                if !dbg then Printf.printf "incode %i -> " incode;
                                match lookup incode encoding with
                                | Some code ->
                                    if !dbg then Printf.printf "encoded %i -> " code;
                                    if encoding_was_custom then
                                      begin match lookup code gidmap with
                                      | Some gid ->
                                        begin
                                          if !dbg then Printf.printf "sid %i -> " code;
                                          begin match lookup gid charsetmap with
                                          | Some glyphname ->
                                              if !dbg then Printf.printf "glyphname %s\n" glyphname;
                                              Some (incode, glyphname)
                                          | None ->
                                              if !dbg then flprint "\n";
                                              None
                                          end
                                        end
                                      | None -> flprint "\n"; None
                                      end
                                    else
                                      begin
                                        if !dbg then Printf.printf "sid %i -> " code;
                                        begin match lookup code charsetmap with
                                        | Some glyphname ->
                                            if !dbg then Printf.printf "glyphname %s\n" glyphname;
                                            Some (incode, glyphname)
                                        | None ->
                                            if !dbg then flprint "\n";
                                            None
                                        end
                                      end
                                | None -> if !dbg then Printf.printf "incode not found in encoding \n"; None
                                )
                             (ilist 0 255)
                       in
                         path_name_pairs, differences, fontmatrix

(* Make a CharProc stream from a graphic. We need to filter out ops we don't
want. *)
let charprocbytes_of_graphic g matrix =
  Pdfops.stream_of_ops
    (Pdfops.Op_d1 (0., 0., 0., 0., 0., 0.)::
      lose
        (function
         | Pdfops.Op_q
         | Pdfops.Op_Q
         | Pdfops.Op_cm _
         | Pdfops.Op_w _
         | Pdfops.Op_J _
         | Pdfops.Op_j _
         | Pdfops.Op_M _
         | Pdfops.Op_ri _
         | Pdfops.Op_CS _
         | Pdfops.Op_SCN _ 
         | Pdfops.Op_cs _
         | Pdfops.Op_scn _ -> true
         | _ -> false)
        (Pdfgraphics.ops_of_simple_graphic (Pdfgraphics.transform_graphic matrix g)))

(* Calculate the maximal bounding box of some graphics *)
let bbox_of_graphics gs =
  fold_left
    box_union_float
    (max_float, min_float, max_float, min_float)
    (map Pdfgraphics.bbox_of_graphic gs)

(* Convert a  Pdftext font in Type 1 format to a Pdftext font in type3 font.
This involves calculating FontBBox, FontMatrix and CharProcs entries, and
removing certain entries from the FontDescriptor *)
let to_type3 pdf = function
  | Pdftext.SimpleFont
      ({Pdftext.fonttype = Pdftext.Type1;
        Pdftext.encoding = original_encoding;
        Pdftext.fontdescriptor =
          Some ({Pdftext.fontfile = Some Pdftext.FontFile3 fontfileobj} as fontdescriptor)} as fontrec) ->
        begin try
          flprint "***inside Pdfcff.to_type3\n";
          let parsed_cffdata, differences, fontmatrix =
            let str = Pdf.direct pdf (Pdf.Indirect fontfileobj) in
              Pdfcodec.decode_pdfstream pdf str;
              match str with
              | Pdf.Stream {contents = (_, Pdf.Got data)} ->
                  begin try parse_cff_font data with
                    e -> Printf.printf "Error %s" (Printexc.to_string e); raise (Pdf.PDFError "CFF Parse failure")
                  end
              | _ -> raise (Pdf.PDFError "CFF data not a stream")
          in
            Printf.printf "parsed_cffdata returned %i things\n" (length parsed_cffdata);
            let fontdescriptor = fontdescriptor
            and charprocs =
              let scalematrix =
                Pdftransform.matrix_compose
                  fontmatrix
                  (Pdftransform.matrix_of_op (Pdftransform.Scale ((0., 0.), 1000., 1000.)))
              in
                map
                  (function (n, g) ->
                     "/" ^ n, Pdf.Indirect (Pdf.addobj pdf (charprocbytes_of_graphic g scalematrix)))
                     parsed_cffdata
            and fontmatrix =
              Pdftransform.matrix_of_op (Pdftransform.Scale ((0., 0.), 0.001, 0.001))
            and fontbbox =
              bbox_of_graphics (snd (split parsed_cffdata))
            and encoding =
              let existing_differences =
                match original_encoding with
                | Pdftext.CustomEncoding (_, diffs) -> map (fun (x, y) -> y, x) diffs
                | _ -> []
              and new_differences =
                map (fun (x, y) -> x, "/" ^ y) differences
              in
                (* Prefer existing differences over the internal ones we generated *)
                let final_differences =
                  fold_left (fun acc (code, name) -> add code name acc) new_differences existing_differences
                in
                  Pdftext.CustomEncoding (Pdftext.ImplicitInFontFile, map (function (x, y) -> y, x) final_differences)
            in
             flprint "***All done, CFF font produced\n";
             Pdftext.SimpleFont
               {fontrec with
                  Pdftext.fonttype = Pdftext.Type3
                  {Pdftext.fontbbox = fontbbox;
                   Pdftext.fontmatrix = fontmatrix;
                   Pdftext.charprocs = charprocs;
                   Pdftext.type3_resources = Pdf.Dictionary []};
                  Pdftext.encoding = encoding;
                  Pdftext.fontdescriptor = Some fontdescriptor}
        with e ->
          Printf.eprintf "Failed to read CFF font with error %s\n" (Printexc.to_string e);
          raise e
        end
  | _ ->
     raise (Pdf.PDFError "Pdfcff.to_type3: This is not a type 1 CFF font")

