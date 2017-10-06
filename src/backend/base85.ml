
let print_for_debug msg =
  ()

open Unsigned


let get_byte s i = Char.code (String.get s i)

let get_uint32 (s : string) (i : int) : uint32 =
  let b0 = get_byte s i in
  let b1 = get_byte s (i + 1) in
  let b2 = get_byte s (i + 2) in
  let b3 = get_byte s (i + 3) in
    let s0 = UInt32.of_int ((b0 lsl 8) lor b1) in
    let s1 = UInt32.of_int ((b2 lsl 8) lor b3) in
    let ans = UInt32.logor (UInt32.shift_left s0 16) s1 in
      let () = print_for_debug (Printf.sprintf "Q(%d %d %d %d)\n" b0 b1 b2 b3) in (* for debug *)
      let () = print_for_debug (string_of_int (UInt32.to_int s0)) in (* for debug *)
      let () = print_for_debug (string_of_int (UInt32.to_int s1)) in (* for debug *)
      let () = print_for_debug ("2 * " ^ (string_of_int (UInt32.to_int (UInt32.div ans (UInt32.of_int 2))))) in (* for debug *)
        ans


let decompose_85ary_5 n =
  let q4 = UInt32.of_int (85 * 85 * 85 * 85) in
  let q3 = UInt32.of_int (85 * 85 * 85) in
  let q2 = UInt32.of_int (85 * 85) in
  let q1 = UInt32.of_int 85 in
  let (x0, r0) = (UInt32.div n q4, UInt32.rem n q4) in
  let (x1, r1) = (UInt32.div r0 q3, UInt32.rem r0 q3) in
  let (x2, r2) = (UInt32.div r1 q2, UInt32.rem r1 q2) in
  let (x3, x4) = (UInt32.div r2 q1, UInt32.rem r2 q1) in
  let (y0, y1, y2, y3, y4) = (UInt32.to_int x0, UInt32.to_int x1, UInt32.to_int x2, UInt32.to_int x3, UInt32.to_int x4) in
    let () = print_for_debug (Printf.sprintf "(%d %d %d %d %d)\n" y0 y1 y2 y3 y4) in (* for debug *)
      (y0, y1, y2, y3, y4)


let convert_to_base85_string (x0, x1, x2, x3, x4) =
  let single x =
    let () = print_for_debug ("C " ^ (string_of_int ((x + (Char.code '!'))))) in (* for debug *)
      Char.chr (x + (Char.code '!'))
  in
  let tup = (single x0, single x1, single x2, single x3, single x4) in
    match tup with
    | ('!', '!', '!', '!', '!') -> "z"
    | (c0, c1, c2, c3, c4)      -> Printf.sprintf "%c%c%c%c%c" c0 c1 c2 c3 c4


let encode (str : string) =
  let len = String.length str in
  let chop = (4 - (len mod 4)) mod 4 in
  let len4s = len + chop in
  let rec aux (i : int) (acc : string list) =
    if i >= len4s then acc else
      let n = get_uint32 str i in
      let strenc = convert_to_base85_string (decompose_85ary_5 n) in
        aux (i + 4) (strenc :: acc) 
  in
  let acc = aux 0 [] in
  let strenc = String.concat "" (List.rev acc) in
  let lenenc = String.length strenc in
    (String.sub strenc 0 (lenenc - chop)) ^ "~>"
