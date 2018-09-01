
let debugfmt =
  let dev_null = if Sys.os_type = "Win32" then "NUL" else "/dev/null" in
  Format.formatter_of_out_channel (open_out dev_null)

let fmtgen  = debugfmt
let fmtGSUB = debugfmt
let fmtMATH = debugfmt
(*
let fmtCFF  = Format.std_formatter
*)

module Alist : sig
  type 'a t
  val empty : 'a t
  val extend : 'a t -> 'a -> 'a t
  val append : 'a t -> 'a list -> 'a t
  val to_list : 'a t -> 'a list
end = struct

  type 'a t = 'a list

  let empty = []

  let extend acc x =
    x :: acc

  let append acc lst =
    List.rev_append lst acc

  let to_list acc =
    List.rev acc

end


(* Error strings *)

let err_invalid_tag s = Printf.sprintf "invalid OpenType tag (%S)" s

(* Unsafe string byte manipulations.

   If you don't believe the author's invariants, replacing with safe
   versions makes everything safe in the module. He won't be
   upset. *)

let unsafe_chr = Char.unsafe_chr
let unsafe_byte s j = Char.code (String.unsafe_get s j)

(* Pretty printers *)

let pp = Format.fprintf
let rec pp_list ?(pp_sep = Format.pp_print_cut) pp_v ppf = function
| [] -> ()
| v :: vs ->
    pp_v ppf v; if vs <> [] then (pp_sep ppf (); pp_list ~pp_sep pp_v ppf vs)


let ( >>= ) x f = match x with Ok(v) -> f v | Error(_) as e -> e
let return x                 = Ok(x)
let err e                    = Error(e)


type byte = char


module WideInt
= struct
    type t = Int64.t
    let ( lsl ) = Int64.shift_left
    let ( lsr ) = Int64.shift_right
    let ( lor ) = Int64.logor
    let ( land ) = Int64.logand
    let ( mod ) = Int64.rem
    let add = Int64.add
    let sub = Int64.sub
    let of_int = Int64.of_int
    let to_int = Int64.to_int
    let of_int64 iw = iw
    let to_int64 iw = iw
    let of_byte ch = Int64.of_int (Char.code ch)
    let to_byte iw = Char.chr (Int64.to_int iw)  (* -- may raise 'Invalid_argument' -- *)
    let is_in_uint32 iw = Int64.zero <= iw && iw < 0x100000000L
    let is_in_int32 iw = -0x80000000L <= iw && iw < 0x80000000L
    let is_in_int64 iw = Int64.min_int <= iw && iw <= Int64.max_int
    let is_neg iw = iw < Int64.zero
    let pp fmt iw = Format.fprintf fmt "%LX" iw
  end


type wint = WideInt.t


let ( +% ) = WideInt.add
let ( -% ) = WideInt.sub
let ( !% ) = WideInt.of_int
let ( !%% ) = WideInt.of_int64


let cut_uint32_unsafe (ui : wint) : byte * byte * byte * byte =
  let open WideInt in
    let b0 = ui lsr 24 in
    let r0 = ui -% (b0 lsl 24) in
    let b1 = r0 lsr 16 in
    let r1 = r0 -% (b1 lsl 16) in
    let b2 = r1 lsr 8 in
    let b3 = r1 -% (b2 lsl 8) in
(*
    Printf.printf "uint32 %d --> (%d, %d, %d, %d)\n" ui b0 b1 b2 b3;
*)
    (to_byte b0, to_byte b1, to_byte b2, to_byte b3)
