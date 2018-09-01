(* General Input and Output *)
open Pdfutil

(* External type for big streams of bytes passed to C*)
type rawbytes =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(* But for speed, we use strings of length < Sys.max_string_length *)
type bytes =
  | Long of rawbytes
  | Short of string

let bigarray_unsafe_get =
  Bigarray.Array1.unsafe_get

let bigarray_unsafe_set =
  Bigarray.Array1.unsafe_set

(* Extract the raw bytes, without necessarily copying *)
let raw_of_bytes = function
  | Short b ->
      let l = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (String.length b) in
        for x = 0 to String.length b - 1 do
          bigarray_unsafe_set l x (int_of_char (String.unsafe_get b x))
        done;
        l
  | Long b -> b

(* Build a bytes from raw data, without copying *)
let bytes_of_raw b = Long b

(* Make a stream of a given size. *)
let mkbytes l =
  if l <= Sys.max_string_length
    then Short (String.create l)
    else Long (Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout l)

(* Find the size of a stream. *)
let bytes_size = function
  | Short s -> String.length s
  | Long b -> Bigarray.Array1.dim b

let bset s n v =
  match s with
  | Short s -> s.[n] <- Char.unsafe_chr v
  | Long s -> Bigarray.Array1.set s n v

let bset_unsafe s n v =
  match s with
  | Short s -> String.unsafe_set s n (Char.unsafe_chr v)
  | Long s -> bigarray_unsafe_set s n v

let bget s n =
  match s with
  | Short s -> int_of_char (s.[n])
  | Long s -> Bigarray.Array1.get s n

let bget_unsafe s n =
  match s with
  | Short s -> int_of_char (String.unsafe_get s n)
  | Long s -> bigarray_unsafe_get s n

(* For lexing / parsing byte streams, keep the position. Starts at zero. *)
type stream =
  {mutable pos : int;
   data : bytes ref}

(* Fill a stream with a value. *)
let fillbytes v s =
  for x = 0 to bytes_size s - 1 do bset_unsafe s x v done

let print_bytes s =
  if bytes_size s > 0 then
    for x = 0 to bytes_size s - 1 do
      Printf.printf "%i " (bget s x)
    done

(* Make a bytes from a string, with no terminator. *)
let bytes_of_string s =
  let l = String.length s in
    let stream = mkbytes l in
      if l > 0 then
        for k = 0 to l - 1 do
          bset_unsafe stream k (int_of_char (String.unsafe_get s k))
        done;
      stream

(* Make a byte stream from an integer list. *)
let bytes_of_list l =
  let length = length l in
    if length = 0 then mkbytes 0 else
      let s = mkbytes length in let l = ref l in
        for pos = 0 to length - 1 do
          bset_unsafe s pos (hd !l);
          l := tl !l
        done;
        s

(* Convert a character list to a stream. *)
let bytes_of_charlist cs =
  let length = length cs in
    if length = 0 then mkbytes 0 else
      let s = mkbytes length in let cs = ref cs in
        for pos = 0 to length - 1 do
          bset_unsafe s pos (int_of_char (hd !cs));
          cs := tl !cs
        done;
        s

let bytes_of_arraylist l =
  let totalsize = fold_left ( + ) 0 (map Array.length l) in
    let output = mkbytes totalsize
    in let pos = ref 0 in
      iter
        (fun a ->
           for x = 0 to Array.length a - 1 do
             bset_unsafe output !pos (Array.unsafe_get a x); incr pos
           done)
        l;
      output

let string_of_bytes s =
  let l = bytes_size s in
    let buf = Buffer.create l in
      for x = 0 to l - 1 do
        Buffer.add_char buf (Char.unsafe_chr (bget_unsafe s x))
      done;
      Buffer.contents buf

let bytes_of_int_array a =
  let s = mkbytes (Array.length a) in
    for i = 0 to bytes_size s - 1 do
      bset_unsafe s i (Array.unsafe_get a i)
    done;
    s

let int_array_of_bytes s =
  let a = Array.make (bytes_size s) 0 in
    for i = 0 to Array.length a - 1 do
      Array.unsafe_set a i (bget_unsafe s i)
    done;
    a

(* Copy a stream. *)
let copybytes s =
  let l = bytes_size s in
    let s' = mkbytes l in
      if l > 0 then
        for k = 0 to l - 1 do
          bset_unsafe s' k (bget_unsafe s k)
        done;
      s'

let int_array_of_string s =
  Array.init (String.length s) (fun i -> int_of_char (String.unsafe_get s i))

let string_of_int_arrays arrays =
  let len = fold_left ( + ) 0 (map Array.length arrays) in
    let buf = Buffer.create len in
      iter (Array.iter (fun v -> Buffer.add_char buf (Char.unsafe_chr v))) arrays;
      Buffer.contents buf

let string_of_int_array a =
  string_of_int_arrays [a]

let bytes_selfmap f s =
  let l = bytes_size s in
    for k = 0 to l - 1 do
      bset_unsafe s k (f (bget_unsafe s k))
    done

let charlist_of_bytes s =
  let l = ref [] in
    for x = bytes_size s - 1 downto 0 do
      l =| Char.unsafe_chr (bget_unsafe s x)
    done;
    !l

(* Position in a file *)
type pos = int

(* End_of_file without exceptions or Some / None *)
let no_more = -1

(* A general type for input functions. This allows paramaterization over
channels, strings, bigarrays etc. *)
type input =
  {pos_in : unit -> pos;
   seek_in : pos -> unit;
   input_char : unit -> char option;
   input_byte : unit -> int;
   in_channel_length : pos;
   set_offset : pos -> unit;
   caml_channel : in_channel option;
   source : string}

(* A general type for output functions, allowing parameterisation as above. *)
type output =
  {pos_out : unit -> pos;
   seek_out : pos -> unit;
   output_char : char -> unit;
   output_byte : int -> unit;
   output_string : string -> unit;
   out_caml_channel : out_channel option;
   out_channel_length : unit -> pos}

(* Create input functions from a channel. *)
let input_of_channel ?(source = "channel") ch =
  let offset = ref 0 in
    {pos_in =
       (fun () -> pos_in ch - !offset);
     seek_in =
       (fun x -> seek_in ch (x + !offset));
     input_char =
       (fun () -> try Some (input_char ch) with End_of_file -> None);
     input_byte =
       (fun () -> try input_byte ch with End_of_file -> no_more);
     in_channel_length =
       in_channel_length ch;
     set_offset =
       (fun o -> offset := o);
     caml_channel = Some ch;
     source = source}

(* Create input functions from a. *)
let input_of_stream ?(source = "bytes") s =
  let offset = ref 0 in
    let ssize = bytes_size !(s.data) in
      let input_int () =
        let r =
          if s.pos > ssize - 1 then no_more else bget_unsafe !(s.data) s.pos
        in
          s.pos <- s.pos + 1;
          r
      in
        {pos_in =
           (fun () -> s.pos - !offset);
         seek_in =
           (fun p ->
              s.pos <- p + !offset);
         input_char =
           (fun () ->
              match input_int () with x when x = no_more -> None | s -> Some (Char.unsafe_chr s));
         input_byte =
           input_int;
         in_channel_length =
           ssize;
         set_offset =
           (fun o -> offset := o);
         caml_channel = None;
         source = source}

(* Create input functions from a Pdfio.bytes *)
let input_of_bytes ?(source = "bytes") b =
  input_of_stream ~source {pos = 0; data = ref b}

(* Create input functions from a string *)
let input_of_string ?(source = "string") s =
 let pos = ref 0 in
   let input_char () =
     if !pos < 0 then failwith "string input_char: attempt to be before beginning" else
     if !pos > String.length s - 1
       then
         begin
           pos := !pos + 1;
           None
         end
       else
         begin
           pos := !pos + 1;
           Some (String.unsafe_get s (!pos - 1))
         end
   in
     {pos_in =
        (fun () -> !pos);
      seek_in =
        (fun p -> if p < 0 then failwith "string seek_in: seek before beginning"; pos := p);
      input_char =
        (fun () -> input_char ());
      input_byte =
        (fun () ->
           if !pos < 0 then failwith "string input_byte: attempt to be before beginning" else
           if !pos > String.length s - 1
             then
               begin
                 pos := !pos + 1;
                 no_more
               end
             else
               begin
                 pos := !pos + 1;
                 int_of_char (String.unsafe_get s (!pos - 1))
               end);
      in_channel_length =
        String.length s;
      set_offset =
        (fun _ -> ());
      caml_channel =
        None;
      source =
        source}

(* Output functions over channels *)
let output_of_channel ch =
  {pos_out = (fun () -> pos_out ch);
   seek_out = seek_out ch;
   output_char = (fun c -> output_byte ch (int_of_char c));
   output_byte = output_byte ch;
   output_string = output_string ch;
   out_caml_channel = Some ch;
   out_channel_length = (fun () -> out_channel_length ch)}

(* Output functions over streams. If data is written past the end of a stream,
we extend the stream to that point plus one-third of that (new) size. Note that
this has an implication upon mixing reading and writing: the stream will have
junk in the extended section and will be longer than that which has been
written. *)
let output_of_bytes s =
  let highest_written = ref 0 in
    let output_int i =
      if s.pos > bytes_size !(s.data) - 1
        then
          let newstream = mkbytes (s.pos * 2) in
            for x = 0 to bytes_size !(s.data) - 1 do
              bset_unsafe newstream x (bget_unsafe !(s.data) x)
            done;
            bset_unsafe newstream s.pos i;
            highest_written := max !highest_written s.pos;
            s.pos <- s.pos + 1;
            s.data := newstream
        else
          begin
            highest_written := max !highest_written s.pos;
            bset_unsafe !(s.data) s.pos i;
            s.pos <- s.pos + 1
          end
    in
      {pos_out =
         (fun () -> s.pos);
       seek_out =
         (fun p -> s.pos <- p);
       output_char =
         (fun c -> output_int (int_of_char c));
       output_string =
         String.iter (function c -> output_int (int_of_char c));
       output_byte =
         output_int;
       out_caml_channel = None;
       out_channel_length =
         (fun () -> !highest_written + 1)}

let input_output_of_bytes l =
  let data = ref (mkbytes l) in
    (output_of_bytes {data = data; pos = 0}, data)

let extract_bytes_from_input_output o data =
  let len = o.pos_out () in
    let b = mkbytes len in
      for x = 0 to len - 1 do bset_unsafe b x (bget_unsafe !data x) done;
      b

(* Nudge forward one character. *)
let nudge i =
  ignore (i.input_byte ())

(* Read one character behind the current position, and reposition ourselves on
that character. *)
let read_char_back i =
  let pos = i.pos_in () in
    i.seek_in (pos - 1);
    let chr = i.input_char () in
      i.seek_in (pos - 1);
      chr

(* Go back one character in a file. *)
let rewind i =
  i.seek_in (i.pos_in () - 1)

(* Read a character, leaving the position unchanged. *)
let peek_char i =
  let r = i.input_char () in
    rewind i; r

(* Read a byte, leaving the position unchanged. *)
let peek_byte i =
  let r = i.input_byte () in
    rewind i; r

(* Make a bytes of an input channel. *)
let bytes_of_input_channel ch =
  let fi = input_of_channel ch in
    let size = fi.in_channel_length in
      let s = mkbytes size in
        for x = 1 to size do
          match fi.input_byte () with
          | b when b = no_more -> failwith "channel length inconsistent"
          | b -> bset_unsafe s (x - 1) b
        done;
        s

(* Save a bytes to a channel. *)
let bytes_to_output_channel ch data =
  for x = 1 to bytes_size data do
    output_byte ch (bget_unsafe data (x - 1))
  done

(* Like Pervasives.read_line *) 
let b = Buffer.create 256

let read_line i =
  Buffer.clear b;
  (* Raise EndOfInput if at end *)
  begin match i.input_byte () with
  | x when x = no_more -> raise End_of_file
  | _ -> ()
  end;
  rewind i;
  (* Read characters whilst <> newline or until end of input *)
  let rec read_chars () =
    match i.input_byte () with
    | x when x = no_more -> Buffer.contents b
    | x when Char.unsafe_chr x = '\n' -> Buffer.add_char b '\n'; Buffer.contents b
    | x -> Buffer.add_char b (Char.unsafe_chr x); read_chars ()
  in
    read_chars ()

(* Read all the lines in an input *)
let read_lines i =
  let ls = ref [] in
    try
      while true do
        ls := read_line i::!ls
      done;
      []
    with
      End_of_file -> rev !ls

let setinit i s o l =
  let max = bytes_size s - 1
  and last = o + 1 - 1 in
    if o > max || o < 0 || last < 0 || last > max then raise (Failure "setinit") else
      match s with
      | Short s ->
          begin match i.caml_channel with
          | None ->
              for x = o to o + l - 1 do String.unsafe_set s x (Char.unsafe_chr (i.input_byte ())) done
          | Some ch -> really_input ch s o l
          end
      | Long s ->
          for x = o to o + l - 1 do bigarray_unsafe_set s x (i.input_byte ()) done

let setinit_string i s o l =
  let max = String.length s - 1
  and last = o + 1 - 1 in
    if o > max || o < 0 || last < 0 || last > max then raise (Failure "setinit_string") else
      match i.caml_channel with
      | Some ch ->
          really_input ch s o l
      | None ->
          for x = o to o + l - 1 do String.unsafe_set s x (Char.unsafe_chr (i.input_byte ())) done

let bytes_of_input i o l =
  i.seek_in o;
  let s = String.create l in
    setinit_string i s 0 l;
    if l <= Sys.max_string_length then
      Short s
    else
      bytes_of_string s

let getinit i s o l =
  let max = bytes_size s - 1
  and last = o + 1 - 1 in
    if o > max || o < 0 || last < 0 || last > max then raise (Failure "getinit") else
      match s with
      | Short s ->
          begin match i.out_caml_channel with
          | None -> for x = o to o + l - 1 do i.output_byte (int_of_char (String.unsafe_get s x)) done
          | Some ch -> output ch s o l
          end
      | Long s ->
          for x = o to o + l - 1 do i.output_byte (bigarray_unsafe_get s x) done

(* The type of bit (MSB first) streams. *)
type bitstream =
  {input : input; (* The input from which bits are taken. It is advanced a byte at a time *)
   mutable currbyte : int; (* Current byte value from input *)
   mutable bit : int; (* Mask for getting the next bit (128, 64,... 2, 1 or 0 = none left) *)
   mutable bitsread : int (* A count of the number of bits read since inception. Debug use only *)}

(* A bitstream position is a byte (the position in the base input of the
CURRENT byte) together with the mask value (128 down to 0) *)
type bitstream_position = int * int * int * int

let bitstream_pos b =
  (b.input.pos_in (), b.currbyte, b.bit, b.bitsread)

let bitstream_seek b (pos, currbyte, bit, bitsread) =
  b.input.seek_in pos;
  b.currbyte <- currbyte;
  b.bit <- bit;
  b.bitsread <- bitsread

(* Make a bitstream from an input. *) 
let bitbytes_of_input i =
  {currbyte = 0;
   bit = 0;
   bitsread = 0;
   input = i}

(* Get a single bit *)
let rec getbit b =
  if b.bit = 0 then
    begin
      b.currbyte <-
        begin match b.input.input_byte () with
        | x when x = no_more -> raise End_of_file
        | x -> x
        end;
      b.bit <- 128;
      getbit b
    end
  else
    let r = b.currbyte land b.bit > 0 in
      b.bitsread <- b.bitsread + 1;
      b.bit <- b.bit / 2;
      r

(* Get a bit as an integer, set = 1, unset = 0 *)
let getbitint i =
  if getbit i then 1 else 0

(* Align on a byte boundary. *)
let align b =
  if b.bit > 0 then b.bitsread <- (b.bitsread / 8 + 1) * 8;
  b.bit <- 0

(* Further speed possibilities - if byte-aligned and bitstoget >=8 read whole bytes in.. *)

(* Get n (up to 32) bits from b, returned as an int32, taken highest bit
first. Getting 0 bits gets the value 0. *)
let getval_32 b n =
  if n < 0 then raise (Invalid_argument "Pdfio.getval_32 - n < 0") else
    if n = 0 then 0l else
      let r = ref Int32.zero in
        for x = n - 1 downto 0 do
          r := Int32.logor !r (Int32.shift_left (i32ofi (getbitint b)) x)
        done;
        !r

let getval_31 b n =
  if n < 0 then raise (Invalid_argument "Pdfio.getval_31 - n < 0") else
    if n = 0 then 0 else
      let r = ref 0 in
        for x = n - 1 downto 0 do
          r := !r lor (getbitint b lsl x)
        done;
        !r

(* Writing MSB-first bit streams *)

(* FIXME: This is excessively complicated and probably inefficient - we should
be able to do the appending and so on by other means? *)

(* The type: A current byte, the position in the byte (0 = nothing in it, 7 =
almost full), and the list (in reverse order) of full bytes so far *)
type bitstream_write =
  {mutable wcurrbyte : int;
   mutable wbit : int;
   mutable bytes : int list}

let make_write_bitstream () =
  {wcurrbyte = 0;
   wbit = 0;
   bytes = []}

let copy_write_bitstream b =
  let b' = make_write_bitstream () in
    b'.wcurrbyte <- b.wcurrbyte;
    b'.wbit <- b.wbit;
    b'.bytes <- b.bytes;
    b'

(* Put a single bit into bitstream b*)
let putbit b bit =
  assert (bit = 0 || bit = 1);
  match b.wbit with
  | 7 ->
      b.bytes <- (b.wcurrbyte lor bit) :: b.bytes;
      b.wbit <- 0;
      b.wcurrbyte <- 0
  | _ ->
      b.wbit <- b.wbit + 1;
      b.wcurrbyte <- b.wcurrbyte lor (bit lsl (8 - b.wbit))

let putbool b bit =
  putbit b (if bit then 1 else 0)

(* Put a multi-bit value n of bits bs (given as an int32) into bitstream b. *)
let rec putval b bs n =
  if bs < 0 || bs > 32 then raise (Invalid_argument "putval");
  match bs with
  | 0 -> ()
  | _ ->
      let bit =
        if land32 n (i32ofi (1 lsl (bs - 1))) > 0l then 1 else 0
      in
        putbit b bit;
        putval b (bs - 1) n

(* Align on a byte boundary, writing zeroes. *)
let align_write b =
  if b.wbit > 0 then
    for x = 1 to 8 - b.wbit do
      putbit b 0
    done

(* Get the output out. *)
let bytes_of_write_bitstream b =
  align_write b;
  bytes_of_list (rev b.bytes)

(* Return a list of booleans, representing (in order) the bits *)
let bits_of_write_bitstream b =
  let numbits = length b.bytes * 8 + b.wbit
  and bytes : bytes = bytes_of_write_bitstream b
  and bits = ref [] in
    let bitstream = bitbytes_of_input (input_of_bytes bytes) in
      for x = 1 to numbits do
        bits =| getbit bitstream
      done;
      rev !bits

(* Same, but from a list *)
let join_write_bitstreams ss =
  let c = make_write_bitstream () in
    iter
      (putbool c)
      (flatten (map bits_of_write_bitstream ss));
    c

(* Append b to a. Inputs unaltered. *)
let write_bitstream_append a b =
  join_write_bitstreams [a; b]

(* Same, but align at the join. *)
let write_bitstream_append_aligned a b =
  let c = copy_write_bitstream a in
    align_write c;
    write_bitstream_append c b

(* Print the next five characters, to the extent that they exist. *)
let debug_next_char i =
  try
    let a = unopt (i.input_char ()) in
      Printf.eprintf "%C = %i\n" a (int_of_char a)
  with
    _ -> ()

let debug_next_n_chars n i =
  for x = 1 to n do debug_next_char i done;
  prerr_string "\n";
  for x = 1 to n do rewind i done


