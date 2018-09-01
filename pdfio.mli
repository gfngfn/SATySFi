(** Generic Input/Ouput from/to channels, strings, files etc. *)

(* {2 Types} *)

(** An input. 

- Calling [pos_in ()] will give the current position (i.e the index of the next byte to be read) in the range zero to the length of the channel minus one.
- [seek_in x] moves to a given position.
- [input_char ()] returns None if end of input, or the next byte of the input as a character.
- [input_byte ()] returns the next byte as an integer, or [Pdfio.no_more] in case of end of input. 
- [in_channel_length] is the length of the channel.
- [set_offset] shifts the channel so positions run from offset to channel_length + offset - 1 instead of 0 to channel_length - 1.
- [caml_channel] is the underlying OCaml channel (if any) of the input.
- [source] is a string used to inficate the original source of the data, for debugging purposes.

*)
type input =
  {pos_in : unit -> int;
   seek_in : int -> unit;
   input_char : unit -> char option;
   input_byte : unit -> int;
   in_channel_length : int;
   set_offset : int -> unit;
   caml_channel : in_channel option;
   source : string}

(** An output.

- Calling [pos_out ()] gives the position of the next byte to be written
- [seek_out x] moves to a a given position in the output
- [output_char] writes a given character as the next byte
- [output_byte] writes an integer between 0 and 255 as the next byte
- [output_string] writes a string as a sequence of bytes
- [out_caml_channel] holds the OCaml channel this output is based on, if any
- [out_channel_length ()] gives the current length of the output channel

*)
type output =
  {pos_out : unit -> int;
   seek_out : int -> unit;
   output_char : char -> unit;
   output_byte : int -> unit;
   output_string : string -> unit;
   out_caml_channel : out_channel option;
   out_channel_length : unit -> int}

(** A distinguished byte value indicating "no more input" *)
val no_more : int

(** The type of fast to access but possibly very large arrays of bytes. *)
type bytes

(** {2 Building inputs} *)

(** Make an input from an OCaml input channel. *)
val input_of_channel : ?source:string -> in_channel -> input

(** Make an input from a bytes. *)
val input_of_bytes : ?source:string -> bytes -> input

(** Make an input from a string. *)
val input_of_string : ?source:string -> string -> input

(** {2 Building outputs} *)

(** Make an output from an OCaml channel *)
val output_of_channel : out_channel -> output

(** {2 Building an input-output} *)

(** An input-output in this context is an output of a bytes, whose content can
be extracted to another bytes having been written. For example, one might use a
write bitstream (see below), and then extract the result into a [bytes] *)

(** Build an input-ouput, with an initial buffer size. *)
val input_output_of_bytes : int -> output * bytes ref

(** Extract the contents of an input-output in bytes *)
val extract_bytes_from_input_output : output -> bytes ref -> bytes

(** {2 Compound operations on inputs} *)

(** Move forward one byte *)
val nudge : input -> unit

(** Move backward one byte *)
val rewind : input -> unit

(** Look at the next character without advancing the pointer. *)
val peek_char : input -> char option

(** Look at the next byte without advancing the pointer. Returns
[Pdfio.no_more] for end of file. *)
val peek_byte : input -> int

(** Read the previous character (if there is one), moving the pointer back one. *)
val read_char_back : input -> char option

(** Read a line from an input in the manner of [Pervasives.read_line]. *)
val read_line : input -> string

(** Read all the lines in an input, in the manner of [Pervasives.read_line]. *)
val read_lines : input -> string list

(** {2 Bytes } *)

(**/**)
type rawbytes

val raw_of_bytes : bytes -> rawbytes

val bytes_of_raw : rawbytes -> bytes
(**/**)

(** Make bytes from a given size. Contents not initialized. *)
val mkbytes : (int -> bytes)

(** Size of bytes. *)
val bytes_size : bytes -> int

(** Fill bytes with a value *)
val fillbytes : int -> bytes -> unit

(** Print bytes to standard output. Format undefined: for debug only. *)
val print_bytes : bytes -> unit

(** Get the value at a position in a bytes *)
val bget : bytes -> int -> int

(** Like [bget], but with no range checking *)
val bget_unsafe : bytes -> int -> int

(** [getinit f s o l] calls f on each s within o...l - 1 *)
val getinit : output -> bytes -> int -> int -> unit

(** [bset s n v] sets the value n at position v in bytes *)
val bset : bytes -> int -> int -> unit

(** Like [bset] but with no range checking *)
val bset_unsafe : bytes -> int -> int -> unit

(** [setinit i s o l] sets s o...o + l - 1 from the input *)
val setinit : input -> bytes -> int -> int -> unit

(** [setinit_string i s o l] sets s o...o + l - 1 from the input *)
val setinit_string : input -> string -> int -> int -> unit

(** [setinit_bytes i o l] gives a [bytes] with s o...o + l - 1 from the input *)
val bytes_of_input : input -> int -> int -> bytes

(** Make bytes from a string. *)
val bytes_of_string : string -> bytes

(** Make bytes from a list of integers, each between 0 and 255. *)
val bytes_of_list : int list -> bytes

(** Make bytes from a character list. *)
val bytes_of_charlist : char list -> bytes

(** Make bytes from a list of integer arrays. *)
val bytes_of_arraylist : int array list -> bytes

(** Make bytes from an integer array *)
val bytes_of_int_array : int array -> bytes

(** An integer array of bytes from bytes *)
val int_array_of_bytes : bytes -> int array

(** Integer array from a string *)
val int_array_of_string : string -> int array

(** A string from a list of integer arrays *)
val string_of_int_arrays : int array list -> string

(** A string from a single int array *)
val string_of_int_array : int array -> string

(** Map bytes onto itself using a function on each byte *)
val bytes_selfmap : (int -> int) -> bytes -> unit

(** Make a string from a bytes. Fails if array is longer than
[String.max_length]. *)
val string_of_bytes : bytes -> string

(** Make a character list from a bytes *)
val charlist_of_bytes : bytes -> char list

(** Copy bytes. *)
val copybytes : bytes -> bytes

(** Write a bytes to an output channel *)
val bytes_to_output_channel : out_channel -> bytes -> unit

(** Extract a bytes from an input or output. *)
val bytes_of_input_channel : in_channel -> bytes

(** {2 Bit streams for reading} *)

(** The type of most-significant-bit-first bitstreams over inputs. *)
type bitstream

(** Make a bitstream from an input. *)
val bitbytes_of_input : input -> bitstream

(** The type of a position within a bitstream *)
type bitstream_position

(** Get the current position. It's abstract, so can't be manipulated, but it
can be used to return to a previous point *)
val bitstream_pos : bitstream -> bitstream_position

(** Seek to a position within a bitstream *)
val bitstream_seek : bitstream -> bitstream_position -> unit

(** Get a bit *)
val getbit : bitstream -> bool

(** Get a bit, but as an integer, 0 or 1. *)
val getbitint : bitstream -> int

(** Align the bitstream on a byte boundary *)
val align : bitstream -> unit

(** Get up to 32 bits as a 32 bit value *)
val getval_32 : bitstream -> int -> int32

(** Get up to 31 bits as a native integer *)
val getval_31 : bitstream -> int -> int

(** {2 Bit streams for writing} *)

(** The type of most-significant-bit-first bitstreams for writing over outputs. *)
type bitstream_write

(** Return a new write bistream. *)
val make_write_bitstream : unit -> bitstream_write

(** Put a single bit, 0 or 1. *)
val putbit : bitstream_write -> int -> unit

(** Put a multi-bit value (given as an [int32]) containing the given number of
useful bits into a bitstream *)
val putval : bitstream_write -> int -> int32 -> unit

(** Byte-align. *)
val align_write : bitstream_write -> unit

(** Append two write bitstreams, aligning at boundary *)
val write_bitstream_append_aligned :
  bitstream_write -> bitstream_write -> bitstream_write

(** Build bytes from a write bitstream, padding the with zero-valued bits. *)
val bytes_of_write_bitstream : bitstream_write -> bytes

(** {2 Debug } *)

(** Debug the next [n] chars to standard output and then rewind back *)
val debug_next_n_chars : int -> input -> unit

