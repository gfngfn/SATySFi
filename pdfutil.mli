(** General Functions. Typically one will [open Pdfutil] and not use [open] elsewhere. *)

val position_1 : 'a -> 'a list -> int option
val position : 'a -> 'a list -> int option

(** {2 Debug Printing} *)

(** Print a string and flush standard output. *)
val flprint : string -> unit

(** Print a list of floating point numbers separated by spaces. *)
val print_floats : float list -> unit

(** Print a list of integers separated by spaces *)
val print_ints : int list -> unit

(** Print a list of [int32]s separated by spaces *)
val print_int32s : int32 list -> unit

(** {2 Strings} *)

(** Return the first character of a string, should it have one. Otherwise return
[None]. *)
val firstchar : string -> char option

(** Return the first character of a string, should it have one. Otherwise return
[None]. *)
val lastchar : string -> char option

(** List of characters representing a string. *)
val explode : string -> char list

(** String representing a list of characters. Fails if list is longer than
[Sys.max_string_length]. *)
val implode : char list -> string

(** Make a string from a character. *)
val string_of_char : char -> string

(** Calling [string_replace_all x x' s] replaces all instances of [x] with [x']
in [s], returning a new string. *)
val string_replace_all : string -> string -> string -> string

val string_replace_all_lazy : string -> (unit -> string) -> string -> string

(** {2 Lists} *)

val hd : ('a list -> 'a)
val tl : ('a list -> 'a list)
val rev : ('a list -> 'a list)
val iter : (('a -> unit) -> 'a list -> unit)
val iter2 : (('a -> 'b -> unit) -> 'a list -> 'b list -> unit)
val iter3 : ('a -> 'b -> 'c -> unit) -> 'a list -> 'b list -> 'c list -> unit
val append : 'a list -> 'a list -> 'a list
val ( @ ) : ('a list -> 'a list -> 'a list)
val flatten : 'a list list -> 'a list
val rev_map : (('a -> 'b) -> 'a list -> 'b list)
val map : ('a -> 'b) -> 'a list -> 'b list
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val split : ('a * 'b) list -> 'a list * 'b list
val split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
val split5 : ('a * 'b * 'c * 'd * 'e) list -> 'a list * 'b list * 'c list * 'd list * 'e list
val split6 : ('a * 'b * 'c * 'd * 'e * 'f) list -> 'a list * 'b list * 'c list *
'd list * 'e list * 'f list
val split8 : ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) list ->
  'a list * 'b list * 'c list * 'd list * 'e list * 'f list * 'g list * 'h list
val combine : 'a list -> 'b list -> ('a * 'b) list
val combine3 : 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
val length : ('a list -> int)
val sort : (('a -> 'a -> int) -> 'a list -> 'a list)
(** Tail-recursive versions of list functions (and some simple variations). See
[Pervasives] for documentation. *)

(** Cumulative sum of a list given an initial value. For example, [cumulative_sum 1 [2; 3; 4]] is [[3; 6; 10]] *)
val cumulative_sum : int -> int list -> int list

(** Count the number of elements in a list for which predicate is true. *)
val lcount : ('a -> bool) -> 'a list -> int

(** Return just the non-[None] elements of an ['a option list] *)
val losenones : 'a option list -> 'a list

(** Map with a function returning an [option], dropping all [None] results and
extracting all [Some] ones. *)
val option_map : ('a -> 'b option) -> 'a list -> 'b list

(** Like [option_map] but with a two-argument function and two (equal-length)
input lists. Uses [List.rev_map2] internally and may raise the same exception.
*)
val option_map2 : ('a -> 'b -> 'c option) -> 'a list -> 'b list -> 'c list

(** Synonym for [List.mem]. *)
val mem : ('a -> 'a list -> bool)

(** Similar to [rev_map], but 3 arguments. *)
val rev_map3 :
  ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list

(** Similar to [map2], but 3 arguments. *)
val map3 : ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list

(** Similar to [rev_map], but 4 arguments. *)
val rev_map4 :
  ('a -> 'b -> 'c -> 'd -> 'e) -> 'a list -> 'b list -> 'c list -> 'd list -> 'e list

(** Similar to [map2], but 4 arguments. *)
val map4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a list -> 'b list -> 'c list -> 'd list -> 'e list 

(** Similar to [rev_map], but 5 arguments. *)
val rev_map5 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
  'a list -> 'b list -> 'c list -> 'd list -> 'e list -> 'f list

(** Similar to [map2], but 5 arguments. *)
val map5 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
  'a list -> 'b list -> 'c list -> 'd list -> 'e list -> 'f list

(** [take l n] takes [n] elements from the list raising [Invalid_argument] if
there are not enough elements to take or if n < 0. *)
val take : 'a list -> int -> 'a list

(** The same as [take],  but with the arguments reversed. *)
val take' : int -> 'a list -> 'a list

(** [drop l n] drops [n] elements from the list raising [Invalid_argument] if [n]
 < 0 or there are not enough elements. *)
val drop : 'a list -> int -> 'a list

(** Same as [drop], but with the arguments reversed. *)
val drop' : int -> 'a list -> 'a list

(** Take elements from a list while a given predicate is true, returning them in order. *)
val takewhile : ('a -> bool) -> 'a list -> 'a list

(** The same as [takewhile], but the list is returned in reverse order. *) 
val takewhile_reverse : ('a -> bool) -> 'a list -> 'a list

(** Drop elements from a list while a given predicate is true. *)
val dropwhile : ('a -> bool) -> 'a list -> 'a list

(** [cleave l n] splits [l] into two parts, returned as a tuple. The first
 contains the first [n] elements, the second the remainder.
 Order is preserved. [Invalid_argument] is raised on negative argument or not
 enough elements in list. *)
val cleave : 'a list -> int -> 'a list * 'a list

(** Same, but split point controlled by a predicate, which is true for elements
in the first returned list. e.g [cleavewhile even [2;4;5;6]] produces [([2;4], [5;6])] *)
val cleavewhile : ('a -> bool) -> 'a list -> 'a list * 'a list

(** The same as [cleavewhile], but the output lists are each unordered. *)
val cleavewhile_unordered : ('a -> bool) -> 'a list -> 'a list * 'a list

(** [isolate p p' l] isolate a central section of a list [l], from the first
element after the elements for which predicate [p] is true, to the element
before [p'] is first true. *)
val isolate : ('a -> bool) -> ('a -> bool) -> 'a list -> 'a list

(** Interleave an element among a list, so that [interleave 0 [1; 2; 3]]
yields [[1; 0; 2; 0; 3]]. An empty or singleton list is unchanged. *)
val interleave : 'a -> 'a list -> 'a list

(** Interleave two equal-length lists, taking from the first list first. *)
val interleave_lists : 'a list -> 'a list -> 'a list

(** Collate a list into a list of lists based upon a comparison function by which
it has already been sorted. e.g [collate compare [1; 2; 2; 3; 3]] calculates
[[[1]; [2;2]; [3;3]]]. *)
val collate : ('a -> 'a -> int) -> 'a list -> 'a list list

(** Map on lists of lists. So [map_lol f] is [map (map f)]. *)
val map_lol : ('a -> 'b) -> ('a list list -> 'b list list)

(** Produce a list of overlapping pairs of elements in a list in order, producing
the empty list if on singleton input. e.g [pairs [1; 2; 3]] is [[(1, 2); (2,
3)]]. *)
val pairs : 'a list -> ('a * 'a) list

(** [List.mem] with arguments reversed. *)
val mem' : 'a list -> 'a -> bool

(** Remove duplicates from a list. *)
val setify : 'a list -> 'a list

(** Same as [setify], but preserve order. *)
val setify_preserving_order : 'a list -> 'a list

(** The set [setminus a b] contains all those elements which are in [a] but are
do not appear in [b]. *)
val setminus : 'a list -> 'a list -> 'a list

(** The same, but preserving order *)
val setminus_preserving_order : 'a list -> 'a list -> 'a list

(** Return a list of the heads of a list of lists, each of which has at least
one element, preserving order. *)
val heads : 'a list list -> 'a list

(** Ditto, tails. *)
val tails : 'a list list -> 'a list list

(** Take a list of lists of equal length, and turn into a list of lists, the
first containing all the first elements of the original lists, the second the
second, and so on. *)
val zipn : 'a list list -> 'a list list

(** Couple the elements of a list [l] using given function. For instance,
[couple ( + ) [[1; 3; 5]]] is [[4; 8]]. The two elements are applied to [f] in
the order in which they appear in the input list. *)
val couple : ('a -> 'a -> 'b) -> 'a list -> 'b list

(** As [couple], but an extra unary function is applied to any last (odd) element. *)
val couple_ext : ('a -> 'a -> 'b) -> ('a -> 'b) -> 'a list -> 'b list

(** Apply [couple] repeatedly until only one element remains. Return that
element. *)
val couple_reduce : ('a -> 'a -> 'a) -> 'a list -> 'a

(** A similar function to [couple], but the coupling is non-overlapping. So
[pair ( + ) [1; 2; 3; 4]] is [3; 7]. *)
val pair : ('a -> 'a -> 'a) -> 'a list -> 'a list

(** A version of [pair] which adds a unary function for the singleton, much
like [couple_ext]. *)
val pair_ext : ('a -> 'a -> 'b) -> ('a -> 'b) -> 'a list -> 'b list

(** As [couple_reduce] is to [couple], so [pair_reduce] is to [pair]. *)
val pair_reduce : ('a -> 'a -> 'a) -> 'a list -> 'a

val keep : (('a -> bool) -> 'a list -> 'a list)

(** [List.filter] has a confusing name, so we define [keep] and [lose] to avoid
error. [keep] keeps all those matching the predicate, [lose] loses all thos
matching a predicate. *)
val lose : ('a -> bool) -> ('a list -> 'a list)

(** [many x n] makes a list of length [n] with each element equal to [x]. *)
val many : 'a -> n:int -> 'a list

(** A version where we need to apply unit each time, for instance when producing
a list of random numbers. Result is ordered. *)
val manyunique : (unit -> 'a) -> int -> 'a list

(** Split a list into some lists of length [n] (and possibly a final one of
length [< n]), preserving order. *)
val splitinto : int -> 'a list -> 'a list list

(** Non-tail recursive version of splitinto, for use only when [n] is small and fixed. *)
val splitinto_small : int -> 'a list -> 'a list list

(** Split a list at the given positions. Point 1 means after the first element *)
val splitat : int list -> 'a list -> 'a list list

(** Select the nth element in a list (first is element 1). Raises
[Invalid_argument] if the number is out-of-range. *)
val select : int -> 'a list -> 'a

(** [replace n x xs] replaces the [n]th element of the list [xs] with [x] (the first is element 1) *)
val replace_number : int -> 'a -> 'a list -> 'a list

val isnull : 'a list -> bool
val notnull : 'a list -> bool
(** Predicates on the nullness of a list. *)

(** Find the last element of a list. Raises [Invalid_argument] on empty list. *)
val last : 'a list -> 'a

(** Produce a list containing all but the last element of a list. For the empty
list, returns the empty list. *)
val all_but_last : 'a list -> 'a list

(** Find the first and last element of a list. If the list has one element, that
is returned twice. If it has no elements, raise [Invalid_argument]. *)
val extremes : 'a list -> 'a * 'a

(** Return the first, middle and last elements of a list which has length at
least two. Otherwise, raise [Invalid_argument]. *)
val extremes_and_middle : 'a list -> 'a * 'a list * 'a

(** [ilist 2 5] returns [[2; 3; 4; 5]]. However, [ilist 5 2] raises [Invalid_argument]. *)  
val ilist : int -> int -> int list

(** Same as [ilist], but return the empty list for [ilist x x] rather than [[x]] *)
val ilist_null : int -> int -> int list

(** Same as [ilist_null], but return empty list if start [>] end, instead of failing *)
val ilist_fail_null : int -> int -> int list

(** [indx l] returns [[1; 2; 3]] if [l] has length 3, for example. *)
val indx : 'a list -> int list

(** Same as [indx], but 0-based. *)
val indx0 : 'a list -> int list

(** Same as [indx], but n-based. *)
val indxn : int -> 'a list -> int list

(** Remove the second, fourth etc. elements from a list, saving the last element
(if of even length) e.g [drop_evens [1; 2; 3; 4; 5; 6]] is [[1; 3; 5; 6]]. *) 
val drop_evens : 'a list -> 'a list

(** Same as [drop_evens], but don't save the last element. *)
val really_drop_evens : 'a list -> 'a list

(** Remove the first, third etc. The last odd element is not saved. e.g [drop_odds
[1;2;3;4;5;6;7]] is [[2;4;6]]. *)
val drop_odds : 'a list -> 'a list

(** Like [List.tl] but [[]] yields [[]]. *)
val tail_no_fail : 'a list -> 'a list

(** Append with reversed arguments. *)
val ( @@ ) : 'a list -> 'a list -> 'a list

(** [replaceinlist f x l] replaces any element of [l] for which [f l] is true
with [x]. *)
val replaceinlist : ('a -> bool) -> 'a -> 'a list -> 'a list

(** Find the position of the first element matching a predicate. The first
element is number one. Fails with [Not_found] if no element matches the
predicate. *)
val index : ('a -> bool) -> 'a list -> int

(** Split a list into a list of lists at every point where a predicate is true *)
val split_around : ('a -> bool) -> 'a list -> 'a list list

(** {2 32 and 64 bit integers} *)

val i32ofi : (int -> int32)
val i32toi : (int32 -> int)
val i32tof : (int32 -> float)
val i32add : (int32 -> int32 -> int32)
val i32sub : (int32 -> int32 -> int32)
val i32div : (int32 -> int32 -> int32)
val i32mul : (int32 -> int32 -> int32)
val lsr32 : (int32 -> int -> int32)
val lsl32 : (int32 -> int -> int32)
val lor32 : (int32 -> int32 -> int32)
val land32 : (int32 -> int32 -> int32)
val lnot32 : (int32 -> int32)
val lxor32 : (int32 -> int32 -> int32)
val i32succ : (int32 -> int32)
val i32pred : (int32 -> int32)
val i32max : (int32 -> int32 -> int32)
val i32min : (int32 -> int32 -> int32)

val i64ofi : (int -> int64)
val i64toi : (int64 -> int)
val i64tof : (int64 -> float)
val i64add : (int64 -> int64 -> int64)
val i64sub : (int64 -> int64 -> int64)
val i64div : (int64 -> int64 -> int64)
val i64mul : (int64 -> int64 -> int64)
val lsr64 : (int64 -> int -> int64)
val lsl64 : (int64 -> int -> int64)
val lor64 : (int64 -> int64 -> int64)
val land64 : (int64 -> int64 -> int64)
val lnot64 : (int64 -> int64)
val lxor64 : (int64 -> int64 -> int64)
val i64succ : (int64 -> int64)
val i64pred : (int64 -> int64)
val i64max : (int64 -> int64 -> int64)
val i64min : (int64 -> int64 -> int64)
val i32ofi64 : (int64 -> int32)
val i64ofi32 : (int32 -> int64)
(** Shortened names for functions from [Int32] and [Int64]. *)


(** {2 Association lists} *)

(** [lookup x l] looks up something, returning [None] if not found. *)
val lookup : 'a -> ('a * 'b) list -> 'b option

(** Same as [lookup], but no option type. Raises [Not_found] if the key is not there. *) 
val lookup_failnull : 'a -> ('a * 'b) list -> 'b

(** [add k v l] Adds [(k, v)] to a dictionary, replacing any existing binding of
[k]. *)
val add : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list

(** [replace k v l] replaces the existing binding of [k] in [l] with one with binds [k]
to [v]. Raises [Not_found] if there is nothing to replace. *)
val replace : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list

(** Remove something from a list, if it's there. If not, don't complain. *)
val remove : 'a -> ('a * 'b) list -> ('a * 'b) list

(** Merge two lists, preferring elements in the second in the case of clashes. *)
val mergedict : ('a * 'b) list -> ('a * 'b) list -> ('a * 'b) list

(** {2 References} *)

(** Set a boolean reference to [true] *)
val set : bool ref -> unit

(** Set a boolean reference to [false] *)
val clear : bool ref -> unit

(** Flip a boolean reference *)
val flip : bool ref -> unit

val ( += ) : int ref -> int -> unit
val ( -= ) : int ref -> int -> unit
val ( /= ) : int ref -> int -> unit
val ( *= ) : int ref -> int -> unit
(** Operations on integer references *)

val ( +.= ) : float ref -> float -> unit
val ( -.= ) : float ref -> float -> unit
val ( /.= ) : float ref -> float -> unit
val ( *.= ) : float ref -> float -> unit
(** Operations on floating point references *)

(** Cons something onto the contents of a list reference. *)
val ( =| ) : 'a list ref -> 'a -> unit

(** Append something to the front of the contents of a list reference. *)
val ( =@ ) : 'a list ref -> 'a list -> unit

(** {2 Geometry} *)

(** The type of vectors. *)
type vector = float * float

(** [mkvector (a, b) (c, d)] makes a vector from point [(a, b)] to point [(c, d)]. *)
val mkvector : float * float -> float * float -> vector

(** Invert a vector. *)
val invert : vector -> vector

(** Offset a point by a vector. *)
val offset_point : vector -> float * float -> vector

(** Find the vector [pi / 2] anticlockwise from the given one. *)
val perpendicular : 'a * float -> float * 'a

(** Square a number *)
val sqr : float -> float

(** The length of a vector. *)
val veclength : vector -> float

(** Scale a vector to a given length. *)
val scalevectolength : float -> vector -> vector

(** Make a unit vector in the direction from one point to a second. *)
val mkunitvector : float * float -> float * float -> vector

(** Find the point equidistant between two others. *)
val between : float * float -> float * float -> float * float

(** Cartesian distance between two points. *)
val distance_between : float * float -> float * float -> float

(** {2 Numbers} *)

(** Round a real. *)
val round : float -> float

(** The same, returning an integer. *)
val iround : float -> int

(** The largest power of two by which a number is exactly divisible. *)
val largest_pow2_divisible : int -> int

(** Largest power of two smaller or equal to an integer. *)
val pow2lt : int -> int

(** Largest power of two greater or equal to an integer. *)
val pow2gt : int -> int

(** Base two logarithm *)
val log2of : int -> int

(** [pow x y] is y to the power x *)
val pow : int -> int -> int

(** Monomorphic integer version of [Pervasives.compare] *)
val compare_i : int -> int -> int

val min : int -> int -> int
val max : int -> int -> int
(** Monomorphic integer versions of [Pervasives] functions. *)

val fmin : float -> float -> float
val fmax : float -> float -> float
val fabs : float -> float
(** Monomorphic floating-point versions of [Pervasives] functions *)

val even : int -> bool
val odd : (int -> bool)
(** Even and odd predicates on integers. Work for negative numbers. *)

(** Pi. *)
val pi : float

(** Square root of two. *)
val root2 : float

val rad_of_deg : float -> float
val deg_of_rad : float -> float
(** Convert between radians and degrees. *)

(** The call [sign_extend l n] extends [n] of length [l] bits to fit a native integer *)
val sign_extend : int -> int -> int

(** A character is a decimal digit. *)
val isdigit : char -> bool

(** Same as [Pervasives.int_of_float] *)
val toint : float -> int

(** Make sure a floating point number is no degenarate, by making it zero if it is. *)
val safe_float : float -> float

(** {2 Options} *)

val some : 'a option -> bool
val none : 'a option -> bool
(** Predicates on the someness or noneness of an ['a option]. *)

(** Strip the [Some] from an option. Fail if it's [None]. *)
val unopt : 'a option -> 'a

(** {2 Hash tables} *)

(** The empty zero-sized hash table. *) 
val null_hash : unit -> ('a, 'b) Hashtbl.t

(** Option lookup on hashtables *)
val tryfind : (('a, 'b) Hashtbl.t -> 'a -> 'b option)

(** Make a list of key-value pairs reflecting the contents of a hash table. *)
val list_of_hashtbl : ('a, 'b) Hashtbl.t -> ('a * 'b) list

(** Build a hashtable from a dictionary (list of key-value pairs). Items are
added from left to right, with no checking for duplicate keys being performed. *)
val hashtable_of_dictionary : ('a * 'b) list -> ('a, 'b) Hashtbl.t

val hashset_of_list : 'a list -> ('a, unit) Hashtbl.t

(** {2 Trees} *)

(** The type for binary trees. *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree

(** {2 Arrays} *)

(** [iter2] on arrays. *)
val array_iter2 : ('a -> 'b -> unit) -> 'a array -> 'b array -> unit

(** [map2] on arrays. *)
val array_map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array

(** Swaps the data at the given indexes in an array. *)
val swap : 'a array -> int -> int -> unit

(** Set all the values of an array. *)
val set_array : 'a array -> 'a -> unit

(** {2 Control flow} *)

(** [memoize f] builds a memoized version of the function [f]. *)
val memoize : (unit -> 'a) -> unit -> 'a

(** The identity function. *)
val ident : 'a -> 'a

(** Apply a function [f] [n] times to initial argument [arg]. *)
val applyn : ('a -> 'a) -> int -> 'a -> 'a

(** This is equivalent to [let ( <| ) a b = a b] *)
val ( <| ) : ('a -> 'b) -> 'a -> 'b

(** [mkunit f x] gives [fun () -> f x] *)
val mkunit : ('a -> 'b) -> 'a -> unit -> 'b

(** {2 Tuples} *)

(** Make a tuple. *)
val tuple : 'a -> 'b -> 'a * 'b

(** Consing to each of a pair of lists at the same time. *)
val conspair : ('a * 'b) * ('a list * 'b list) -> 'a list * 'b list

(** Version of [conspair] where there may or may not be somthing to cons in each case. *)
val conspairopt :
  ('a option * 'b option) * ('a list * 'b list) -> 'a list * 'b list

(** Make consecutive elements of an even-length list into a list of pairs. *)
val pairs_of_list : 'a list -> ('a * 'a) list

(** [do_return f g] Evaluate [f ()], evaluate and ignore [g ()], return [f ()], in that order. *)
val do_return : (unit -> 'a) -> (unit -> 'b) -> 'a

(** [do_many f n] calls [f ()] n times. *)
val do_many : (unit -> unit) -> int -> unit

(** {2 Equality} *)

(** Equality. *)
val eq : ('a -> 'a -> bool)

(** Inequality. *)
val neq : ('a -> 'a -> bool)

(** Like [Pervasives.compare], but the other way around. *)
val rev_compare : 'a -> 'a -> int

(** {2 Logic} *)
(** The always-true predicate. *)
val always : 'a -> bool

(** The always-false predicate. *)
val never : 'a -> bool

(** Exclusive OR *)
val ( |&| ) : bool -> bool -> bool

(** Invert a predicate. *)
val notpred : ('a -> bool) -> 'a -> bool

(** {2 Boxes} *)

(** The smallest box enclosing both given integer boxes. Each box is [(xmin, xmax, ymin, ymax)]. *)
val box_union :
  int * int * int * int -> int * int * int * int -> int * int * int * int

(** The smallest box enclosing both given floating-point boxes. Each box is [(xmin, xmax, ymin, ymax)]. *)
val box_union_float :
  float * float * float * float -> float * float * float * float -> float * float * float * float

(** The intersection box (if any) of two integer boxes, each defined as [xmin ymin xmax ymax]. *) 
val box_overlap :
  int -> int -> int -> int -> int -> int -> int -> int ->
    (int * int * int * int) option

(** The intersection box (if any) of two floating-point boxes, each defined as [xmin ymin xmax ymax]. *)
val box_overlap_float :
  float -> float -> float -> float -> float -> float -> float -> float ->
  (float * float * float * float) option

(** {2 Filenames} *)

(** Return a list of leafnames for the given folder in the current folder *)
val leafnames_of_dir : string -> string list

(** {2 Roman numerals} *)

(** Uppercase roman representation of a number *)
val roman_upper : int -> string

(** Lowercase roman representation of a number *)
val roman_lower : int -> string

