(* This module contains general-purpose functions used in many modules.
Typically a module will use the [open] keyword to bring these definitions up to
top level, so their names are considered reserved words in other modules.

All functions in this module are tail-recursive, unless otherwise noted. *)

let rec position_gen n e = function
  [] -> None
| e'::t when e = e' -> Some n
| _::t -> position_gen (n + 1) e t

let position e l = position_gen 0 e l

let position_1 e l = position_gen 1 e l

(* Replace all instances of x with x' in s *)
let string_replace_all x x' s =
  if x = "" then s else
    let p = ref 0
    and slen = String.length s
    and xlen = String.length x in
      let output = Buffer.create (slen * 2) in
        while !p < slen do
          try
            if String.sub s !p xlen = x
              then (Buffer.add_string output x'; p := !p + xlen)
              else (Buffer.add_char output s.[!p]; incr p)
          with
            _ -> Buffer.add_char output s.[!p]; incr p
        done;
        Buffer.contents output

let string_replace_all_lazy x x' s =
  if x = "" then s else
    let p = ref 0
    and slen = String.length s
    and xlen = String.length x in
      let output = Buffer.create (slen * 2) in
        while !p < slen do
          try
            if String.sub s !p xlen = x
              then (Buffer.add_string output (x' ()); p := !p + xlen)
              else (Buffer.add_char output s.[!p]; incr p)
          with
            _ -> Buffer.add_char output s.[!p]; incr p
        done;
        Buffer.contents output

(* Print something and then flush standard output. *)
let flprint s =
  print_string s; flush stdout

(* Debug printing *)
let dp_print = ref false

let dpr s = if !dp_print then flprint s

(* [xxx] is a tail-recursive version of [List.xxx]. See [List] module for
details. *)
let sort = List.sort

let hd = List.hd

let tl = List.tl

let rev = List.rev

let iter = List.iter

let iter2 = List.iter2

let rec iter3 f a b c =
  match a, b, c with
  | [], [], [] -> ()
  | ah::a', bh::b', ch::c' ->
      f ah bh ch;
      iter3 f a' b' c'
  | _ -> raise (Invalid_argument "iter3")

let append a b =
  List.rev_append (rev a) b

let ( @ ) = append

let flatten lists =
  let rec flatten out = function
    | [] -> out
    | l::ls -> flatten (append l out) ls
  in
    flatten [] (rev lists)

let rev_map = List.rev_map

let map f l =
  rev (List.rev_map f l)

let map2 f a b =
  rev (List.rev_map2 f a b)

let split l =
  let rec split_inner (l1, l2) = function
    | [] -> rev l1, rev l2
    | (a, b)::t -> split_inner (a::l1, b::l2) t
  in
    split_inner ([], []) l

let split3 l =
  let rec split3_inner (l1, l2, l3) = function
    | [] -> rev l1, rev l2, rev l3
    | (a, b, c)::t -> split3_inner (a::l1, b::l2, c::l3) t
  in
    split3_inner ([], [], []) l

let split5 l =
  let rec split5_inner (l1, l2, l3, l4, l5) = function
    | [] -> rev l1, rev l2, rev l3, rev l4, rev l5
    | (a, b, c, d, e)::t -> split5_inner (a::l1, b::l2, c::l3, d::l4, e::l5) t
  in
    split5_inner ([], [], [], [], []) l

let split6 l =
  let rec split6_inner (l1, l2, l3, l4, l5, l6) = function
    | [] -> rev l1, rev l2, rev l3, rev l4, rev l5, rev l6
    | (a, b, c, d, e, f)::t ->
        split6_inner (a::l1, b::l2, c::l3, d::l4, e::l5, f::l6) t
  in
    split6_inner ([], [], [], [], [], []) l

let split8 l =
  let rec split8_inner (l1, l2, l3, l4, l5, l6, l7, l8) = function
    | [] -> rev l1, rev l2, rev l3, rev l4, rev l5, rev l6, rev l7, rev l8
    | (a, b, c, d, e, f, g, h)::t ->
        split8_inner (a::l1, b::l2, c::l3, d::l4, e::l5, f::l6, g::l7, h::l8) t
  in
    split8_inner ([], [], [], [], [], [], [], []) l

let combine a b =
  let pairs = ref [] in
    try
      List.iter2 (fun x y -> pairs := (x, y)::!pairs) a b;
      rev !pairs
    with
      Invalid_argument _ -> raise (Invalid_argument "Utility.combine")

let combine3 a b c =
  let pairs = ref [] in
    try
      iter3 (fun x y z -> pairs := (x, y, z)::!pairs) a b c;
      rev !pairs
    with
      Invalid_argument _ -> raise (Invalid_argument "Utility.combine3")
 
let fold_left f b l = List.fold_left f b l

let fold_right f l e =
  List.fold_left (fun x y -> f y x) e (rev l)

let length = List.length

let rec rev_map3_inner f a b c outputs =
  match a, b, c with
  | [], [], [] -> outputs
  | ha::ta, hb::tb, hc::tc ->
      rev_map3_inner f ta tb tc (f ha hb hc::outputs)
  | _ -> raise (Invalid_argument "map3")

let rev_map3 f a b c =
  rev_map3_inner f a b c []

let map3 f a b c =
  rev (rev_map3 f a b c)

let rec rev_map4_inner f a b c d outputs =
  match a, b, c, d with
  | [], [], [], [] -> outputs
  | ha::ta, hb::tb, hc::tc, hd::td ->
      rev_map4_inner f ta tb tc td (f ha hb hc hd::outputs)
  | _ -> raise (Invalid_argument "map4")

let rev_map4 f a b c d =
  rev_map4_inner f a b c d []

let map4 f a b c d =
  rev (rev_map4 f a b c d)

let rec rev_map5_inner f a b c d e outputs =
  match a, b, c, d, e with
  | [], [], [], [], [] -> outputs
  | ha::ta, hb::tb, hc::tc, hd::td, he::te ->
      rev_map5_inner f ta tb tc td te (f ha hb hc hd he::outputs)
  | _ -> raise (Invalid_argument "map5")

let rev_map5 f a b c d e =
  rev_map5_inner f a b c d e []

let map5 f a b c d e =
  rev (rev_map5 f a b c d e)

(* Calculate the cumulative sum of a list given a base e.g [cumulative_sum 5
[1;2;3] = [6; 8; 11]] *)
let cumulative_sum b l = 
  let rec cumulative_sum prev bse = function
    | [] -> rev prev
    | h::t -> cumulative_sum ((bse + h)::prev) (bse + h) t
  in
    cumulative_sum [] b l

(* Split a list into a list of lists at every point where [p] is true *)
let rec split_around_inner p prev curr = function
  | [] -> if curr = [] then (rev prev) else (rev (rev curr::prev))
  | h::t ->
      if p h
        then split_around_inner p (rev curr::prev) [] t
        else split_around_inner p prev (h::curr) t

let split_around p l =
  split_around_inner p [] [] l

(* Count the number of elements matching a predicate. *)
let rec lcount_inner p c = function
  | [] -> c
  | h::t ->
      if p h
        then lcount_inner p (c + 1) t
        else lcount_inner p c t

let lcount p l =
  lcount_inner p 0 l

(* Find the position of the first element matching a predicate. The first
element is number one. *)
let rec index_inner n p = function
  | [] -> dpr "b"; raise Not_found
  | h::_ when p h -> n
  | _::t -> index_inner (n + 1) p t

let index n p = index_inner 1 n p

(* Functions on Strings *)

let firstchar s =
  try Some s.[0] with Invalid_argument _ -> dpr "3R"; None

let lastchar s =
  try Some s.[String.length s - 1] with Invalid_argument _ -> dpr "3S"; None

(* Make a list of characters from a string, preserving order. *)
let explode s =
  let l = ref [] in
    for p = String.length s downto 1 do
      l := String.unsafe_get s (p - 1)::!l
    done;
    !l

(* Make a string from a list of characters, preserving order. *)
let implode l =
  let s = String.create (length l) in
    let rec list_loop x = function
       [] -> ()
     | i::t -> String.unsafe_set s x i; list_loop (x + 1) t
    in
      list_loop 0 l;
      s

(* String of character. *)
let string_of_char c =
  let s = String.create 1 in
    String.unsafe_set s 0 c;
    s

(* Long-integer function abbreviations *)
let i32ofi = Int32.of_int

let i32toi = Int32.to_int

let i32tof = Int32.to_float

let i32add = Int32.add

let i32sub = Int32.sub

let i32mul = Int32.mul

let i32div = Int32.div

let lsr32 = Int32.shift_right_logical

let lsl32 = Int32.shift_left

let lor32 = Int32.logor

let land32 = Int32.logand

let lnot32 = Int32.lognot

let lxor32 = Int32.logxor

let i32succ = Int32.succ

let i32pred = Int32.pred

let i32max = Pervasives.max

let i32min = Pervasives.min

let i64ofi = Int64.of_int

let i64toi = Int64.to_int

let i64tof = Int64.to_float

let i64add = Int64.add

let i64sub = Int64.sub

let i64mul = Int64.mul

let i64div = Int64.div

let lsr64 = Int64.shift_right_logical

let lsl64 = Int64.shift_left

let land64 = Int64.logand

let lor64 = Int64.logor

let lnot64 = Int64.lognot

let lxor64 = Int64.logxor

let i64succ = Int64.succ

let i64pred = Int64.pred

let i64max = Pervasives.max

let i64min = Pervasives.min

let i32ofi64 = Int64.to_int32

let i64ofi32 = Int64.of_int32 

(* Sign extension for integer of number of bits l. *)
let sign_extend l n =
  let shift = Nativeint.size - 1 - l in
    (n lsl shift) asr shift

(* Set each element of array [a] to value [v]. *)
let set_array a v =
  Array.fill a 0 (Array.length a) v

(* Evaluate [v ()], evaluate and ignore [f ()], return [v ()], in that order. *)
let do_return v f =
  let r = v () in ignore (f ()); r

(* Call [f ()] some number of times. *)
let rec do_many f = function
  | n when n < 0 -> raise (Invalid_argument "do_many")
  | 0 -> ()
  | n -> f (); do_many f (n - 1) 

(* Interleave an element among a list, so that [interleave 0 [1; 2; 3]]
yields [[1; 0; 2; 0; 3]]. An empty or singleton list is unchanged. *)
let interleave e l =
  let rec interleave_inner result elt = function
    | [] -> rev result
    | [e] -> interleave_inner (e::result) elt []
    | h::t -> interleave_inner (elt::h::result) elt t
  in
    interleave_inner [] e l

(* Interleave two same-length lists together, taking from the first list first.
*)
let interleave_lists a b =
  let rec interleave_lists_inner r a b =
    match a, b with
    | [], [] -> rev r
    | h::t, h'::t' -> interleave_lists_inner (h'::h::r) t t'
    | _ -> raise (Invalid_argument "interleave_lists")
  in
    interleave_lists_inner [] a b

(* Cons on list references *)
let ( =| ) r e =
  r := e::!r

(* Append on list references *)
let ( =@ ) r l =
  r := l @ !r

(* Functions on characters. *)
let isdigit = function
  | x when x >= '0' && x <= '9' -> true
  | _ -> false

(* Abbreviation. *)
let toint x = int_of_float x

(* Invert a predicate. *)
let notpred f =
  function e -> not (f e)

(* Prefix equality *)
let eq = ( = )

let neq = ( <> )

(* Map on the individual (inner) elements of a list of lists *)
let map_lol f =
  map (map f)

(* Raise [x] to the power [i]. *)
let rec pow i x =
  match i with
  | 0 -> 1
  | 1 -> x
  | i -> pow (i / 2) (x * x) * (if i mod 2 = 0 then 1 else x)

(* Dictionaries implemented as association lists *)

(* Look something up in a dictionary. *)
let rec lookup k' = function
  | [] -> None
  | (k, v)::t -> if k = k' then Some v else lookup k' t

(* Same, but no [option] type. *)
let rec lookup_failnull k' = function
  | [] -> dpr "e"; raise Not_found
  | (k, v)::t -> if k = k' then v else lookup_failnull k' t

(* Add something to a dictionary, replacing it if it's already there. *)
let add k' v d =
  let rec add_inner r k' v = function
    | [] -> (k', v)::r
    | (k, _)::t when k = k' -> r @ ((k', v)::t)
    | h::t -> add_inner (h::r) k' v t
  in
    add_inner [] k' v d

(* Replace something in a dictionary, failing if it doesn't exist. *)
let replace k' v l =
  let rec replace_inner r k' v = function
    | [] -> dpr "f"; raise Not_found
    | (k, _)::t when k = k' -> List.rev_append r ((k', v)::t)
    | h::t -> replace_inner (h::r) k' v t
  in
    replace_inner [] k' v l

(* Remove something from a dictionary. *)
let remove k' l =
  let rec remove_inner r k' = function
    | [] -> r
    | (k, _)::t when k = k' -> List.rev_append r t
    | h::t -> remove_inner (h::r) k' t
  in
    remove_inner [] k' l

(* Merge two dictionaries, prefering elements in the second in the case of
clashes. *)
let rec mergedict d = function
  | [] -> d
  | (k, v)::es -> mergedict (add k v d) es

(* An infix operator for the composition of functions. *)
let ( <| ) a b = a b

(* Opposite version of [@], the reverse append. *)
let ( @@ ) a b = b @ a

(* In order to return pairs of list from recursive functions without recourse
to accumulating arguments. *)
let conspair ((x, y), (xs, ys)) = x::xs, y::ys

(* The same with options determining whether or not each element is included in
the output list. *)
let conspairopt ((xo, yo), (xs, ys)) =
  (match xo with None -> xs | Some x -> x::xs),
  (match yo with None -> ys | Some y -> y::ys)

(* Make consecutive elements of an even-length list into a list of pairs. *)
let pairs_of_list l =
  let rec pairs_of_list_inner r = function
    | [] -> rev r
    | [_] -> raise (Invalid_argument "pairs_of_list")
    | h::h'::t -> pairs_of_list_inner ((h, h')::r) t
  in
    pairs_of_list_inner [] l

(* Return a list identical to the input but with any item true under predicate
[p] replaced with [o]. *)
let replaceinlist p o l =
  let rec replaceinlist_inner r p o = function
    | [] -> rev r
    | h::t ->
        if p h
          then replaceinlist_inner (o::r) p o t
          else replaceinlist_inner (h::r) p o t
  in
    replaceinlist_inner [] p o l 

(* Produce a list of overlapping pairs of elements in a list in order, producing
the empty list if on singleton input. *)
let pairs l =
  let rec pairs_inner r = function
    | [] | [_] -> rev r
    | a::b::rest -> pairs_inner ((a, b)::r) (b::rest)
  in
    pairs_inner [] l

(* Predicate to test if [x] is a member of a list. *)
let mem = List.mem

(* The same, with reversed arguments. *)
let mem' l x = mem x l

(* Return the set of distinct  elements in a list. Does not preserve order. *)
let setify_simple l =
  let rec setify_inner r = function
    | [] -> r
    | h::t ->
        if mem h t
          then setify_inner r t
          else setify_inner (h::r) t
  in
    setify_inner [] l

(* The same, preserving the order of the first occurance of each distinct
element in the input list. FIXME: This is still n^2, of course. How to improve?
*)
let setify_preserving_order l =
  setify_simple (rev l)

let rec sorted_setify prev = function
   [] -> rev prev
 | [x] -> rev (x::prev)
 | a::b::t when a = b -> sorted_setify prev (b::t)
 | h::t -> sorted_setify (h::prev) t

let setify l =
  sorted_setify [] (List.sort compare l)

(* Remove all elts of l' from l if l, l' sets. *)
let setminus l l' =
  let rec setminus_inner r l l' =
    match l with
    | [] -> r
    | h::t ->
        if mem h l'
          then setminus_inner r t l'
          else setminus_inner (h::r) t l'
  in
    setminus_inner [] l l' 

let setminus_preserving_order l l' =
  rev (setminus l l')

(* Return a list of the heads of a list of lists. *)
let heads l =
  let rec heads_inner r = function
    | [] -> rev r
    | h::t -> heads_inner (hd h::r) t
  in
    heads_inner [] l

(* Return a list of the tails of a list of lists, failing if any of them are
the empty list. *)
let tails l =
  let rec tails_inner r = function
    | [] -> rev r
    | h::t -> tails_inner (tl h::r) t
  in
    tails_inner [] l
 
(* Take a list of lists of equal length, and turn into a list of lists, the
first containing all the first elements of the original lists, the second the
second, and so on. *)
let zipn l =
  let rec zipn_inner r = function
    | [] | []::_ -> rev r
    | l -> zipn_inner (heads l::r) (tails l)
  in
    zipn_inner [] l

(* Remove the second, fourth etc elements from a list, saving the last element
(if of even length) e.g [drop_evens [1;2;3;4;5;6] is [1;3;5;6]]. *) 
let drop_evens l =
  let rec drop_evens_inner r = function
    | h::_::h''::t -> drop_evens_inner (h::r) (h''::t)
    | h::h'::[] -> rev (h'::h::r)
    | [x] -> rev (x::r)
    | _ -> rev r
  in
    drop_evens_inner [] l

(* Same, but don't save the last even one. *)
let really_drop_evens l =
  let rec really_drop_evens_inner r = function
    | [] -> rev r
    | [h] -> really_drop_evens_inner (h::r) []
    | h::_::more -> really_drop_evens_inner (h::r) more
  in
    really_drop_evens_inner [] l

(* Remove the first, third etc. The last odd element is not saved. e.g
[drop_odds [1;2;3;4;5;6;7] is [2;4;6]]. *)
let drop_odds l =
  let rec drop_odds_inner r = function
    | _::h'::t -> drop_odds_inner (h'::r) t
    | _ -> rev r
  in
    drop_odds_inner [] l

(* tl but silent failure. *)
let tail_no_fail = function
  | [] -> []
  | _::t -> t

(* Couple the elements of a list [l] using function [f]. For instance,
[couple ( + ) [[1; 3; 5]]] $\Longrightarrow$ [[4; 8]]. The two elements
are applied to [f] in the order in which they appear in the input list. *)
let couple f l =
  let rec couple_inner r f = function
    | x::x'::xs -> couple_inner (f x x'::r) f (x'::xs)
    | _ -> rev r
  in
    couple_inner [] f l
 
(* As above, but an extra function [g] is applied to any last (odd) element. *)
let couple_ext f g l =
  let rec couple_ext_inner r f g = function
    | x::x'::xs -> couple_ext_inner (f x x'::r) f g (x'::xs)
    | x::[] -> couple_ext_inner (g x::r) f g []
    | [] -> rev r
  in
    couple_ext_inner [] f g l

(* Apply [couple] repeatedly until only one element remains. Return that
element. *)
let rec couple_reduce f = function
  | [] -> raise (Invalid_argument "Utility.couple_reduce")
  | [a] -> a
  | l -> couple_reduce f (couple f l)

(* A similar function to [couple], but the coupling is non-overlapping. *)
let pair f l =
  let rec pair_inner r f = function
    | [] -> rev r
    | [a] -> pair_inner (a::r) f []
    | a::b::t -> pair_inner (f a b::r) f t
  in
    pair_inner [] f l

(* A version of [pair] which adds a unary function for the singleton, much
like [couple_ext]. *)
let pair_ext f g l =
  let rec pair_ext_inner r f g = function
    | [] -> rev r
    | [a] -> pair_ext_inner (g a::r) f g []
    | a::b::t -> pair_ext_inner (f a b::r) f g t
  in
    pair_ext_inner [] f g l

(* As [couple_reduce] is to [couple], so this is to [pair]. *)
let rec pair_reduce f = function
  | [] -> raise (Invalid_argument "Utility.pair_reduce")
  | [a] -> a
  | l -> pair_reduce f (pair f l)

(* [List.filter] has a confusing name, so we define [keep] and [lose] to avoid
error. *)
let keep = List.filter

let rec lose_inner prev p = function
  | [] -> rev prev
  | h::t ->
    if p h
      then lose_inner prev p t
      else lose_inner (h::prev) p t

let lose p = lose_inner [] p

(* Make a list of length [n] with each element equal to [x]. *)
let many x ~n =
  Array.to_list (Array.make n x)

(* A version where we need to apply unit each time, for instance when producing
a list of random numbers. Result is ordered. *)
let manyunique f n =
  let rec manyunique_inner r f n =
    if n = 0
      then rev r
      else manyunique_inner (f ()::r) f (n - 1)
  in
    manyunique_inner [] f n

(* Take [n] elements from the front of a list [l], returning them in order. *)
let take l n =
  if n < 0 then raise (Invalid_argument "Utility.take") else
  let rec take_inner r l n =
    if n = 0 then rev r else
      match l with
      | [] -> raise (Invalid_argument "Utility.take")
      | h::t -> take_inner (h::r) t (n - 1)
  in
    take_inner [] l n

let take' n l = take l n

(* Same, but order is reversed *)
let takewhile_reverse p l =
  let rec takewhile_reverse_inner r p = function
    | [] -> r
    | h::t -> if p h then takewhile_reverse_inner (h::r) p t else r
  in
    takewhile_reverse_inner [] p l

(* Take from the list [l] while the predicate [p] is true. *)
let takewhile p l =
  let rec takewhile_inner r p l =
    match l with
    | [] -> rev r
    | h::t -> if p h then takewhile_inner (h::r) p t else rev r
  in
    takewhile_inner [] p l

(* Drop [n] elements from the front of a list, returning the remainder in
order. *)
let rec drop_inner n = function
  | [] -> raise (Invalid_argument "drop")
  | _::t -> if n = 1 then t else drop_inner (n - 1) t

let drop l n =
  if n < 0 then raise (Invalid_argument "drop") else
  if n = 0 then l else
    drop_inner n l

let drop' n l = drop l n

let rec dropwhile p = function
  | [] -> []
  | h::t -> if p h then dropwhile p t else (h::t)

(* Split a list [l] into two parts, the first part containing [n] elements. *)
let cleave l n =
  let rec cleave_inner l left n =
    if n = 0 then rev left, l else
      match l with
      | [] -> raise (Invalid_argument "cleave: not enough elements")
      | _  -> cleave_inner (tl l) (hd l::left) (n - 1)
  in
    if n < 0
      then raise (Invalid_argument "cleave: negative argument")
      else cleave_inner l [] n

(* Returns elements for which p is true, until one is not, paired with the
remaining list. The same as [takewhile p l], [dropwhile p l], but requiring
only one pass over the list. *)
let cleavewhile p l =
  let rec cleavewhile_inner p l elts =
    match l with
    | [] -> rev elts, []
    | e::es ->
        if p e
          then cleavewhile_inner p es (e::elts)
          else rev elts, l
  in
    cleavewhile_inner p l []

(* The same, faster, but output lists are unordered. *)
let cleavewhile_unordered p l =
  let rec cleavewhile_unordered_inner p l elts =
    match l with
    | [] -> elts, []
    | e::es ->
        if p e
          then cleavewhile_unordered_inner p es (e::elts)
          else elts, l
  in
    cleavewhile_unordered_inner p l []

(* Isolate a central section of a list, from the first element after the element
for which predicate [p] is true, to the element before [p'] is first true. *)
let isolate p p' l =
  let _, during_and_after = cleavewhile (notpred p) l in
    match during_and_after with
    | [] -> []
    | _::t -> fst (cleavewhile (notpred p') t)

(* Collate a list into a list of lists based upon a comparison function by which
it has already been sorted. e.g [collate [1; 2; 2; 3; 3]] calculates
[[[1]; [2;2]; [3;3]]]. *)
let collate cmp l =
  let rec collate_inner prev = function
    | [] -> rev prev
    | h::t ->
        let x, y = cleavewhile (fun a -> cmp h a = 0) (h::t) in
          collate_inner (x::prev) y
  in
    collate_inner [] l

(* Split a list into some lists of length [n] (and possibly a final one of
length < [n]). *)
let splitinto n l =
  let rec splitinto_inner a n l len =
    match l with [] -> rev a | _ ->
      if len < n then rev (l::a) else
        let h, t = cleave l n in
          splitinto_inner (h::a) n t (len - n)
  in
    splitinto_inner [] n l (length l)

(* Non-tail recursive version, for use when [n] is small and fixed. *)
let rec takeatmost n l =
  match l with
  | h::t when n > 0 -> h :: takeatmost (n - 1) t
  | _ -> []

let rec dropatmost n l =
  match l with
  | _::t when n > 0 -> dropatmost (n - 1) t
  | l -> l

let rec splitinto_small n l =
  match l with
  | [] -> []
  | _ ->
      let first = takeatmost n l in
        first :: splitinto_small n (dropatmost n l)

(* Split a list [l] at the given points. Point 1 means after the first element.
*)
let rec splitat_inner prev l = function
  | [] -> begin match l with [] -> rev prev | _ -> rev (l::prev) end
  | h::t ->
      let this, rest = cleave l h in
        splitat_inner (this::prev) rest t

let splitat points l =
  splitat_inner [] l (couple (fun a b -> b - a) (0::points)) 

(* Select the nth element in a list (first is element 1) *)
let select n l =
  try hd (drop l (n - 1)) with
    Invalid_argument _ (*"drop"*)
  | Failure _ (*"hd"*) -> raise (Invalid_argument "select")

(* Replace the nth element of a list (first is element 1) *)
let rec replace_number_inner prev n e = function
  | [] -> rev prev
  | l::ls ->
      if n = 1
        then replace_number_inner (e::prev) (n - 1) e ls
        else replace_number_inner (l::prev) (n - 1) e ls

let replace_number n e l =
  replace_number_inner [] n e l

(* Simple list utilities. *)
let isnull = function [] -> true | _ -> false

let notnull = function [] -> false | _ -> true

(* Find the last element of a list. *)
let rec last = function
  | [] -> raise (Invalid_argument "Utility.last")
  | x::[] -> x
  | _::xs -> last xs

(* Produce a list containing all but the last element of a list *)
let all_but_last = function
  | [] | [_] -> []
  | l -> rev (tl (rev l))

(* Find the first and last element of a list. If the list has one element, that
is returned twice. *)
let extremes = function
  | [] -> raise (Invalid_argument "Utility.extremes")
  | x::[] -> x, x
  | x::xs -> x, last xs

(* Return the first, middle and last elements of a list which has length at
least two. *)
let extremes_and_middle = function
  | [] | [_] ->
      raise (Invalid_argument "extremes_and_middle")
  | h::t ->
      let m, l = cleave t (length t - 1) in
         h, m, hd l
 
(* Set a boolean reference. *)
let set r =
  r := true

(* Clear a boolean reference. *)
let clear r =
  r := false

(* Change the value of a boolean reference. *)
let flip r =
  r := not !r

(* Increment and decrement integer references [r] by an integer [n]. *)
let ( += ) r n =
  r := !r + n

let ( -= ) r n =
  r := !r - n 

let ( /= ) r n = 
  r := !r / n

let ( *= ) r n =
  r := !r * n

(* Similar functions on floating-point references. *)
let ( +.= ) r n =
  r := !r +. n

let ( -.= ) r n =
  r := !r -. n

let ( /.= ) r n =
  r := !r /. n

let ( *.= ) r n =
  r := !r *. n

(* Vectors in two dimensions. *)
type vector = float * float

(* Make a vector from a point [(x0, y0)] to a point [(x1, y1)]. *)
let mkvector (x0, y0) (x1, y1) = x1 -. x0, y1 -. y0

(* Invert a vector. *)
let invert (a, b) = -.a, -.b

(* Offset a point [(px, py)] by a vector [(x, y)]. *)
let offset_point (x, y) (px, py) = px +. x, py +. y

(* Find the vector pi / 2 anticlockwise from the given one. *)
let perpendicular (a, b) = -.b, a

(* Square a number *)
let sqr x = x *. x

(* Find the length of a vector. *)
let veclength (x, y) =
  sqrt (sqr x +. sqr y)

(* Scale a vector to a length [l]. *)
let scalevectolength l (a, b) =
  let currentlength = veclength (a, b) in
    if currentlength = 0. then (a, b) else
      let factor = l /. currentlength in
        a *. factor, b *. factor

(* Make a unit vector from [s] to [e] *)
let mkunitvector s e =
  scalevectolength 1. (mkvector s e)

(* Find the point equidistant between two others. *)
let between (x, y) (x', y') =
  (x +. x') /. 2., (y +. y') /. 2.

(* The cartesian distance between two points. *)
let distance_between (px, py) (px', py') =
  sqrt (sqr (px -. px') +. sqr (py' -. py))

(* The largest power of two by which [n] is exactly divisible. *)
let largest_pow2_divisible n =
  let rec s test n =
    if n mod test = 0 then s (test * 2) n
    else test / 2
  in
    s 1 n

(* Find the largest power of two smaller or equal to an integer [t]. *)
let pow2lt t =
  let rec pow2lt_i target current =
    if current * 2 > target
      then current
      else pow2lt_i target (current * 2)
  in
    pow2lt_i t 1

(* Find the largest power of two greater or equal to an integer [t]. *)
let pow2gt t =
  let lt = pow2lt t in
    if lt = t then t else lt * 2

(* Find the integer base two logarithm of a number. *)
let log2of t =
  let rec log2of_i target num =
    if num * 2 > target
      then 0
      else let n = log2of_i target (num * 2) in n + 1
  in
    log2of_i t 1

(* Integer compare function --- saves the cost of polymorphic comparisons. *)
let compare_i (a : int) b =
  if a < b then -1 else if a > b then 1 else 0

(* Reverse comparison *)
let rev_compare a b =
  -compare a b
 
(* The integer range between $[s..e]$ inclusive. *)
let ilist s e =
  if e < s then raise (Invalid_argument "Utility.ilist") else
    let nums = ref [] in
      let rec ilist s e =
        if s = e
          then nums =| e
          else (nums =| s; ilist (s + 1) e)
      in
        ilist s e;
        rev !nums

(* Same, but return null list for ilist x x rather than [x] *)
let ilist_null s e =
  if s = e then [] else ilist s e

(* Same, but upon failure just return null. *)
let ilist_fail_null s e =
  if s > e then [] else ilist_null s e

(* A common case: Make indexes for a list *)
let indx l =
  if l = [] then [] else ilist 1 (length l)

(* Same zero-indexed. *)
let indx0 l =
  if l = [] then [] else ilist 0 (length l - 1)

(* Same, n-indexed. *)
let indxn n l =
  if l = [] then [] else ilist n (n + length l - 1)
 
(* Even/odd predicates. Zero is considered even, -1 odd, -2 even etc. *)
let even x = x mod 2 = 0

let odd = notpred even

(* Exclusive Or of [a] and [b]. *)
let ( |&| ) a b =
  (a || b) && not (a && b)

(* The identity function. *)
let ident x = x

(* An array analog of [List.iter2].*)
let array_iter2 f a b =
  if Array.length a = Array.length b then
    if Array.length a = 0 then () else
      for x = 0 to (Array.length a) - 1 do
        f (Array.get a x) (Array.get b x)
      done
  else
    raise (Invalid_argument "Utility.array_iter2")
   
let array_map2 f a b =
  if Array.length a = Array.length b then
    Array.init (Array.length a) (function i -> f a.(i) b.(i))
  else
    raise (Invalid_argument "Utility.array_map2")
 
(* Some simple functions for working with the [option] type. *)
let some = function None -> false | _ -> true

let none = function None -> true | _ -> false

let unopt = function
  | Some x -> x
  | None -> failwith "unopt"

let rec losenones prev = function
  | [] -> rev prev
  | None::t -> losenones prev t
  | Some h::t -> losenones (h::prev) t 

let losenones l = losenones [] l

let option_map f l =
  losenones (map f l)

let option_map2 f a b =
  losenones (map2 f a b)

(* Integer-specialised minimum and maximum functions for speed, overriding
\emph{Pervasives.min} and \emph{Pervasives.max}. *)
let min (a : int) b = if a < b then a else b
let max (a : int) b = if a > b then a else b

(* Floating point ones. *)
let fmin (a : float) b = if a < b then a else b
let fmax (a : float) b = if a > b then a else b

let fabs x = abs_float x

(* The union of two rectangles, each defined by its minimum and maximum
coordinates *)
let box_union (xmin0, xmax0, ymin0, ymax0) (xmin1, xmax1, ymin1, ymax1) =
  min xmin0 xmin1, max xmax0 xmax1, min ymin0 ymin1, max ymax0 ymax1

(* The union of two rectangles, each defined by its minimum and maximum
coordinates *)
let box_union_float (xmin0, xmax0, ymin0, ymax0) (xmin1, xmax1, ymin1, ymax1) =
  fmin xmin0 xmin1, fmax xmax0 xmax1, fmin ymin0 ymin1, fmax ymax0 ymax1

(* The intersection rectangle of two rectangles defined by integers. [x0, y0]
etc refer to the top left, [x1, y1] etc. to the bottom right. *)
let box_overlap ax0 ay0 ax1 ay1 bx0 by0 bx1 by1 =
  if ax0 > bx1 || ay0 > by1 || ax1 < bx0 || ay1 < by0
    then None
    else Some (max ax0 bx0, max ay0 by0, min ax1 bx1, min ay1 by1)

(* The same for floating point coordinates. *)
let box_overlap_float ax0 ay0 ax1 ay1 bx0 by0 bx1 by1 =
  if ax0 > bx1 || ay0 > by1 || ax1 < bx0 || ay1 < by0
    then None
    else Some (fmax ax0 bx0, fmax ay0 by0, fmin ax1 bx1, fmin ay1 by1)
    
(* Apply a function [f] [n] times to initial argument [arg]. *)
let rec applyn f n arg =
  if n = 0 then arg else applyn f (n - 1) (f arg)

(* The type of binary trees. *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree

(* Define pi. *)
let pi = 4. *. atan 1.

(* Define sqrt 2. *)
let root2 = sqrt 2.

(* Radians of degrees. *)
let rad_of_deg a = a *. pi /. 180.

(* Degrees of radians. *)
let deg_of_rad a = a *. 180. /. pi

(* Constant boolean predicates *)
let always _ = true
let never _ = false

(* A null hash table. *)
let null_hash () =
  Hashtbl.create 0

let tryfind table k =
  try
    Some (Hashtbl.find table k)
  with
    Not_found -> None

(* Extract all (key, value) pairs from a hash table. *)
let list_of_hashtbl t =
  let contents = ref [] in
    Hashtbl.iter
      (fun k v -> contents =| (k, v))
      t;
  !contents

(* Build a hashtable from a dictionary *)
let hashtable_of_dictionary pairs =
  let table = Hashtbl.create (length pairs * 2) in
    iter (fun (k, v) -> Hashtbl.add table k v) pairs;
    table

let hashset_of_list l =
  let table = Hashtbl.create (length l * 2) in
    iter (fun k -> Hashtbl.add table k ()) l;
    table

(* Round a number. *)
let round x =
  let c = ceil x in let f = floor x in
    if c -. x <= x -. f then c else f

let iround x =
  int_of_float (round x)

(* Render a float normal by replacing anything abnormal by 0. *)
let safe_float f =
  match classify_float f with
  | FP_nan | FP_infinite | FP_zero | FP_subnormal -> 0.
  | _ -> f

(* Build a tuple *)
let tuple x y = x, y

(* Make a unit function. *)
let mkunit f x = fun () -> f x

(* Swap two elements of an array. *)
let swap a i j =
  let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t

(* Print floats, integers or int32 values with spaces between them. *)
let print_floats fs =
  iter (fun x -> print_float x; print_string " ") fs;
  print_newline ()

let print_ints is =
  iter (fun x -> print_int x; print_string " ") is;
  print_newline ()

let print_int32s is =
  iter (fun x -> Printf.printf "%li " x) is;
  print_newline ()

let leafnames_of_dir d =
  Array.to_list (Sys.readdir d)

(* Roman numerals. *)
let roman_vals =
  [(900, "CM"); (500, "D"); (400, "CD"); (100, "C"); (100, "C"); (100, "C");
  (90, "XC"); (50, "L"); (40, "XL"); (10, "X"); (10, "X"); (10, "X");
  (9, "IX"); (5, "V"); (4, "IV"); (1, "I"); (1, "I"); (1, "I")]

let rec roman n =
  if n < 1 then ""
  else if n >= 1000 then implode (many 'M' (n / 1000)) ^ roman (n mod 1000)
  else 
    let rec roman_recurse n = function
    | [] -> ""
    | (n', s)::t ->
        if n >= n'
          then s ^ roman_recurse (n - n') t
          else roman_recurse n t
    in
      assert (n > 0 && n < 1000);
      roman_recurse n roman_vals

let roman_upper = roman

let roman_lower n = String.lowercase (roman n)

let memoize f =
  let result = ref None in
    fun () ->
      match !result with
      | Some thing -> thing
      | None -> result := Some (f ()); unopt !result

