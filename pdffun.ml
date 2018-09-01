open Pdfutil
open Pdfio

(* Types *)

(* Postscript calculator functions. *)
type calculator =
  | If of calculator list
  | IfElse of calculator list * calculator list
  | Bool of bool
  | Float of float
  | Int of int32
  | Abs | Add | Atan | Ceiling | Cos | Cvi | Cvr
  | Div | Exp | Floor | Idiv | Ln | Log | Mod
  | Mul | Neg | Round | Sin | Sqrt | Sub | Truncate
  | And | Bitshift | Eq | Ge | Gt | Le | Lt | Ne | Not
  | Or | Xor | Copy | Exch | Pop | Dup | Index | Roll

(* Sampled functions. *)
type sampled =
  {size : int list; 
   order : int;
   encode : float list;
   decode : float list;
   bps : int;
   samples : int32 array}

(* Interpolation functions. *)
and interpolation =
  {c0 : float list;
   c1 : float list;
   n : float}

(* Stitching functions. *)
and stitching =
  {functions : t list;
   bounds : float list;
   stitch_encode : float list}

(* Collect the above types into a single type. *)
and pdf_fun_kind =
  | Interpolation of interpolation
  | Stitching of stitching
  | Sampled of sampled
  | Calculator of calculator list

(* Main type. *)
and t =
  {func : pdf_fun_kind;
   domain : float list;
   range : float list option}

(* Printing functions *)

(* Build a string of a calculator function. For debug only, since could exceed
string length limit. *)
let rec string_of_calculator_inner = function
  | If l ->
      string_of_calculator l ^ " if"
  | IfElse (l, l') ->
      string_of_calculator l ^ " " ^ string_of_calculator l' ^ " ifelse"
  | Bool true -> "true"
  | Bool false -> "false"
  | Float f -> string_of_float f
  | Int i -> Int32.to_string i
  | Abs -> "abs" | Add -> "add" | Atan -> "atan" | Ceiling -> "ceiling"
  | Cos -> "cos" | Cvi -> "cvi" | Cvr -> "cvr" | Div -> "div"
  | Exp -> "exp" | Floor -> "floor" | Idiv -> "idiv" | Ln -> "ln"
  | Log -> "log" | Mod -> "mod" | Mul -> "mul" | Neg -> "neg"
  | Round -> "round" | Sin -> "sin" | Sqrt -> "sqrt" | Sub -> "sub"
  | Truncate -> "truncate" | And -> "and" | Bitshift -> "bitshift"
  | Eq -> "eq" | Ge -> "ge" | Gt -> "gt" | Le -> "le" | Lt -> "lt"
  | Ne -> "ne" | Not -> "not" | Or -> "or" | Xor -> "xor"
  | Copy -> "copy" | Exch -> "exch" | Pop -> "pop" | Dup -> "dup"
  | Index -> "index" | Roll -> "roll"

and string_of_calculator cs =
  let ops =
    fold_right ( ^ ) (interleave " " (map string_of_calculator_inner cs)) ""
  in
    "{" ^ ops ^ "}"

(* Print a function out for debug. *)
let rec print_function f =
  print_string "Domain...\n";
  print_floats f.domain;
  begin match f.range with
  | None -> print_string "null range\n"
  | Some values -> print_floats values
  end;
  match f.func with
  | Sampled s ->
      print_string "Sampled\n";
      print_string "size: ";
      print_ints s.size;
      print_string "order: ";
      print_int s.order;
      print_string "\nencode:\n";
      print_floats s.encode;
      print_string "decode:\n";
      print_floats s.decode;
      print_string "original bits per sample..\n";
      print_int s.bps;
      print_string "\ndata:\n";
      print_int32s (Array.to_list s.samples)
  | Interpolation i ->
      print_string "Interpolation\n";
      print_string "C0:\n";
      print_floats i.c0;
      print_string "C1:\n";
      print_floats i.c1;
      Printf.printf "n = %f\n" i.n;
  | Stitching s ->
      print_string "Stitching\n";
      iter print_function s.functions;
      print_string "Bounds:\n";
      print_floats s.bounds;
      print_string "Encode:\n";
      print_floats s.stitch_encode;
  | Calculator c ->
      print_string "Calculator:\n";
      print_string (string_of_calculator c)

(* Parsing Calculator Functions *)
let keyword_of_string = function
  | "abs" -> Abs | "add" -> Add | "atan" -> Atan | "ceiling" -> Ceiling
  | "cos" -> Cos | "cvr" -> Cvr | "div" -> Div | "exp" -> Exp
  | "floor" -> Floor | "idiv" -> Idiv | "ln" -> Ln | "log" -> Log
  | "mod" -> Mod | "mul" -> Mul | "neg" -> Neg | "round" -> Round
  | "sin" -> Sin | "sqrt" -> Sqrt | "sub" -> Sub
  | "truncate" -> Truncate | "and" -> And | "bitshift" -> Bitshift
  | "eq" -> Eq | "ge" -> Ge | "gt" -> Gt | "le" -> Le | "lt" -> Lt
  | "ne" -> Ne | "not" -> Not | "or" -> Or | "xor" -> Xor
  | "copy" -> Copy | "exch" -> Exch | "pop" -> Pop
  | "dup" -> Dup | "index" -> Index | "roll" -> Roll
  | s ->
      Printf.eprintf "%s" ("Bad keyword " ^ s); assert false

let parse_calculator s =
  let lexemes =
    Pdfgenlex.lex (Pdfio.input_of_string s)
  in
    let rec strip_outer_braces = function
      | Pdfgenlex.LexName ("{" | "}")::t ->
          rev (strip_outer_braces (rev t))
      | x -> x
    and group_operators = function
      | [] -> []
      | Pdfgenlex.LexName "{"::t ->
          let ops, rest = cleavewhile (neq (Pdfgenlex.LexName "}")) t in
            ops::group_operators (tl rest)
      | h::t -> [h]::group_operators t
    and parse = function
      | [] -> []
      | l::l'::[Pdfgenlex.LexName "ifelse"]::t ->
          IfElse (procss l, procss l')::parse t
      | l::[Pdfgenlex.LexName "if"]::t -> If (procss l)::parse t
      | [Pdfgenlex.LexName "true"]::t -> Bool true::parse t
      | [Pdfgenlex.LexName "false"]::t -> Bool false::parse t
      | [Pdfgenlex.LexReal f]::t ->
          (* If the float is actually a whole number, convert to an Int. This
          will be accepted as a float (where any number is ok) and as an int
          (where an integer is required. Sometimes, buggy code has things
          like 1.000000 where 1 is required. *)
          (*(if f = floor f then Int (Int32.of_float f) else *)
          Float f::parse t
      | [Pdfgenlex.LexInt i]::t-> Int (i32ofi i)::parse t (* FIXME: range *)
      | [Pdfgenlex.LexName x]::t -> keyword_of_string x::parse t
      | h::_ -> (*i Printf.eprintf (Cgenlex.string_of_tokens h); i*) raise (Failure "Bad lexeme")
    and procss lexemes =
      try
        parse (group_operators (strip_outer_braces lexemes))
      with
      _ -> raise (Pdf.PDFError "Cannot parse Type 4 function")
    in
      procss lexemes

(* Parsing functions *)
let rec parse_function pdf f =
  let f = Pdf.direct pdf f in
    let getnum_direct o = Pdf.getnum (Pdf.direct pdf o) in
      let domain =
        match Pdf.lookup_fail "No /Domain" pdf "/Domain" f with
        | Pdf.Array ns -> map getnum_direct ns
        | _ -> raise (Pdf.PDFError "Bad /Domain")
      in let range =
        match Pdf.lookup_direct pdf "/Range" f with
        | Some (Pdf.Array ns) -> Some (map getnum_direct ns)
        | _ -> None
      in
      let func =
        match Pdf.lookup_fail "no /FunctionType" pdf "/FunctionType" f with
        | Pdf.Integer 0 ->
            let size =
              match Pdf.lookup_fail "no /Size (sampled fun)" pdf "/Size" f with
              | Pdf.Array ns ->
                  map
                    (function
                     | Pdf.Integer n -> n
                     | _ -> raise (Pdf.PDFError "bad /Size (sampled fun)"))
                    ns
              | _ -> raise (Pdf.PDFError "Bad /Size (sampled fun)")
            in
              let order =
                match Pdf.lookup_direct pdf "/Order" f with
                | Some (Pdf.Integer n) -> n
                | _ -> 1
              in let encode =
                match Pdf.lookup_direct pdf "/Encode" f with
                | Some (Pdf.Array ns) when length ns = 2 * length size ->
                    map Pdf.getnum ns
                | _ ->
                    interleave_lists
                      (many 0. (length size))
                      (map (fun x -> float (x - 1)) size)
              in let decode =
                match Pdf.lookup_direct pdf "/Decode" f with
                | Some (Pdf.Array ns) -> map Pdf.getnum ns
                | _ ->
                    match range with
                    | Some r -> r
                    | None -> raise (Pdf.PDFError "No /Range")
              in
                let bitspersample =
                  match
                    Pdf.lookup_fail "no /BitsPerSample" pdf "/BitsPerSample" f
                  with
                  | Pdf.Integer i -> i
                  | _ -> raise (Pdf.PDFError "Bad /BitsPerSample")
                in
                  let data =
                    Pdfcodec.decode_pdfstream pdf f; 
                    let samples =
                      fold_left ( * ) 1 size * (length decode / 2)
                    in let bitstream =
                      match f with
                      | Pdf.Stream {contents = _, Pdf.Got data} ->
                          Pdfio.bitbytes_of_input (Pdfio.input_of_bytes data)
                      | _ -> assert false

                    in
                      let data = Array.make samples 1l in
                        for i = 0 to Array.length data - 1 do
                          data.(i) <- Pdfio.getval_32 bitstream bitspersample
                        done;
                        data
                  in
                    Sampled
                     {size = size;
                      order = order;
                      encode = encode;
                      decode = decode;
                      bps = bitspersample;
                      samples = data}
        | Pdf.Integer 2 ->
            let c0 =
              match Pdf.lookup_direct pdf "/C0" f with
              | Some (Pdf.Array ns) -> map getnum_direct ns
              | _ -> [0.]
            in let c1 =
              match Pdf.lookup_direct pdf "/C1" f with
              | Some (Pdf.Array ns) -> map getnum_direct ns
              | _ -> [1.]
            in let n =
              Pdf.getnum (Pdf.lookup_fail "No /N in Type 2 fun" pdf "/N" f)
            in
              Interpolation {c0 = c0; c1 = c1; n = n}
        | Pdf.Integer 3 ->
            let functions =
              match Pdf.lookup_fail "no /Functions" pdf "/Functions" f with
              | Pdf.Array fs -> fs
              | _ -> raise (Pdf.PDFError "Bad /Functions")
            in let bounds =
              match Pdf.lookup_fail "no /Bounds" pdf "/Bounds" f with
              | Pdf.Array fs -> fs
              | _ -> raise (Pdf.PDFError "Bad /Bounds")
            in let encode =
              match Pdf.lookup_fail "no /Encode" pdf "/Encode" f with
              | Pdf.Array fs -> fs
              | _ -> raise (Pdf.PDFError "Bad /Bounds")
            in
              Stitching
                {functions = map (parse_function pdf) functions;
                 bounds = map getnum_direct bounds;
                 stitch_encode = map getnum_direct encode}
        | Pdf.Integer 4 ->
            (* Read contents of stream, build string, parse. *)
            Pdfcodec.decode_pdfstream pdf f;
            begin match f with
            | Pdf.Stream {contents = _, Pdf.Got data} ->
                Calculator (parse_calculator (string_of_bytes data))
            | _ -> raise (Pdf.PDFError "This is not a function")
            end
        | _ -> raise (Pdf.PDFError "Unknown function type")
      in
        {domain = domain; range = range; func = func}

(* Evaluating Sampled Functions *)

(* Inappropriate inputs have been given to a function. *)
exception BadFunctionEvaluation of string

let interpolate x xmin xmax ymin ymax =
  ymin +. ((x -. xmin) *. ((ymax -. ymin) /. (xmax -. xmin)))

(* Evaluate a sampled function. We support only linear interpolation and then
only sensibly for one-dimensional functions. Although results will be
produced for higher dimensions, the results will not be fully accurate. *)
let eval_function_sampled f s clamped_inputs =
  (* 1. Encode the input values *)
  (*i Printf.printf "eval_function_sampled: length of clamped_inputs %i input is
   * %f\n" (length clamped_inputs) (hd clamped_inputs); i*)
  let range =
    match f.range with
    | None -> raise (BadFunctionEvaluation "No Range")
    | Some r -> r
  in
    let d, d' = split (pairs_of_list f.domain)
    in let e, e' = split (pairs_of_list s.encode)
    in let dec, dec' = split (pairs_of_list s.decode)
    in let r, r' = split (pairs_of_list range) in
      let encoded_inputs =
        map5 interpolate clamped_inputs d d' e e'
      in
      (* 2. Clip to the size of the table dimension *)
      let clamped_encoded_inputs =
        map2
          (fun i s -> fmin (fmax i 0.) (float s -. 1.))
          encoded_inputs
          s.size
      in
        let read_table inputs =
          let vals_to_read = length range / 2 in
            let size = s.size in
            if length size <> length inputs then
              raise (BadFunctionEvaluation "Incompatible /Size with inputs");
            let pos =
              let multipliers =
                let inputs' = if length inputs = 1 then [] else ilist 1 (length inputs - 1) in
                1::
                map
                  (function x -> fold_left ( * ) 1 (take size x)) inputs'
                  (* (ilist_fail_null 1 (length inputs - 1)) *)
              in
                (*i Printf.printf "%i inputs and %i multipliers" (length inputs)
                (length multipliers); i*)
                fold_left ( + ) 0 (map2 ( * ) inputs multipliers) * vals_to_read
            in
              Array.to_list (Array.sub s.samples pos vals_to_read)
        in
          (* 3. Read values from table. For now, just linear iterpolation. *)
          let ceilings =
            map (fun x -> toint (ceil x)) clamped_encoded_inputs
          in let floors =
            map (fun x -> toint (floor x)) clamped_encoded_inputs
          in
            let outputs = 
              let ceiling_results = read_table ceilings
              in let floor_results = read_table floors in
                (*i Printf.printf "length of floors: %i, length of ceilings %i\n"
                (List.length ceiling_results) (List.length floor_results); i*)
                map2
                  (fun x y ->
                     Int32.to_float x /. 2. +. Int32.to_float y /. 2.)
                  ceiling_results
                  floor_results
            in
              (* 4. Decode output values *)
              let outputs_decoded =
                map5
                  interpolate
                  outputs
                  (many 0. (length outputs))
                  (many (2. ** float s.bps -. 1.) (length outputs))
                  dec dec'
              in
                map3 (fun x r r' -> fmin (fmax x r) r') outputs_decoded r r'

(* Evaluating Calculator Functions *)
let eval_function_calculator clamped_inputs ops =
  let s =
    ref (map (fun i -> Float i) (rev clamped_inputs))
  in let typecheck () =
    raise (BadFunctionEvaluation "Type error")
  in
    let rec getfloat () =
      match !s with
      | Int i::r -> s := r; i32tof i
      | Float f::r -> s := r; f
      | _ -> typecheck ()
    and getint () =
      match !s with
      | Int i::r -> s := r; i
      | _ -> typecheck ()
    and getfloats () =
      let x = getfloat () in x, getfloat ()
    and getints () =
      let x = getint () in x, getint ()
    in
      let rec eval k =
      (* flprint (string_of_calculator !s);
      Printf.printf "Now op %s\n\n" (string_of_calculator [k]); *)
      match k with
      | If l ->
          begin match !s with
          | Bool b::r -> s := r; if b then iter eval l
          | _ -> typecheck ()
          end
      | IfElse (l, l') ->
          begin match !s with
          | Bool b::r -> s := r; iter eval (if b then l else l')
          | _ -> typecheck ()
          end
      | (Bool _ | Float _ | Int _) as immediate ->
          s =| immediate
      | Abs ->
          begin match !s with
          | Float f::r -> s := Float (fabs f)::r
          | Int i::r ->
              let out =
                if i = Int32.min_int
                  then (Float (i32tof Int32.max_int))
                  else (Int (Int32.abs i))
              in
                s := out::r
          | _ -> typecheck ()
          end
      | Add ->
          begin match !s with
          | Int i::Int i'::r ->
              s := Int (i32add i i')::r (*r FIXME: Overflow to float *)
          | Int i::Float f::r
          | Float f::Int i::r ->
              s := Float (i32tof i +. f)::r
          | Float f::Float f'::r ->
              s := Float (f +. f')::r
          | _ -> typecheck ()
          end
      | Atan ->
          let num, den = getfloats () in
            let result = atan2 num den in
              s := Float result::tl (tl !s)
      | Ceiling ->
          begin match !s with
          | Float f::r -> s := Float (ceil f)::r
          | Int _::_ -> ()
          | _ -> typecheck ()
          end
      | Cos ->
          let f = getfloat () in
            s := Float (cos (rad_of_deg f))::!s
      | Cvi ->
          begin match !s with
          | Int _::r -> ()
          | Float f::r -> s := Int (Int32.of_float (floor f))::r
          | _ -> typecheck ()
          end
      | Cvr ->
          begin match !s with
          | Int i::r -> s := Float (i32tof i)::r
          | Float f::r -> ()
          | _ -> typecheck ()
          end
      | Div ->
          let n, n' = getfloats () in
            s := Float (n /. n')::!s
      | Exp ->
          let bse, exponent = getfloats () in
            s := Float (bse ** exponent)::!s
      | Floor ->
          begin match !s with
          | Int i::r -> ()
          | Float f::r -> s := Int (Int32.of_float (floor f))::r
          | _ -> typecheck ()
          end
      | Idiv ->
          let i, i' = getints () in
            s := Int (i32div i i')::!s
      | Ln ->
          let f = getfloat () in
            s := Float (log f)::!s
      | Log ->
          let f = getfloat () in
            s := Float (log10 f)::!s
      | Mod ->
          let i, i' = getints () in
            s := Int (Int32.rem i i')::!s
      | Mul ->
          begin match !s with
          | Int i::Int i'::r ->
              s := Int (i32mul i i')::r (*r FIXME: Overflow to float *)
          | Int i::Float f::r
          | Float f::Int i::r ->
              s := Float (i32tof i *. f)::r
          | Float f::Float f'::r ->
              s := Float (f *. f')::r
          | _ -> typecheck ()
          end
      | Neg ->
          begin match !s with
          | Float f::r -> s := Float (-.f)::r
          | Int i::r ->
              let out =
                if i = Int32.min_int
                  then (Float (i32tof Int32.max_int))
                  else (Int (Int32.neg i))
              in
                s := out::r
          | _ -> typecheck ()
          end
      | Round ->
          begin match !s with
          | Int _::_ -> ()
          | Float f::r -> s := Int (Int32.of_float (round f))::r
          | _ -> typecheck ()
          end
      | Sin ->
          let f = getfloat () in
            s := Float (sin (rad_of_deg f))::!s
      | Sqrt ->
          let f = getfloat () in
            s := Float (sqrt f)::!s
      | Sub ->
          begin match !s with
          | Int i::Int i'::r ->
              s := Int (i32sub i' i)::r (*r \FIXME{Overflow to float} *)
          | Int i::Float f::r ->
              s := Float (f -. i32tof i)::r
          | Float f::Int i::r ->
              s := Float (i32tof i -. f)::r
          | Float f::Float f'::r ->
              s := Float (f' -. f)::r
          | _ -> typecheck ()
          end
      | Truncate ->
          begin match !s with
          | Int _::_ -> ()
          | Float f::r -> s := Int (i32ofi (toint f))::r
          | _ -> typecheck ()
          end
      | And ->
          begin match !s with
          | Int i::Int i'::r ->
              s := Int (Int32.logand i i')::r
          | Bool b::Bool b'::r ->
              s := Bool (b && b')::r
          | _ -> typecheck ()
          end
      | Bitshift ->
          let i = getint () in
            let shift = i32toi (getint ()) in
              let r =
                if i < 0l
                  then Int32.shift_left i shift
                  else Int32.shift_right_logical i (abs shift)
              in
                s := Int r::!s
      | Eq ->
          begin match !s with
          | a::b::r -> s := Bool (a = b)::r
          | _ -> typecheck ()
          end
      | Ge ->
          begin match !s with
          | a::b::r -> s := Bool (a >= b)::r
          | _ -> typecheck ()
          end
      | Gt -> 
          begin match !s with
          | a::b::r -> s := Bool (a > b)::r
          | _ -> typecheck ()
          end
      | Le ->
          begin match !s with
          | a::b::r -> s := Bool (a <= b)::r
          | _ -> typecheck ()
          end
      | Lt ->
          begin match !s with
          | a::b::r -> s := Bool (a < b)::r
          | _ -> typecheck ()
          end
      | Ne ->
          begin match !s with
          | a::b::r -> s := Bool (a <> b)::r
          | _ -> typecheck ()
          end
      | Not ->
          begin match !s with
          | Int i::r ->
              s := Int (Int32.lognot i)::r
          | Bool b::Bool b'::r ->
              s := Bool (not b)::r
          | _ -> typecheck ()
          end
      | Or ->
          begin match !s with
          | Int i::Int i'::r ->
              s := Int (Int32.logor i i')::r
          | Bool b::Bool b'::r ->
              s := Bool (b || b')::r
          | _ -> typecheck ()
          end
      | Xor ->
          begin match !s with
          | Int i::Int i'::r ->
              s := Int (Int32.logxor i i')::r
          | Bool b::Bool b'::r ->
              s := Bool (b |&| b')::r
          | _ -> typecheck ()
          end
      | Copy ->
          begin match !s with
          | Int i::r when i >= 0l ->
              s := take r (i32toi i) @ r
          | _ -> typecheck ()
          end
      | Exch ->
          begin match !s with
          | a::b::r -> s := b::a::r
          | _ -> typecheck ()
          end
      | Pop ->
          begin match !s with
          | a::r -> s := r
          | _ -> typecheck ()
          end
      | Dup ->
          begin match !s with
          | a::r -> s := a::a::r
          | _ -> typecheck ()
          end
      | Index ->
          begin match !s with
          | Int i::r when i >= 0l ->
              let v = select (i32toi i + 1) r in
                s := v::r
          | _ -> typecheck ()
          end
      | Roll ->
          let rec rotate j l =
            if j = 0 then l else
            match l with
            | [] -> []
            | h::t -> rotate (j - 1) (t @ [h])
          and rotate_o j l =
            if j = 0 then l else
              match rev l with
              | [] -> []
              | h::t -> rotate (j - 1) ([h] @ rev t)
          in
            begin match !s with
            | Int j::Int n::r when n >= 1l ->
                let j = i32toi j in let n = i32toi n in
                  let vals, rest = cleave r n in
                    let newvals =
                      if j > 0
                        then rotate j vals
                        else rotate_o (-j) vals
                    in
                    s := newvals @ rest
            | _ -> typecheck ()
            end
  in
    try
      iter eval ops;
      rev_map
        (function
         | Float x -> x
         | Int i -> Int32.to_float i (* Added as a FIX, but we should look at this again...*)
         | _ -> raise (BadFunctionEvaluation "Type 4-a"))
        !s
    with
      e -> flprint (Printexc.to_string e); raise (BadFunctionEvaluation "Type 4-b")

(* Evaluating functions *)

(* Evaluate a function on some inputs. *)
let rec eval_function f inputs =
  let rec clampvals vals domain =
    let clampval v d d' =
      if v < d then d else if v > d' then d' else v
    in
      match vals, domain with
      | [], [] -> []
      | v::vs, d::d'::ds ->
          clampval v d d'::clampvals vs ds
      | _ -> raise (BadFunctionEvaluation "Domain wrong")
  in
    let clamped_inputs = clampvals inputs f.domain in
      let outputs =
        match f.func with
        | Calculator ops ->
            eval_function_calculator clamped_inputs ops
        | Sampled s ->
            eval_function_sampled f s clamped_inputs 
        | Interpolation f ->
            let interp n c0 c1 i =
              try
                map2
                  (fun c0 c1 -> c0 +. i ** n *. (c1 -. c0)) c0 c1
              with
                Invalid_argument _ ->
                  raise (BadFunctionEvaluation "Interpolation")
            in
              flatten (map (interp f.n f.c0 f.c1) clamped_inputs)
        | Stitching s ->
            (* iPrintf.printf "First input to function: %f\n" (hd
             * clamped_inputs); *)
            match clamped_inputs, f.domain with
            | [i], [d0; d1] ->
                let points = [d0] @ s.bounds @ [d1] in
                  (*i flprint "points: ";
                  iter (Printf.printf "%f ") points;
                  flprint "\n";
                  flprint "s.stitch_encode: ";
                  iter (Printf.printf "%f ") s.stitch_encode;
                  flprint "\n"; i*)
                  let rec extract_subfunction points funs n =
                    match points, funs with
                    | p::p'::_, f::_ when i >= p && i < p' -> p, p', f, n
                    | p::p'::_, f::_ when i = p' -> p, p', f, n
                    | _::ps, _::fs -> extract_subfunction ps fs (n + 1)
                    | _ ->
                        raise (BadFunctionEvaluation "stitching: funs")
                  in
                    let d0', d1', f, n = extract_subfunction points s.functions 0 in
                     (*i Printf.printf "d0 = %f, d1 = %f, n = %i\n" d0' d1' n;
                      * i*)
                     let encode =
                       try
                         let a, b, c, d =
                           select (n + 1) points, select (n + 2) points,
                           select (n * 2 + 1) s.stitch_encode, select (n * 2 + 2) s.stitch_encode
                         in
                           (* Printf.printf "a = %f, b %f, c = %f, d = %f\n" a b
                            * c d; *)
                           fun x -> interpolate x a b c d
                       with
                         Invalid_argument _ (*"select"*) ->
                           raise (BadFunctionEvaluation "stitching: encode/domain")
                     in
                       eval_function f [encode i]
            | _ -> raise (BadFunctionEvaluation "stitching: Bad arity")
      in
        let outputs = 
        match f.range with
        | None -> outputs
        | Some range -> clampvals outputs range
        in
          (* iter (Printf.printf "%f ") outputs;
          flprint "\n"; *)
          outputs

(*i let eval_function f vals =
  Printf.printf "eval_function\n";
  iter (Printf.printf "%f ") vals;
  flprint "\n";
  print_function f;
  flprint "\n";
  let vals = eval_function f vals in
    iter (Printf.printf "%f ") vals;
    flprint "\n";
    vals i*)

let funtype_of_function f =
  match f.func with
  | Interpolation _ -> 2
  | Stitching _ -> 3
  | Sampled _ -> 0
  | Calculator _ -> 4

let mkreal x = Pdf.Real x

let entries_of_interpolation i =
  ["/C0", Pdf.Array (map mkreal i.c0);
   "/C1", Pdf.Array (map mkreal i.c1);
   "/N", Pdf.Real i.n]

let rec entries_of_stitching pdf i =
  (* Add the functions as objects. *)
  let numbers =
    map (fun f -> Pdf.Indirect (Pdf.addobj pdf (pdfobject_of_function pdf f))) i.functions
  in
    ["/Functions", Pdf.Array numbers;
     "/Bounds", Pdf.Array (map mkreal i.bounds);
     "/Encode", Pdf.Array (map mkreal i.stitch_encode)]

and extra_entries_of_function pdf f =
  match f.func with
  | Interpolation i -> entries_of_interpolation i
  | Stitching s -> entries_of_stitching pdf s
  | Sampled s -> []
  | Calculator c -> []

and pdfobject_of_function pdf f =
  let domain =
    Pdf.Array (map (function x -> Pdf.Real x) f.domain)
  in let range =
    match f.range with
    | None -> []
    | Some fs -> ["/Range", Pdf.Array (map (function x -> Pdf.Real x) fs)]
  in
    Pdf.Dictionary
      (["/FunctionType", Pdf.Integer (funtype_of_function f); "/Domain", domain]
      @ range @ extra_entries_of_function pdf f)

