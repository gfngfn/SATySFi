open Pdfutil

type t =
  {year : int;
   month : int;
   day : int;
   hour : int;
   minute : int;
   second : int;
   hour_offset : int;
   minute_offset : int}

exception BadDate

let check_date d = 
  try
    assert (d.year >= 0 && d.year <= 9999);
    assert (d.month >= 1 && d.month <= 12);
    assert (d.day >= 1 && d.day <= 31);
    assert (d.hour >= 0 && d.hour <= 23);
    assert (d.minute >= 0 && d.minute <= 59);
    assert (d.second >= 0 && d.second <= 59);
    assert (d.hour_offset >= -23 && d.hour_offset <= 23);
    assert (d.minute_offset >= -59 && d.minute_offset <= 59);
  with
    _ -> raise BadDate

(* For now, no detection of default values which could be omitted. *)
let string_of_date d =
  check_date d;
  let ostr =
    if d.hour_offset < 0 then
      Printf.sprintf "-%02i'%02i'" (abs d.hour_offset) (abs d.minute_offset)
    else if d.hour_offset > 0 then
      Printf.sprintf "+%02i'%02i'" (abs d.hour_offset) (abs d.minute_offset)
    else "Z"
  in
    Printf.sprintf
      "D:%04i%02i%02i%02i%02i%02i%s"
      d.year d.month d.day d.hour d.minute d.second ostr

let date_of_string s =
  let safe_int_string chars =
    try int_of_string (implode chars) with
      _ -> raise BadDate
  in
    let hour_offset = ref 0
    in let minute_offset = ref 0
    in let o = ref 0 in
      let rec optional_twochar def cs =
        match cs with
        | ('-' | '+' | 'Z')::_ ->
            parse_local_time cs;
            def, []
        | a::b::more -> safe_int_string [a;b], more
        | _ -> def, []
      and parse_local_time cs =
        let o_got, cs =
          match cs with
          | '+'::more -> 1, more
          | '-'::more -> -1, more
          | 'Z'::more -> 0, more
          | _ -> 0, []
        in
          let h, cs = optional_twochar 0 cs in
            match cs with
            | [] -> ()
            | _ ->
                let m, cs = optional_twochar 0 (tl cs) in
                  hour_offset := h;
                  minute_offset := m;
                  o := o_got
      in
        let cs = explode s in
          let cs =
            match cs with
            | 'D'::':'::more -> more
            | _ -> cs
          in
            let year, cs =
              match cs with
              | '1'::'9'::'1'::'0'::'0'::more -> 2000, more (* Adobe Distiller 3 Y2K Bug *)
              | a::b::c::d::more -> safe_int_string [a;b;c;d], more
              | _ -> raise BadDate
            in
              let month, cs = optional_twochar 1 cs in
                let day, cs = optional_twochar 1 cs in
                  let hour, cs = optional_twochar 0 cs in
                    let minute, cs = optional_twochar 0 cs in
                      let second, cs = optional_twochar 0 cs in
                        parse_local_time cs;
                        let date =
                          {year = year;
                           month = month;
                           day = day;
                           hour = hour;
                           minute = minute;
                           second = second;
                           hour_offset = !hour_offset * !o;
                           minute_offset = !minute_offset * !o}
                        in
                          check_date date;
                          date

(* Example *)
let test () =
  flprint
    (string_of_date
      {year = 2000; month = 3; day = 16; hour = 13; minute = 2; second = 34; hour_offset = -1; minute_offset = 24});
  let d =
    date_of_string "D:199812231952-08'00'"
  in
    Printf.printf "\n%i %i %i %i %i %i %i %i\n" d.year d.month d.day d.hour d.minute d.second d.hour_offset d.minute_offset;
    flprint "\n"

