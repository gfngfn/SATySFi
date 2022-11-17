
exception NotDuringPageBreak

type test_result =
  | Pass of {
      test_name : string;
    }
  | Fail of {
      test_name : string;
      message   : string;
    }

val start_page_break : unit -> unit

val during_page_break : unit -> bool

val add_test_result : test_result -> unit

val get_all_test_results : unit -> test_result list
