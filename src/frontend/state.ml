
exception NotDuringPageBreak

type test_result =
  | Pass
  | Fail of string
(* TODO: extend this *)

type state = {
  mutable during_page_break : bool;
  mutable test_result_acc   : test_result Alist.t;
}


let state = {
  during_page_break = false;
  test_result_acc   = Alist.empty
}


let start_page_break () = state.during_page_break <- true

let during_page_break () = state.during_page_break

let add_test_result (r : test_result) =
  state.test_result_acc <- Alist.extend state.test_result_acc r

let get_all_test_results () =
  Alist.to_list state.test_result_acc
