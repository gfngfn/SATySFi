
exception NotDuringPageBreak

type state = {
  mutable during_page_break : bool;
}


let state = {
  during_page_break = false;
}


let start_page_break () = state.during_page_break <- true

let during_page_break () = state.during_page_break
