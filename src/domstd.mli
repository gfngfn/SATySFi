
type tag_name = string

class type node =
  object
    method childNodes : node list
    method firstChild : node option
    method lastChild : node option
    method nextSibling : node option
    method previousSibling : node option
    method nodeName : string
    method parentNode : node
    method ownerDocument : document option
  end

and document =
  object
    inherit node
    method body : element
    method documentElement : element
  end

and element =
  object
    inherit node
    method tagName : string
  end

type text_node = node

type comment = node

val afterLoadingHTML : (unit -> unit) -> unit

val setInterval : (unit -> unit) -> int -> unit


(* ---- node ---- *)

val setAttribute : string -> string -> #node -> unit

val appendChild : (#node as 'a) -> #node -> 'a

val appendChildMap : #node list -> #node -> unit
(*
val childNodes : #node -> node list

val ownerDocument : #node -> document
*)
(* ---- document ---- *)

val document : document

val getElementById : string -> document -> element
val getElementsByTagName : tag_name -> document -> element list
val getElementsByName : string -> document -> element list

val createElement : tag_name -> document -> element
val createTextNode : tag_name -> document -> text_node
val createComment : tag_name -> document -> comment


(* ---- event ---- *)

type window

class type mouse_event =
  object
    method view : window
    method detail : int
    method button : int
    method altKey : bool
    method ctrlKey : bool
    method metaKey : bool
    method shiftKey : bool
    method clientX : int
    method clientY : int
    method screenX : int
    method screenY : int
  end

class type keyboard_event =
  object
    method altKey : bool
    method ctrlKey : bool
    method shiftKey : bool
    method keyCode : int
  end

type _ event_kind =
  | Click       : mouse_event event_kind
  | DblClick    : mouse_event event_kind
  | ContextMenu : mouse_event event_kind
  | MouseEnter  : mouse_event event_kind
  | MouseLeave  : mouse_event event_kind
  | MouseDown   : mouse_event event_kind
  | MouseUp     : mouse_event event_kind
  | MouseMove   : mouse_event event_kind
  | MouseOver   : mouse_event event_kind
  | MouseOut    : mouse_event event_kind
  | KeyDown     : keyboard_event event_kind
  | KeyPress    : keyboard_event event_kind
  | KeyUp       : keyboard_event event_kind

val addEventListener : 'a event_kind -> ('a -> unit) -> element -> unit


(* ---- extension ---- *)

val setInnerText : string -> #node -> node

val createSvgElement : tag_name -> document -> element

val setAttributeMap : (string * string) list -> #node -> unit
(*
val setAttributeMap : (string * string) list -> node -> node
*)
