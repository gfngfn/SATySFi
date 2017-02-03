(*
type element   = [ `Element ]
type document  = [ `Document ]
type text_node = [ `TextNode ]
type comment   = [ `Comment ]
type node      = [ element | text_node | comment | document ]
*)
type tag_name = string

class type node_js =
  object
    method childNodes : node_js Js.t array
    method firstChild : node_js Js.t Js.null
    method lastChild : node_js Js.t Js.null
    method nextSibling : node_js Js.t Js.null
    method previousSibling : node_js Js.t Js.null
    method nodeName : string
    method parentNode : node_js Js.t
    method ownerDocument : document_js Js.t Js.null
  end [@bs]

and document_js =
  object
    inherit node_js
    method body : element_js Js.t
    method documentElement : element_js Js.t
  end [@bs]

and element_js =
  object
    inherit node_js
    method tagName : string
  end [@bs]

(*
type node_js =
  < childNodes : node_js array;
    firstChild : node_js Js.null;
    lastChild : node_js Js.null;
    nextSibling : node_js Js.null;
    previousSibling : node_js Js.null;
    nodeName : string;
    parentNode : node_js;
    ownerDocument : document_js Js.null > Js.t

and document_js =
  < childNodes : node_js array;
    firstChild : node_js Js.null;
    lastChild : node_js Js.null;
    nextSibling : node_js Js.null;
    previousSibling : node_js Js.null;
    nodeName : string;
    parentNode : node_js;
    ownerDocument : document_js Js.null;
    body : element_js;
    documentElement : element_js > Js.t

and element_js =
  < childNodes : node_js array;
    firstChild : node_js Js.null;
    lastChild : node_js Js.null;
    nextSibling : node_js Js.null;
    previousSibling : node_js Js.null;
    nodeName : string;
    parentNode : node_js;
    ownerDocument : document_js Js.null > Js.t
*)

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


let option_map f opt =
  match opt with
  | None    -> None
  | Some(x) -> Some(f x)


let rec to_node (njs : node_js Js.t) : node =
  object
    method childNodes      = List.map to_node (Array.to_list njs##childNodes)
    method firstChild      = option_map to_node (Js.Null.to_opt njs##firstChild)
    method lastChild       = option_map to_node (Js.Null.to_opt njs##lastChild)
    method nextSibling     = option_map to_node (Js.Null.to_opt njs##nextSibling)
    method previousSibling = option_map to_node (Js.Null.to_opt njs##previousSibling)
    method nodeName        = njs##nodeName
    method parentNode      = to_node njs##parentNode
    method ownerDocument   = option_map to_document (Js.Null.to_opt njs##ownerDocument)
  end

and to_document (djs : document_js Js.t) : document =
  object
    method childNodes      = List.map to_node (Array.to_list djs##childNodes)
    method firstChild      = option_map to_node (Js.Null.to_opt djs##firstChild)
    method lastChild       = option_map to_node (Js.Null.to_opt djs##lastChild)
    method nextSibling     = option_map to_node (Js.Null.to_opt djs##nextSibling)
    method previousSibling = option_map to_node (Js.Null.to_opt djs##previousSibling)
    method nodeName        = djs##nodeName
    method parentNode      = to_node djs##parentNode
    method ownerDocument   = option_map to_document (Js.Null.to_opt djs##ownerDocument)
    method body            = to_element djs##body
    method documentElement = to_element djs##documentElement
  end

and to_element (ejs : element_js Js.t) : element =
  object
    method childNodes      = List.map to_node (Array.to_list ejs##childNodes)
    method firstChild      = option_map to_node (Js.Null.to_opt ejs##firstChild)
    method lastChild       = option_map to_node (Js.Null.to_opt ejs##lastChild)
    method nextSibling     = option_map to_node (Js.Null.to_opt ejs##nextSibling)
    method previousSibling = option_map to_node (Js.Null.to_opt ejs##previousSibling)
    method nodeName        = ejs##nodeName
    method parentNode      = to_node ejs##parentNode
    method ownerDocument   = option_map to_document (Js.Null.to_opt ejs##ownerDocument)
    method tagName         = ejs##tagName
  end


let option_to_nullable (opt : 'a option) : 'a Js.null =
  match opt with
  | None    -> Js.Null.empty
  | Some(x) -> Js.Null.return x


let rec to_node_js (nd : #node) : node_js Js.t =
  [%bs.obj {
    childNodes      = Array.of_list (List.map to_node_js nd#childNodes);
    firstChild      = option_to_nullable (option_map to_node_js nd#firstChild);
    lastChild       = option_to_nullable (option_map to_node_js nd#lastChild);
    nextSibling     = option_to_nullable (option_map to_node_js nd#nextSibling);
    previousSibling = option_to_nullable (option_map to_node_js nd#previousSibling);
    nodeName        = nd#nodeName;
    parentNode      = to_node_js nd#parentNode;
    ownerDocument   = option_to_nullable (option_map to_document_js nd#ownerDocument) }]

and to_document_js (nd : document) : document_js Js.t =
  [%bs.obj {
    childNodes      = Array.of_list (List.map to_node_js nd#childNodes);
    firstChild      = option_to_nullable (option_map to_node_js nd#firstChild);
    lastChild       = option_to_nullable (option_map to_node_js nd#lastChild);
    nextSibling     = option_to_nullable (option_map to_node_js nd#nextSibling);
    previousSibling = option_to_nullable (option_map to_node_js nd#previousSibling);
    nodeName        = nd#nodeName;
    parentNode      = to_node_js nd#parentNode;
    ownerDocument   = option_to_nullable (option_map to_document_js nd#ownerDocument);
    body            = to_element_js nd#body;
    documentElement = to_element_js nd#documentElement }]

and to_element_js (nd : element) : element_js Js.t =
  [%bs.obj {
    childNodes      = Array.of_list (List.map to_node_js nd#childNodes);
    firstChild      = option_to_nullable (option_map to_node_js nd#firstChild);
    lastChild       = option_to_nullable (option_map to_node_js nd#lastChild);
    nextSibling     = option_to_nullable (option_map to_node_js nd#nextSibling);
    previousSibling = option_to_nullable (option_map to_node_js nd#previousSibling);
    nodeName        = nd#nodeName;
    parentNode      = to_node_js nd#parentNode;
    ownerDocument   = option_to_nullable (option_map to_document_js nd#ownerDocument);
    tagName         = nd#tagName }]

let afterLoadingHTML : (unit -> unit) -> unit =
  [%bs.raw{|
    function(f) { window.onload = f; }
  |}]

external setInterval : (unit -> unit) -> int -> unit = "setInterval" [@@bs.val]


(* ---- node ---- *)

external setAttribute : string -> string -> unit = "setAttribute" [@@bs.send.pipe: #node]

external appendChild : #node -> node = "appendChild" [@@bs.send.pipe: #node]

let appendChildMap (lst : #node list) (prnt : #node) : unit =
  List.iter (fun x -> prnt |> appendChild x |> ignore) lst
(*
external appendChild_ext : node_js Js.t -> node_js Js.t = "appendChild" [@@bs.send.pipe: node_js Js.t]
let appendChild (ch : #node) (nd : #node) = to_node (appendChild_ext (to_node_js ch) (to_node_js nd))
*)

(*
let childNodes_aux : #node -> node array =
  [%bs.raw{|
    function(nd) { return nd.childNodes; }
  |}]
let childNodes (nd : #node) = Array.to_list (nd |> childNodes_aux)

let ownerDocument : #node -> document =
  [%bs.raw{|
    function(nd) { return nd.ownerDocument; }
  |}]
*)


(* ---- document ---- *)

external document : document = "document" [@@bs.val]

external getElementById : string -> element = "getElementById" [@@bs.send.pipe: document]

external getElementsByTagName_ext : tag_name -> element array = "getElementsByTagName" [@@bs.send.pipe: document]
let getElementsByTagName tagnm doc = Array.to_list (doc |> getElementsByTagName_ext tagnm)

external getElementsByName_ext : string -> element array = "getElementsByName" [@@bs.send.pipe: document]
let getElementsByName nm doc = Array.to_list (getElementsByName_ext nm doc)

external createElement : tag_name -> element = "createElement" [@@bs.send.pipe: document]
external createTextNode : string -> text_node = "createTextNode" [@@bs.send.pipe: document]
external createComment : string -> comment = "createComment" [@@bs.send.pipe: document]
external createElementNS : string -> tag_name -> element = "createElementNS" [@@bs.send.pipe: document]

external close : unit = "close" [@@bs.send.pipe: document]


(* ---- event ---- *)

type window

class type mouse_event_js =
  object
    method view : window
    method detail : int
    method button : int
    method altKey : Js.boolean
    method ctrlKey : Js.boolean
    method metaKey : Js.boolean
    method shiftKey : Js.boolean
    method clientX : int
    method clientY : int
    method screenX : int
    method screenY : int
  end [@bs]

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

let ocaml_object_of_mouse_event_js (ejs : mouse_event_js Js.t) : mouse_event =
  object
    method view = ejs##view
    method detail = ejs##detail
    method button = ejs##button
    method altKey = Js.to_bool ejs##altKey
    method ctrlKey = Js.to_bool ejs##ctrlKey
    method metaKey = Js.to_bool ejs##metaKey
    method shiftKey = Js.to_bool ejs##shiftKey
    method clientX = ejs##clientX
    method clientY = ejs##clientY
    method screenX = ejs##screenX
    method screenY = ejs##screenY
  end

class type keyboard_event_js =
  object
    method altKey : Js.boolean
    method ctrlKey : Js.boolean
    method shiftKey : Js.boolean
    method keyCode : int
  end [@bs]

class type keyboard_event =
  object
    method altKey : bool
    method ctrlKey : bool
    method shiftKey : bool
    method keyCode : int
  end

let ocaml_object_of_keyboard_event_js (ejs : keyboard_event_js Js.t) : keyboard_event =
  object
    method altKey = Js.to_bool ejs##altKey
    method ctrlKey = Js.to_bool ejs##ctrlKey
    method shiftKey = Js.to_bool ejs##shiftKey
    method keyCode = ejs##keyCode
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


external addEventListener_ext : string -> ('a -> unit) -> unit  = "addEventListener" [@@bs.send.pipe: element]
let addEventListener (type a) (k : a event_kind) (f : a -> unit) (elem : element) : unit =
  let me g = (fun mejs -> g (ocaml_object_of_mouse_event_js mejs)) in
  let ke g = (fun kejs -> g (ocaml_object_of_keyboard_event_js kejs)) in
    match k with
    | Click       -> addEventListener_ext "click" (me f) elem
    | DblClick    -> addEventListener_ext "dblclick" (me f) elem
    | ContextMenu -> addEventListener_ext "contextmenu" (me f) elem
    | MouseEnter  -> addEventListener_ext "mouseenter" (me f) elem
    | MouseLeave  -> addEventListener_ext "mouseleave" (me f) elem
    | MouseDown   -> addEventListener_ext "mousedown" (me f) elem
    | MouseUp     -> addEventListener_ext "mouseup" (me f) elem
    | MouseMove   -> addEventListener_ext "mousemove" (me f) elem
    | MouseOver   -> addEventListener_ext "mouseover" (me f) elem
    | MouseOut    -> addEventListener_ext "mouseout" (me f) elem
    | KeyDown     -> addEventListener_ext "keydown" (ke f) elem
    | KeyPress    -> addEventListener_ext "keypress" (ke f) elem
    | KeyUp       -> addEventListener_ext "keyup" (ke f) elem


(* ---- extension ---- *)

let removeAllChild (nd : #node) : node =
  [%bs.raw{|
    function(nd) {
      while(nd.childNodes) { nd.removeChild(nd.firstChild); }
      return nd;
    }
  |}]
(*
let setInnerText txt nd =
  let txtnd = (nd |> ownerDocument) |> createTextNode txt in
    nd |> removeAllChild |> appendChild txtnd
*)

external setInnerText : string -> #node -> node = "setInnerText_aux" [@@bs.val]
[%%bs.raw{|
  var setInnerText_aux = function(txt, nd) {
    nd.innerHTML = txt;
    console.log("A: " + txt);
    return nd;
  }
|}]

let createSvgElement tagnm doc =
  doc |> createElementNS "http://www.w3.org/2000/svg" tagnm

let setAttributeMap lst elem =
  List.iter (fun (k, v) -> elem |> setAttribute k v) lst
(*  List.fold_left (fun e (k, v) -> e |> setAttribute k v) elem lst *)
