module Field : sig
  type t = {
    name : string;
    type_ : string;
  }

  val name : t -> string

  val type_ : t -> string
end

module Param : sig
  type t = {
    name : string;
    type_ : string option;
  }

  val name : t -> string

  val type_ : t -> string option
end

type pp =
  | Default                       (** Use default pretty printer *)
  | Custom of string              (** Specify custom pretty printer *)
  | Simple                        (** Use a simple pretty printer *)

type t = {
  inst : string;                   (** Instruction name (essential) *)
  is_pdf_mode_primitive : bool; (** Primitive for generating PDFs or not (default: [false]) *)
  is_text_mode_primitive : bool; (** Primitive for generating texts or not (default: [false]) *)
  needs_reducef : bool; (** Use [reducef] for evaluating applications (default: [false]) *)
  needs_runtime_config : bool; (** Use [runtime_config] for runtime config values (default: [false]) *)
  no_interp : bool; (** Suppress code generation for [evaluator_.ml] (default: [not is_primitive]) *)
  no_ircode : bool; (** Suppress code generation for [ir_.ml] (default: [false]) *)
  pp : pp;          (** pretty printer setting *)
  name : string option;       (** Identifier for the primitive *)
  type_ : Type.t option;      (** Type expression for the primitive *)
  fields : Field.t list;  (** Field list (for [abstract_tree] type) *)
  params : Param.t list;  (** Paramater list *)
  code : string;          (** Instruction code *)
  code_interp : string option;          (** Code for interpreter *)
}
(**
   Instruction description.

   (note: If [is_primitive] is [true],
   code that pushes to stack and go to next instruction
   will be automatically inserted. (see [gencode.ml]))
*)

val is_primitive : t -> bool

val inst
  : ?is_pdf_mode_primitive:bool
  -> ?is_text_mode_primitive:bool
  -> ?needs_reducef:bool
  -> ?needs_runtime_config:bool
  -> ?no_interp:bool
  -> ?no_ircode:bool
  -> ?pp:pp
  -> ?name:string
  -> ?type_:Type.t
  -> fields:Field.t list
  -> params:Param.t list
  -> ?code_interp:string
  -> code:string
  -> string                             (** inst *)
  -> t
(** smart constructor for {! t} *)

val field : type_:string -> string -> Field.t
(** smart constructor for {! Field.t} *)

val param : ?type_:string -> string -> Param.t
(** smart constructor for {! Param.t} *)
