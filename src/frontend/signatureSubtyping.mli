
open StaticEnv
open TypecheckUtil

(** [substitute_abstract θ ξ] means [θ(ξ)],
    i.e., the application of the substitution [θ] to the abstract signature [ξ]. *)
val substitute_abstract : substitution -> signature abstracted -> signature abstracted

(** [substitute_concrete θ Σ] means [θ(Σ)],
    i.e., the application of the substitution [θ] to the concrete signature [Σ]. *)
val substitute_concrete : substitution -> signature -> signature

(** [subtype_concrete_with_abstract rng Σ_1 (∃A. Σ_2)] judges
    whether there exists a substitution [θ] that satisfies [Σ_1 <: θ(Σ_2)],
    i.e., one that makes [θ(Σ_2)] a supertype of [Σ_1],
    and returns such [θ] if existent.
    [rng] is used only for error reports. *)
val subtype_concrete_with_abstract : Range.t -> signature -> signature abstracted -> substitution ok
