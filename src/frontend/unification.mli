
open Types
open TypeError

(** [unify_type τ_1 τ_2] imperatively updates type/row variables in [τ_1] and [τ_2]
    in order to make [τ_1] and [τ_2] equal to each other. *)
val unify_type : mono_type -> mono_type -> (unit, unification_error) result

(** [unify_row r_1 r_2] imperatively updates type/row variables in [r_1] and [r_2]
    in order to make [r_1] and [r_2] equal to each other. *)
val unify_row : mono_row -> mono_row -> (unit, unification_error) result
