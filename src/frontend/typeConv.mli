
open Types
open StaticEnv

val erase_range_of_type : mono_type -> mono_type

val overwrite_range_of_type : mono_type -> Range.t -> mono_type

val unlink : mono_type -> mono_type

val get_opaque_type : type_scheme -> TypeID.t option

val generalize : level -> mono_type -> poly_type

val generalize_macro_type : mono_macro_type -> poly_macro_type

val lift_poly : mono_type -> poly_type

val instantiate : level -> quantifiability -> poly_type -> mono_type

val instantiate_by_map_mono :  mono_type BoundIDMap.t -> poly_type -> mono_type

val instantiate_macro_type : level -> quantifiability -> poly_macro_type -> mono_macro_type

val apply_type_scheme_poly : type_scheme -> poly_type_body list -> poly_type option

val apply_type_scheme_mono : type_scheme -> mono_type list -> mono_type option

val normalize_poly_row : poly_row -> normalized_poly_row

val make_opaque_type_scheme : int -> TypeID.t -> type_scheme

val kind_equal : kind -> kind -> bool

val poly_type_equal : poly_type -> poly_type -> bool

val normalize_mono_row : mono_row -> normalized_mono_row

val normalized_poly_row_equal : normalized_poly_row -> normalized_poly_row -> bool
