open Types

type t

val from_list : (var_name * poly_type) list -> t

val add : t -> var_name -> poly_type -> t

val find : t -> (module_name list) -> var_name -> poly_type

val enter_new_module : t -> module_name -> t

val leave_module : t -> t

