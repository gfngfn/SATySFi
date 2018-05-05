(* Ugly hack: we just want to pass `error_store` as a context to the parser
 * here. We could just use Menhir's ability to parameterize the parser over
 * modules, but it's like breaking a butterfly upon a wheel. We would need
 * to separate tokens to an independent module to avoid parameterization of
 * token definitions, and thus have to modify builds.
 *
 * Instead, we use a thread local storage to accomplish the task.
 *)

(* For now, the only context is error_store. *)
val with_context : ErrorReporting.error_store -> (unit -> 'a) -> 'a
val get_error_store : unit -> ErrorReporting.error_store
