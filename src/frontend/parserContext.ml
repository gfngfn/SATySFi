(* Ugly hack: we just want to pass `error_store` as a context to the parser
 * here. We could just use Menhir's ability to parameterize the parser over
 * modules, but it's like breaking a butterfly upon a wheel. We would need
 * to separate tokens to an independent module to avoid parameterization of
 * token definitions, and thus have to modify builds.
 *
 * Instead, we use a thread local storage to accomplish the task.
 *)

let contexts_mutex = Mutex.create ()

let with_locked callback =
  Mutex.lock contexts_mutex;
  try
    let result = callback (Thread.id (Thread.self ())) in
    Mutex.unlock contexts_mutex;
    result
  with e ->
    Mutex.unlock contexts_mutex;
    raise e

let contexts_tls = Hashtbl.create 1

let push_context ctx =
  with_locked (fun tid -> Hashtbl.add contexts_tls tid ctx)

let top_context () =
  with_locked (fun tid -> Hashtbl.find contexts_tls tid)

let pop_context () =
  with_locked (fun tid -> Hashtbl.remove contexts_tls tid)

let with_context ctx callback =
  push_context ctx;
  let result =
    try
      callback ()
    with e ->
      pop_context ();
      raise e
  in
  pop_context ();
  result

let get_error_store () =
  let ctx = top_context () in
  ctx
