
open MyUtil
open Types
open EnvelopeSystemBase
open ConfigError


type 'a ok = ('a, config_error) result


module EnvelopeDependencyGraph = DependencyGraph.Make(String)


let main (display_config : Logging.config) ~(use_test_only_envelope : bool) ~(extensions : string list) (deps_config : DepsConfig.t) : ((envelope_name * (EnvelopeConfig.t * untyped_envelope)) list) ok =
  let open ResultMonad in

  let DepsConfig.{ envelopes } = deps_config in

  (* Add vertices: *)
  let* (graph, entryacc) =
    envelopes |> foldM (fun (graph, entryacc) (envelope : DepsConfig.envelope) ->
      let DepsConfig.{ envelope_name; envelope_path; envelope_dependencies; test_only_envelope; _ } = envelope in
      if test_only_envelope && not use_test_only_envelope then
      (* Skips test-only envelopes when using sources only: *)
        return (graph, entryacc)
      else
        let absdir_envelope = make_abs_path envelope_path in
(*
          match envelope_contents with
          | RegisteredLock{ registry_hash_value; package_name; version = locked_version } ->
              let libdir = Constant.lock_directory Lock.{ registry_hash_value; package_name; locked_version } in
              make_abs_path (Filename.concat (get_abs_path_string absdir_lib_root) (get_lib_path_string libdir))
*)
        let* envelope_with_config =
          EnvelopeReader.main display_config ~use_test_files:use_test_only_envelope ~extensions absdir_envelope
        in
        let* (graph, vertex) =
          graph |> EnvelopeDependencyGraph.add_vertex envelope_name envelope_with_config
            |> Result.map_error (fun _ -> EnvelopeNameConflict(envelope_name))
        in
        let envelope_info =
          {
            envelope_name;
            envelope_directory = absdir_envelope;
            envelope_dependencies;
          }
        in
        return (graph, Alist.extend entryacc (envelope_info, vertex))
    ) (EnvelopeDependencyGraph.empty, Alist.empty)
  in

  (* Add edges: *)
  let* graph =
    entryacc |> Alist.to_list |> foldM (fun graph (envelope_info, vertex) ->
      envelope_info.envelope_dependencies |> foldM (fun graph envelope_name_dep ->
        begin
          match graph |> EnvelopeDependencyGraph.get_vertex envelope_name_dep with
          | None ->
              err @@ DependencyOnUnknownEnvelope{
                depending = envelope_info.envelope_name;
                depended  = envelope_name_dep;
              }

          | Some(vertex_dep) ->
              let graph = graph |> EnvelopeDependencyGraph.add_edge ~from:vertex ~to_:vertex_dep in
              return graph
        end
      ) graph
    ) graph
  in

  EnvelopeDependencyGraph.topological_sort graph
    |> Result.map_error (fun cycle ->
      CyclicEnvelopeDependency(cycle |> map_cycle (fun (envelope_name, (_config, envelope_info)) -> (envelope_name, envelope_info)))
    )
