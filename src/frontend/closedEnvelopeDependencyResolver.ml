
open MyUtil
open LoggingUtil
open EnvelopeSystemBase
open ConfigError
open Types


type 'a ok = ('a, config_error) result


module EnvelopeDependencyGraph = DependencyGraph.Make(String)


type envelope_info = {
  envelope_name         : envelope_name;
  envelope_config       : abs_path;
  envelope_dependencies : envelope_dependency list;
}


let main (logging_spec : logging_spec) ~(use_test_only_envelope : bool) ~(extensions : string list) (deps_config : DepsConfig.t) : ((envelope_name * (EnvelopeConfig.t * untyped_envelope)) list) ok =
  let open ResultMonad in

  let { envelopes; _ } = deps_config in

  (* Add vertices: *)
  let* (graph, entryacc) =
    envelopes |> foldM (fun (graph, entryacc) (envelope_spec : envelope_spec) ->
      let
        {
          envelope_name;
          envelope_path = abspath_envelope_config;
          envelope_dependencies;
          test_only_envelope;
        } = envelope_spec
      in
      if test_only_envelope && not use_test_only_envelope then
      (* Skips test-only envelopes when using sources only: *)
        return (graph, entryacc)
      else
        let* envelope_with_config =
          EnvelopeReader.main
            logging_spec
            ~use_test_files:false (* Does not use tests of depended packages. *)
            ~extensions
            ~envelope_config:abspath_envelope_config
        in
        let* (graph, vertex) =
          graph |> EnvelopeDependencyGraph.add_vertex envelope_name envelope_with_config
            |> Result.map_error (fun _ -> EnvelopeNameConflict(envelope_name))
        in
        let envelope_info =
          {
            envelope_name;
            envelope_config = abspath_envelope_config;
            envelope_dependencies;
          }
        in
        return (graph, Alist.extend entryacc (envelope_info, vertex))
    ) (EnvelopeDependencyGraph.empty, Alist.empty)
  in

  (* Add edges: *)
  let* graph =
    entryacc |> Alist.to_list |> foldM (fun graph (envelope_info, vertex) ->
      envelope_info.envelope_dependencies |> foldM (fun graph envelope_dependency ->
        let { dependency_name = envelope_name_dep; _ } = envelope_dependency in
          (* TODO: use `dependency_used_as` *)
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
      CyclicEnvelopeDependency(
        cycle |> map_cycle (fun (envelope_name, (_config, envelope_info)) -> (envelope_name, envelope_info))
      )
    )
