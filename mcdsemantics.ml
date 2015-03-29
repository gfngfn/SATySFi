(* module Mcdsemantics *)
  open Types

  type location = abstract_tree ref
  type var_environment = (var_name, location) Assoclist.t
  type macro_environment = (macro_name, macro_location) Assoclist.t
  and function_spec = DummyFunc | Func of (var_name list) * abstract_tree * abstract_tree * macro_environment * var_environment
  and macro_location = function_spec ref

  let report_error errmsg =
    print_string ("[ERROR IN MCDSEMANTICS] " ^ errmsg ^ ".")

  (* for test *)
  let print_process stat =
    (* enable below in order to see the process of interpretation *)
  (*
    print_string stat ; print_newline () ;
  *)
    ()

  let loc_indent : location = ref EmptyAbsBlock

  (* abstract_tree -> abstract_tree *)
  let rec semantics abstr =
    print_process "[BEGIN SEMANTICS]" ;
    loc_indent := Output("") ;
    let loc_deepen : macro_location = ref DummyFunc in
    let loc_shallow : macro_location = ref DummyFunc in
    let loc_ifempty : macro_location = ref DummyFunc in
    let menv_main : macro_environment ref = ref Assoclist.empty in
    let venv_main : var_environment ref = ref Assoclist.empty in
      venv_main := (Assoclist.add "~indent" loc_indent !venv_main) ;
      menv_main := (Assoclist.add "\\deepen" loc_deepen !menv_main) ;
      menv_main := (Assoclist.add "\\shallow" loc_shallow !menv_main) ;
      menv_main := (Assoclist.add "\\ifempty" loc_ifempty !menv_main) ;
      loc_deepen := Func([], DeepenIndent, EmptyAbsBlock, !menv_main, !venv_main) ;
      loc_shallow := Func([], ShallowIndent, EmptyAbsBlock, !menv_main, !venv_main) ;
      loc_ifempty := Func(["~subj"; "~tru"; "~fls"],
                       PrimitiveIfEmpty(ContentOf("~subj"), ContentOf("~tru"), ContentOf("~fls")),
                       EmptyAbsBlock, !menv_main, !venv_main
                     ) ;
      interpret menv_main venv_main abstr

  (* (macro_environment ref) -> (var_environment ref) -> abstract_tree -> abstract_tree *)
  and interpret menv venv abstr =

    match abstr with

      PrimitiveIfEmpty(abstr_subj, abstr_tru, abstr_fls) -> (
          let value_subj = interpret menv venv abstr_subj in
          let value_tru = interpret menv venv abstr_tru in
          let value_fls = interpret menv venv abstr_fls in
            match value_subj with
              EmptyAbsBlock -> value_tru
            | _ -> value_fls
        )

    | DeepenIndent -> (
          print_process "$DeepenIndent" ;
          (
            match !loc_indent with
              Output(indent_str) -> loc_indent := Output(indent_str ^ "  ")
            | _ -> report_error "illegal DeepenIndent"
          ) ;
          EmptyAbsBlock
        )

    | ShallowIndent -> (
          print_process "$ShallowIndent" ;
          (
            match !loc_indent with
              Output(indent_str) ->
                let len = String.length indent_str in
                  if len >= 2 then loc_indent := Output(String.sub indent_str 0 (len - 2))
                  else ()
            | _ -> report_error "illegal ShallowIndent"
          ) ;
          EmptyAbsBlock
        )

    | EmptyAbsBlock -> (
          print_process "$EmptyAbsBlock" ;
          EmptyAbsBlock
        )

    | AbsBlock(abstr_head, abstr_tail) -> (
          print_process "$AbsBlock 2" ;
          let value_head = interpret menv venv abstr_head in
          let value_tail = interpret menv venv abstr_tail in
            match value_head with
              EmptyAbsBlock -> value_tail
            | _ -> (
                  match value_tail with
                    EmptyAbsBlock -> value_head
                  | _ -> AbsBlock(value_head, value_tail)
                )
        )

    | Output(c) -> (
          print_process ("$Output: " ^ c) ;
          Output(c)
        )

    | ContentOf(v) -> (
          print_process ("$ContentOf: " ^ v) ;
          interpret menv venv !(Assoclist.get_value (!venv) v)
        (*  !(Assoclist.get_value (!venv) v) *)
        )

    | Separated(abstr_former, abstr_latter) -> (
          print_process "$Separated" ;
          let value_former = interpret menv venv abstr_former in
          let value_latter = interpret menv venv abstr_latter in
            Separated(value_former, value_latter)
        )

    | Pop(u, v, abstr_rawlist, abstr_content) -> (
          let value_rawlist = interpret menv venv abstr_rawlist in
            match value_rawlist with
              EmptyAbsBlock -> (
                  print_process "$Pop (Empty)" ;
                  EmptyAbsBlock
                )
            | _ -> (
                  match pop_from_separated_tree value_rawlist UnderConstruction with
                    (value_former, value_latter) -> (
                        print_process "Pop (Content)" ;
                        let loc_former : location = ref value_former in
                        let loc_latter : location = ref value_latter in
                        let venv_content = ref (Assoclist.add v loc_latter (Assoclist.add u loc_former !venv)) in
                          interpret menv venv_content abstr_content
                      )
                )
        )
(*
          match value_rawlist with
            EmptyAbsBlock -> (
                print_process "$Pop (Empty)" ;
                EmptyAbsBlock
              )
          | Separated(abstr_former, abstr_latter) -> (
                print_process "$Pop (Plural)" ;
                let value_former = interpret menv venv abstr_former in
                let value_latter = interpret menv venv abstr_latter in
                let loc_former : location = ref value_former in
                let loc_latter : location = ref value_latter in
                let venv_content = ref (Assoclist.add v loc_latter (Assoclist.add u loc_former !venv)) in
                (* venv{ u |-> loc_former, v |-> loc_latter } *)
                (*
                  print_string " ***( " ;
                  Assoclist.print_key !venv_content ;
                  print_string " )***" ; print_newline () ;
                *)
                  interpret menv venv_content abstr_content
              )
          | abstr_former -> (
                print_process "$Pop (Single)" ;
                let value_former = interpret menv venv abstr_former in
                let loc_former : location = ref value_former in
                let loc_latter : location = ref EmptyAbsBlock in
                let venv_content = ref (Assoclist.add v loc_latter (Assoclist.add u loc_former !venv)) in
                (* venv{ u|->loc_former, v|->loc_latter } *)
                (*
                  print_string " **( " ;
                  Assoclist.print_key !venv_content ;
                  print_string " )**" ; print_newline () ;
                *)
                  interpret menv venv_content abstr_content
              )
*)

    | Macro(f, var_list, abstr_noid, abstr_id) -> (
          print_process "$Macro" ;
          let loc : macro_location = ref DummyFunc in (* dummy *)
          let menv_new = ref (Assoclist.add f loc !menv) in
          (* menv{ f|->loc } *)
          let value = Func(var_list, abstr_noid, abstr_id, !menv_new, !venv) in
            ( loc := value ; menv := !menv_new ; EmptyAbsBlock )
        )

    | Apply(f, NoID, param_list) -> (
          print_process "$Apply (NoID)" ;
          match !(Assoclist.get_value (!menv) f) with (* !(menv(f)) *)
            Func(var_list, abstr_noid, abstr_id, cont_menv_f, cont_venv_f) -> (
                let value_list = interpret_list menv venv param_list in
                let loc_list : location list = ref_list value_list in
                let venv_new = ref (Assoclist.add_list var_list loc_list cont_venv_f) in
                (* venv_f{ v_1|->l_1, ..., v_n|->l_n } *)
                (*
                  print_string " *|*( " ;
                  Assoclist.print_key !venv_new ;
                  print_string " )*|*" ; print_newline () ;
                *)
                  interpret menv venv_new abstr_noid
              )
          | DummyFunc -> ( report_error "illegal Apply of DummyFunc" ; Invalid )
        )
    | Apply(f, RealID(i), param_list) -> (
          print_process "$Apply (ID)" ;
          match !(Assoclist.get_value (!menv) f) with (* !(menv(f)) *)
            Func(var_list, abstr_noid, abstr_id, cont_menv_f, cont_venv_f) -> (
                let value_list = interpret_list menv venv param_list in
                let loc_list : location list = ref_list value_list in
                let loc_id : location = ref (id_to_abstract_tree i) in
                let venv_new = ref (Assoclist.add "@id" loc_id (Assoclist.add_list var_list loc_list cont_venv_f)) in
                (* venv_f{ v_1|->l_1, ..., v_n|->l_n, @id|->loc_id } *)
                (*
                  print_string " *|*( " ;
                  Assoclist.print_key !venv_new ;
                  print_string " )*|*" ; print_newline () ;
                *)
                  interpret menv venv_new abstr_id
              )
          | DummyFunc -> ( report_error "illegal Apply of DummyFunc" ; Invalid )
        )

    | _ -> Invalid

  (* macro_environment -> var_environment -> (abstract_tree list) -> (abstract_tree list) *)
  and interpret_list menv venv abstr_list =
    match abstr_list with
      [] -> []
    | abstr_head :: abstr_tail -> (
          let intrprtd_head = interpret menv venv abstr_head in
          let intrprtd_tail = interpret_list menv venv abstr_tail in
            intrprtd_head :: intrprtd_tail
        )

  (* abstract_tree list -> location list *)
  and ref_list value_list =
    match value_list with
      [] -> []
    | value_head :: value_tail -> (ref value_head) :: (ref_list value_tail)

  and id_to_abstract_tree id = Output(id)

  (* abstract_tree -> abstract_tree -> (abstract_tree * abstract_tree) *)
  and pop_from_separated_tree abstr_in abstr_constr =
    match abstr_in with
      Separated(abstr_former, abstr_latter) -> (
          match abstr_former with
            Separated(a, b) -> (
              pop_from_separated_tree abstr_former (compensate abstr_constr (Separated(UnderConstruction, abstr_latter)))
            )
          | _ -> (abstr_former, compensate abstr_constr abstr_latter)
        )
    | _ -> (abstr_in, EmptyAbsBlock)

  (* abstract_tree -> abstract_tree -> abstract_tree *)
  and compensate abstr_under_constr abstr_cmpnstd =
    match abstr_under_constr with
      Separated(abstr_former, abstr_latter)
        -> Separated((compensate abstr_former abstr_cmpnstd), (compensate abstr_latter abstr_cmpnstd))
    | UnderConstruction -> abstr_cmpnstd
    | abstr_other -> abstr_other