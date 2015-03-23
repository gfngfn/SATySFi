
module McdSemantics (* : sig

end *) = struct

  let rec interpret menv venv abstr =

    match abstr with

      EmptyAbsBlock -> EmptyAbsBlock

    | AbsBlock(abstr_head, abstr_tail) -> (
          let val_head = interpret menv venv abstr_head in
          let val_tail = interpret menv venv abstr_tail in
            AbsBlock(val_head, val_tail)
        )

    | Output(c) -> Output(c)

    | ContentOf(v) -> (* !(venv(v)) *)

    | Pop(u, v, Separated(abstr_former, abstr_latter), abstr_main) -> (
          let val_former = interpret menv venv abstr_former in
          let val_latter = interpret menv venv abstr_latter in
          let loc_former = ref val_former in
          let loc_latter = ref val_latter in
          let venv_main = (* E{ u|->loc_head, v|->loc_tail } *) in
            interpret menv venv_main abstr_main
        )
    | Pop(u, v, EmptyAbsBlock, abstr_main) -> EmptyAbsBlock
    | Pop(u, v, abstr_head, abstr_main) -> (
          let val_head = interpret menv venv abstr_head in
          let loc_head = ref val_head in
          let loc_tail = ref EmptyAbsBlock in
          let venv_main = (* venv{ u|->loc_head, v|->loc_tail } *) in
            interpret menv venv_main abstr_main
        )

    | Macro(f, var_list, abstr_noid, abstr_id) -> (
          let loc = ref EmptyAbsBlock in
          let menv_new = (* menv{ f|->loc } *) in
          let val = Func(var_list, abstr_noid, abstr_id, menv_new, venv) in
            ( loc := val ; (* menv := menv_new *) ; EmptyAbsBlock )
        )

    | Apply(f, NoID, param_list) -> (
          match (* !(menv(f)) *) with
            Function(var_list, abstr_noid, abstr_id, menv_f, venv_f) -> (
                let val_list = interpret_list menv venv param_list in
                let loc_list = ref_list var_list in
                let venv_new = (* venv_f{ v_1|->l_1, ..., v_n|->l_n } *) in
                  interpret menv venv_new abstr_noid
              )
        )

    | Apply(f, RealID(i), param_list) -> (
          match (* !(menv(f)) *) with
            Function(var_list, abstr_noid, abstr_id, menv_f, venv_f) -> (
                let val_list = interpret_list menv venv param_list in
                let loc_list = ref_list var_list in
                let loc_id = ref i in
                let venv_new = (* venv_f{ v_1|->l_1, ..., v_n|->l_n, @id|->loc_id } *) in
                  interpret menv venv_new abstr_id
              )
        )

  and interpret_list menv venv abstr_list =
    match abstr_list with
      [] -> []
    | abstr_head :: abstr_tail -> (
    	    let intrprtd_head = interpret abstr_head in
    	    let intrprtd_tail = interpret_list abstr_tail in
            intrprtd_head :: intrprtd_tail
        )

end