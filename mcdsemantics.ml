
exception IllegalLengthOfLists
exception ValueNotFound

module AssocList : sig

  type ('a, 'b) t

  val empty : ('a, 'b) t
  val add : 'a -> 'b -> (('a, 'b) t) -> (('a, 'b) t)
  val add_list : ('a list) -> ('b list) -> (('a, 'b) t) -> (('a, 'b) t)
  val get_value : (('a, 'b) t) -> 'a -> 'b

end = struct

  type ('a, 'b) t = ('a * 'b) list

  let empty = []

  let rec add key value asclst =
    match asclst with
      [] -> [(key, value)]
    | (key, v) :: tail -> (key, value) :: tail
    | (k, v) :: tail -> (k, v) :: (add key value tail)

  let rec add_list key_list value_list asclst =
    match (key_list, value_list) with
      ([], []) -> empty
    | (key_head :: key_tail, value_head :: value_tail) -> (
    	  	(add_list key_tail value_tail (add key_head value_head asclst))
    	  )
    | _ -> raise IllegalLengthOfLists

  let rec get_value asclst key =
    match asclst with
      [] -> raise ValueNotFound
    | (key, value) :: tail -> value
    | (k, v) :: tail -> get_value tail key

end

module McdSemantics (* : sig

end *) = struct

  type var_environment = (var_name, abstract_tree) AssocList.t
  type macro_environment = (macro_name, function_spec) AssocList.t
  and function_spec = Func of (var_name list) * abstract_tree * abstract_tree * macro_environment * var_environment

  (* abstract_tree -> abstract_tree *)
  let rec semantics abstr =
    let menv_main : macro_environment ref = ref AssocList.empty in
    let venv_main : var_environment ref = ref AssocList.empty in
      interpret menv_main venv_main abstr

  (* (macro_environment ref) -> (var_environment ref) -> abstract_tree -> abstract_tree *)
  and interpret menv venv abstr =

    match abstr with

      EmptyAbsBlock -> EmptyAbsBlock

    | AbsBlock(abstr_head, abstr_tail) -> (
          let value_head = interpret menv venv abstr_head in
          let value_tail = interpret menv venv abstr_tail in
            AbsBlock(value_head, value_tail)
        )

    | Output(c) -> Output(c)

    | ContentOf(v) -> AssocList.get_value (!venv) v (*!venv(v)*)

    | Pop(u, v, Separated(abstr_former, abstr_latter), abstr_content) -> (
          let value_former = interpret menv venv abstr_former in
          let value_latter = interpret menv venv abstr_latter in
          let loc_former = ref value_former in
          let loc_latter = ref value_latter in
          let venv_content = ref (AssocList.add v loc_latter (AssocList.add u loc_former !venv)) in
          (* venv{ u |-> loc_former, v |-> loc_latter } *)
            interpret menv venv_content abstr_content
        )
    | Pop(u, v, EmptyAbsBlock, abstr_content) -> EmptyAbsBlock
    | Pop(u, v, abstr_former, abstr_content) -> (
          let value_former = interpret menv venv abstr_former in
          let loc_former = ref value_former in
          let loc_latter = ref EmptyAbsBlock in
          let venv_content = ref (AssocList.add v loc_latter (AssocList.add u loc_former !venv)) in
          (* venv{ u|->loc_former, v|->loc_latter } *)
            interpret menv venv_content abstr_content
        )

    | Macro(f, var_list, abstr_noid, abstr_id) -> (
          let loc = ref EmptyAbsBlock in
          let menv_new = ref (AssocList.add f loc !menv) in
          (* menv{ f|->loc } *)
          let value = Func(var_list, abstr_noid, abstr_id, menv_new, venv) in
            ( loc := value ; menv := !menv_new ; EmptyAbsBlock )
        )

    | Apply(f, NoID, param_list) -> (
          match (AssocList.get_value (!menv) f) with (* !(menv(f)) *)
            Func(var_list, abstr_noid, abstr_id, menv_f, venv_f) -> (
                let value_list = interpret_list menv venv param_list in
                let loc_list = ref_list var_list in
                let venv_new = ref (AssocList.add_list var_list param_list !venv_f) in
                (* venv_f{ v_1|->l_1, ..., v_n|->l_n } *)
                  interpret menv venv_new abstr_noid
              )
        )
    | Apply(f, RealID(i), param_list) -> (
          match (AssocList.get_value (!menv) f) with (* !(menv(f)) *)
            Func(var_list, abstr_noid, abstr_id, menv_f, venv_f) -> (
                let value_list = interpret_list menv venv param_list in
                let loc_list = ref_list var_list in
                let loc_id = ref i in
                let venv_new = ref (AssocList.add "@id" loc_id (AssocList.add_list var_list param_list !venv_f)) in
                (* venv_f{ v_1|->l_1, ..., v_n|->l_n, @id|->loc_id } *)
                  interpret menv venv_new abstr_id
              )
        )

  (* macro_environment -> var_environment -> (abstract_tree list) -> (abstract_tree list) *)
  and interpret_list menv venv abstr_list =
    match abstr_list with
      [] -> []
    | abstr_head :: abstr_tail -> (
    	    let intrprtd_head = interpret abstr_head in
    	    let intrprtd_tail = interpret_list abstr_tail in
            intrprtd_head :: intrprtd_tail
        )

  (* var_name -> ref var_name *)
  and ref_list var_list =
    match var_list with
      [] -> []
    | var_head :: var_tail -> (ref var_head) :: (ref_list var_tail)

end