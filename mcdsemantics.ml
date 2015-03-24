
exception IllegalLengthOfLists
exception ValueNotFound

module AssocList (* : sig

  type ('a, 'b) t

  val empty : ('a, 'b) t
  val add : 'a -> 'b -> (('a, 'b) t) -> (('a, 'b) t)
  val add_list : ('a list) -> ('b list) -> (('a, 'b) t) -> (('a, 'b) t)
  val get_value : (('a, 'b) t) -> 'a -> 'b

end *) = struct

  type ('a, 'b) t = ('a * 'b) list

  let empty = []

  let rec add key value asclst =
  (*  (key, value) :: asclst *)
    match asclst with
      [] -> [(key, value)]
    | (k, v) :: tail -> (k, v) :: (add key value tail)

  (* 'a list -> 'b list -> ('a, 'b) t -> ('a, 'b) t *)
  let rec add_list key_list value_list asclst =
    match key_list with
      [] -> asclst
    | key_head :: key_tail -> (
    	    match value_list with
    	      [] -> raise IllegalLengthOfLists
    	    | value_head :: value_tail -> (
        	    let asclstsub = (add key_head value_head asclst) in
                (add_list key_tail value_tail asclstsub)
    	      )
        )

  (* ('a, 'b) t -> 'a -> 'b *)
  let rec get_value asclst key =
    match asclst with
      [] -> raise ValueNotFound
    | (k, v) :: tail ->
      if (compare k key) == 0 then v else get_value tail key

  (* for test *)
  let rec print_key asclst =
    match asclst with
      [] -> ()
    | (k, v) :: tail -> ( print_string k ; print_key tail )

end


module McdSemantics (* : sig

end *) = struct

  type location = abstract_tree ref
  type var_environment = (var_name, location) AssocList.t
  type macro_environment = (macro_name, macro_location) AssocList.t
  and function_spec = DummyFunc | Func of (var_name list) * abstract_tree * abstract_tree * macro_environment * var_environment
  and macro_location = function_spec ref

  let print_process stat =
    (* enable below in order to see the process of interpretation *)
  (*
    print_string stat ; print_newline () ;
  *)
    ()

  (* abstract_tree -> abstract_tree *)
  let rec semantics abstr =
    let menv_main : macro_environment ref = ref AssocList.empty in
    let venv_main : var_environment ref = ref AssocList.empty in
      interpret menv_main venv_main abstr

  (* (macro_environment ref) -> (var_environment ref) -> abstract_tree -> abstract_tree *)
  and interpret menv venv abstr =

    match abstr with

      EmptyAbsBlock -> (
          print_process "$EmptyAbsBlock" ;
          EmptyAbsBlock
        )

    | AbsBlock(abstr_head, abstr_tail) -> (
          print_process "$AbsBlock 2" ;
          let value_head = interpret menv venv abstr_head in
          let value_tail = interpret menv venv abstr_tail in
            AbsBlock(value_head, value_tail)
        )

    | Output(c) -> (
          print_process ("$Output: " ^ c) ;
          Output(c)
        )

    | ContentOf(v) -> (
          print_process ("$ContentOf: " ^ v) ;
          !(AssocList.get_value (!venv) v) (*!venv(v)*)
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
    	    | Separated(abstr_former, abstr_latter) -> (
                print_process "$Pop (Plural)" ;
                let value_former = interpret menv venv abstr_former in
                let value_latter = interpret menv venv abstr_latter in
                let loc_former : location = ref value_former in
                let loc_latter : location = ref value_latter in
                let venv_content = ref (AssocList.add v loc_latter (AssocList.add u loc_former !venv)) in
                (* venv{ u |-> loc_former, v |-> loc_latter } *)
                (*
                  print_string " ***( " ;
                  AssocList.print_key !venv_content ;
                  print_string " )***" ; print_newline () ;
                *)
                  interpret menv venv_content abstr_content
    	        )
    	    | abstr_former -> (
                print_process "$Pop (Single)" ;
                let value_former = interpret menv venv abstr_former in
                let loc_former : location = ref value_former in
                let loc_latter : location = ref EmptyAbsBlock in
                let venv_content = ref (AssocList.add v loc_latter (AssocList.add u loc_former !venv)) in
                (* venv{ u|->loc_former, v|->loc_latter } *)
                (*
                  print_string " **( " ;
                  AssocList.print_key !venv_content ;
                  print_string " )**" ; print_newline () ;
                *)
                  interpret menv venv_content abstr_content
    	        )
        )

    | Macro(f, var_list, abstr_noid, abstr_id) -> (
          print_process "$Macro" ;
          let loc : macro_location = ref DummyFunc in (* dummy *)
          let menv_new = ref (AssocList.add f loc !menv) in
          (* menv{ f|->loc } *)
          let value = Func(var_list, abstr_noid, abstr_id, !menv_new, !venv) in
            ( loc := value ; menv := !menv_new ; EmptyAbsBlock )
        )

    | Apply(f, NoID, param_list) -> (
          print_process "$Apply (NoID)" ;
          match !(AssocList.get_value (!menv) f) with (* !(menv(f)) *)
            Func(var_list, abstr_noid, abstr_id, cont_menv_f, cont_venv_f) -> (
                let value_list = interpret_list menv venv param_list in
                let loc_list : location list = ref_list value_list in
                let venv_new = ref (AssocList.add_list var_list loc_list cont_venv_f) in
                (* venv_f{ v_1|->l_1, ..., v_n|->l_n } *)
                (*
                  print_string " *|*( " ;
                  AssocList.print_key !venv_new ;
                  print_string " )*|*" ; print_newline () ;
                *)
                  interpret menv venv_new abstr_noid
              )
        )
    | Apply(f, RealID(i), param_list) -> (
          print_process "$Apply (ID)" ;
          match !(AssocList.get_value (!menv) f) with (* !(menv(f)) *)
            Func(var_list, abstr_noid, abstr_id, cont_menv_f, cont_venv_f) -> (
                let value_list = interpret_list menv venv param_list in
                let loc_list : location list = ref_list value_list in
                let loc_id : location = ref (id_to_abstract_tree i) in
                let venv_new = ref (AssocList.add "@id" loc_id (AssocList.add_list var_list loc_list cont_venv_f)) in
                (* venv_f{ v_1|->l_1, ..., v_n|->l_n, @id|->loc_id } *)
                (*
                  print_string " *|*( " ;
                  AssocList.print_key !venv_new ;
                  print_string " )*|*" ; print_newline () ;
                *)
                  interpret menv venv_new abstr_id
              )
        )

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

end