open Types

let add_to_type_environment tyenv varnm tystr =
  Hashtbl.add tyenv varnm tystr

let make_type_environment () =
  let tyenv : type_environment = Hashtbl.create 128 in
    add_to_type_environment tyenv "same"
      (FuncType(StringType, FuncType(StringType, BoolType))) ;
    add_to_type_environment tyenv "string-sub"
      (FuncType(StringType, FuncType(IntType, FuncType(IntType, StringType)))) ;
    add_to_type_environment tyenv "string-length"
      (FuncType(StringType, IntType)) ;
    add_to_type_environment tyenv "class-is-valid" BoolType ;
    add_to_type_environment tyenv "id-is-valid" BoolType ;
    add_to_type_environment tyenv "\\deeper"
      (FuncType(StringType, StringType)) ;
    add_to_type_environment tyenv "\\break"
      StringType ;
    add_to_type_environment tyenv "\\space"
      StringType ;
    add_to_type_environment tyenv "\\include"
      (FuncType(StringType, StringType)) ;
    add_to_type_environment tyenv "\\arabic"
      (FuncType(IntType, StringType)) ;
    add_to_type_environment tyenv "list-head"
      (FuncType(ListType(StringType), StringType)) ;
    add_to_type_environment tyenv "list-tail"
      (FuncType(ListType(StringType), ListType(StringType))) ;
    add_to_type_environment tyenv "is-empty"
      (FuncType(ListType(StringType), BoolType)) ;
    tyenv

let add_to_environment env varnm rfast =
  Hashtbl.add env varnm rfast

let make_environment () =
  let loc_same : location = ref NoContent in
  let loc_stringsub : location = ref NoContent in
  let loc_stringlength : location = ref NoContent in
  let loc_classisvalid : location = ref NoContent in
  let loc_idisvalid : location = ref NoContent in
  let loc_deeper : location = ref NoContent in
  let loc_break : location = ref NoContent in
  let loc_space : location = ref NoContent in
  let loc_include : location = ref NoContent in
  let loc_arabic : location = ref NoContent in
  let loc_listhead : location = ref NoContent in
  let loc_listtail : location = ref NoContent in
  let loc_isempty : location = ref NoContent in
  let env : environment = Hashtbl.create 128 in
    add_to_environment env "same" loc_same ;
    add_to_environment env "string-sub" loc_stringsub ;
    add_to_environment env "string-length" loc_stringlength ;
    add_to_environment env "class-is-valid" loc_classisvalid ;
    add_to_environment env "id-is-valid" loc_idisvalid ;
    add_to_environment env "\\deeper" loc_deeper ;
    add_to_environment env "\\break" loc_break ;
    add_to_environment env "\\space" loc_space ;
    add_to_environment env "\\include" loc_include ;
    add_to_environment env "\\arabic" loc_arabic ;
    add_to_environment env "list-head" loc_listhead ;
    add_to_environment env "list-tail" loc_listtail ;
    add_to_environment env "is-empty" loc_isempty ;
    loc_same := FuncWithEnvironment("~stra", 
                  LambdaAbstract("~strb",
                    PrimitiveSame(ContentOf("~stra"), ContentOf("~strb"))
                  ),
                  env
                ) ;
    loc_stringsub :=  FuncWithEnvironment("~str", 
                        LambdaAbstract("~pos",
                          LambdaAbstract("~wid",
                            PrimitiveStringSub(ContentOf("~str"), ContentOf("~pos"), ContentOf("~wid"))
                          )
                        ),
                        env
                      ) ;
    loc_stringlength := FuncWithEnvironment("~str",
                          PrimitiveStringLength(ContentOf("~str")),
                          env
                        ) ;
    loc_classisvalid := PrimitiveClassIsValid ;
    loc_idisvalid := PrimitiveIDIsValid ;

    loc_deeper := FuncWithEnvironment("~content",
                    Concat(DeeperIndent(Concat(BreakAndIndent, ContentOf("~content"))), BreakAndIndent),
                    env
                  ) ;
    loc_break  := BreakAndIndent ;
    loc_space  := StringConstant(" ") ;

    loc_include  := FuncWithEnvironment("~filename",
                      PrimitiveInclude(ContentOf("~filename")),
                      env
                    ) ;
    loc_arabic := FuncWithEnvironment("~num",
                    PrimitiveArabic(ContentOf("~num")),
                    env
                  ) ;
    loc_listhead := FuncWithEnvironment("~list",
                      PrimitiveListHead(ContentOf("~list")),
                      env
                    ) ;
    loc_listtail := FuncWithEnvironment("~list",
                      PrimitiveListTail(ContentOf("~list")),
                      env
                    ) ;
    loc_isempty :=  FuncWithEnvironment("~list",
                      PrimitiveIsEmpty(ContentOf("~list")),
                      env
                    ) ;
    env
