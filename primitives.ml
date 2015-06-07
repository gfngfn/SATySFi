open Types

let add_to_type_environment tyenv varnm tystr =
  Hashtbl.add tyenv varnm tystr

let make_type_environment () =
  let tyenv : type_environment = Hashtbl.create 128 in
    add_to_type_environment tyenv "same"
      (FuncType(StringType, FuncType(StringType, BoolType))) ;
    add_to_type_environment tyenv "is-valid"
      (FuncType(StringType, BoolType)) ;
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
    tyenv

let add_to_environment env varnm rfast =
  Hashtbl.add env varnm rfast

let make_environment () =
  let loc_same : location = ref NoContent in
  let loc_isvalid : location = ref NoContent in
  let loc_deeper : location = ref NoContent in
  let loc_break : location = ref NoContent in
  let loc_space : location = ref NoContent in
  let loc_include : location = ref NoContent in
  let loc_arabic : location = ref NoContent in
  let env : environment = Hashtbl.create 128 in
    add_to_environment env "same" loc_same ;
    add_to_environment env "is-valid" loc_isvalid ;
    add_to_environment env "\\deeper" loc_deeper ;
    add_to_environment env "\\break" loc_break ;
    add_to_environment env "\\space" loc_space ;
    add_to_environment env "\\include" loc_include ;
    add_to_environment env "\\arabic" loc_arabic ;
    loc_same := FuncWithEnvironment("~stra", 
                  FuncWithEnvironment("~strb",
                    PrimitiveSame(ContentOf("~stra"), ContentOf("~strb")),
                    env
                  ),
                  env
                ) ;
    loc_isvalid := FuncWithEnvironment("~content",
                    PrimitiveIsValid(ContentOf("~content")),
                    env
                  ) ;
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
    env
