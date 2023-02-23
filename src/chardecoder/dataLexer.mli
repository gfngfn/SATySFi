val expr : Sedlexing.lexbuf -> DataParser.token
val parse : Sedlexing.lexbuf -> (CharBasis.code_point_kind * string) list