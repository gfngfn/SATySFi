open P

let%expect_test _ =
  p {test|
module Sxlist = struct
  val x = {|}
end
|test}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-14>), "Sxlist"), None,
          [((Range.Normal <:3.3-6>),
            (UTBindValue (Stage1,
               (UTNonRec (((Range.Normal <:3.7-8>), "x"), UTEndOfList)))))
            ]))) |}]

let%expect_test _ =
  p {test|
module Sxlist = struct
  val y = {|aa|bb|}
end
|test}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-14>), "Sxlist"), None,
          [((Range.Normal <:3.3-6>),
            (UTBindValue (Stage1,
               (UTNonRec
                  (((Range.Normal <:3.7-8>), "y"),
                   (UTListCons ((UTInlineText [IT:aa]),
                      (UTListCons ((UTInlineText [IT:bb]), UTEndOfList))))))
               )))
            ]))) |}]
