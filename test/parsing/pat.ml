open P

let%expect_test _ =
  p {|
module Pats = struct
  val pats-test1 =
    match a with
    % | b when c -> d
    | b -> d
    end
end
|}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-12>), "Pats"), None,
          [((Range.Normal <:3.3-6>),
            (UTBindValue (Stage1,
               (UTNonRec
                  (((Range.Normal <:3.7-17>), "pats-test1"),
                   (UTPatternMatch (
                      (UTContentOf ([], ((Range.Normal <:4.11-12>), "a"))),
                      [(UTPatternBranch (
                          ((Range.Normal <:6.7-8>), (UTPVariable "b")),
                          (UTContentOf ([], ((Range.Normal <:6.12-13>), "d")))
                          ))
                        ]
                      ))))
               )))
            ]))) |}]

let%expect_test _ =
  p {|
module Pats = struct
  val pats-test2 =
    match a with
    | b -> d
    % | b when c -> d
    end
end
|}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-12>), "Pats"), None,
          [((Range.Normal <:3.3-6>),
            (UTBindValue (Stage1,
               (UTNonRec
                  (((Range.Normal <:3.7-17>), "pats-test2"),
                   (UTPatternMatch (
                      (UTContentOf ([], ((Range.Normal <:4.11-12>), "a"))),
                      [(UTPatternBranch (
                          ((Range.Normal <:5.7-8>), (UTPVariable "b")),
                          (UTContentOf ([], ((Range.Normal <:5.12-13>), "d")))
                          ))
                        ]
                      ))))
               )))
            ]))) |}]

let%expect_test _ =
  p {|
module Patlist = struct
  val pat-test =
    match a with
    | [b] -> c
    | [b, c] -> d
    | [b, c,] -> d
    end
end
|}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-15>), "Patlist"), None,
          [((Range.Normal <:3.3-6>),
            (UTBindValue (Stage1,
               (UTNonRec
                  (((Range.Normal <:3.7-15>), "pat-test"),
                   (UTPatternMatch (
                      (UTContentOf ([], ((Range.Normal <:4.11-12>), "a"))),
                      [(UTPatternBranch (
                          ((Range.Normal <:5.7-10>),
                           (UTPListCons (
                              ((Range.Normal <:5.8-9>), (UTPVariable "b")),
                              ((Range.Dummy "list-pattern-nil"), UTPEndOfList)
                              ))),
                          (UTContentOf ([], ((Range.Normal <:5.14-15>), "c")))
                          ));
                        (UTPatternBranch (
                           ((Range.Normal <:6.7-13>),
                            (UTPListCons (
                               ((Range.Normal <:6.8-9>), (UTPVariable "b")),
                               ((Range.Dummy "list-pattern-cons"),
                                (UTPListCons (
                                   ((Range.Normal <:6.11-12>),
                                    (UTPVariable "c")),
                                   ((Range.Dummy "list-pattern-nil"),
                                    UTPEndOfList)
                                   )))
                               ))),
                           (UTContentOf ([], ((Range.Normal <:6.17-18>), "d")))
                           ));
                        (UTPatternBranch (
                           ((Range.Normal <:7.7-14>),
                            (UTPListCons (
                               ((Range.Normal <:7.8-9>), (UTPVariable "b")),
                               ((Range.Dummy "list-pattern-cons"),
                                (UTPListCons (
                                   ((Range.Normal <:7.11-12>),
                                    (UTPVariable "c")),
                                   ((Range.Dummy "list-pattern-nil"),
                                    UTPEndOfList)
                                   )))
                               ))),
                           (UTContentOf ([], ((Range.Normal <:7.18-19>), "d")))
                           ))
                        ]
                      ))))
               )))
            ]))) |}]

let%expect_test _ =
  p {|
module Pattuple = struct
  val pattuple-test =
    match a with
    | (b, c) -> d
    end
end
|}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-16>), "Pattuple"), None,
          [((Range.Normal <:3.3-6>),
            (UTBindValue (Stage1,
               (UTNonRec
                  (((Range.Normal <:3.7-20>), "pattuple-test"),
                   (UTPatternMatch (
                      (UTContentOf ([], ((Range.Normal <:4.11-12>), "a"))),
                      [(UTPatternBranch (
                          ((Range.Normal <:5.7-13>),
                           (UTPTuple
                              ((Range.Normal <:5.8-9>), (UTPVariable "b"))
                              ((Range.Normal <:5.11-12>), (UTPVariable "c")) )),
                          (UTContentOf ([], ((Range.Normal <:5.17-18>), "d")))
                          ))
                        ]
                      ))))
               )))
            ]))) |}]

let%expect_test _ =
  p {|
module PatRecord = struct
  val patrecord-test (| x = x, y = y |) =
    (x, y)
end
|}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-17>), "PatRecord"), None,
          [((Range.Normal <:3.3-6>),
            (UTBindValue (Stage1,
               (UTNonRec
                  (((Range.Normal <:3.7-21>), "patrecord-test"),
                   (UTFunction (
                      (UTParameterUnit ([],
                         ((Range.Normal <:3.22-40>),
                          (UTPRecord (false,
                             [(((Range.Normal <:3.25-26>), "x"),
                               ((Range.Normal <:3.29-30>), (UTPVariable "x")));
                               (((Range.Normal <:3.32-33>), "y"),
                                ((Range.Normal <:3.36-37>), (UTPVariable "y")))
                               ]
                             ))),
                         None)),
                      (UTTuple
                         (UTContentOf ([], ((Range.Normal <:4.6-7>), "x")))
                         (UTContentOf ([], ((Range.Normal <:4.9-10>), "y"))) )
                      ))))
               )))
            ]))) |}]

let%expect_test _ =
  p {|
module PatRecord = struct
  val patrecord-test (| x, y |) =
    (x, y)
end
|}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-17>), "PatRecord"), None,
          [((Range.Normal <:3.3-6>),
            (UTBindValue (Stage1,
               (UTNonRec
                  (((Range.Normal <:3.7-21>), "patrecord-test"),
                   (UTFunction (
                      (UTParameterUnit ([],
                         ((Range.Normal <:3.22-32>),
                          (UTPRecord (false,
                             [(((Range.Normal <:3.25-26>), "x"),
                               ((Range.Normal <:3.25-26>), (UTPVariable "x")));
                               (((Range.Normal <:3.28-29>), "y"),
                                ((Range.Normal <:3.28-29>), (UTPVariable "y")))
                               ]
                             ))),
                         None)),
                      (UTTuple
                         (UTContentOf ([], ((Range.Normal <:4.6-7>), "x")))
                         (UTContentOf ([], ((Range.Normal <:4.9-10>), "y"))) )
                      ))))
               )))
            ]))) |}]

let%expect_test _ =
  p {|
module PatRecord = struct
  val patrecord-test (| x, y, _ |) =
    (x, y)
end
|}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-17>), "PatRecord"), None,
          [((Range.Normal <:3.3-6>),
            (UTBindValue (Stage1,
               (UTNonRec
                  (((Range.Normal <:3.7-21>), "patrecord-test"),
                   (UTFunction (
                      (UTParameterUnit ([],
                         ((Range.Normal <:3.22-35>),
                          (UTPRecord (true,
                             [(((Range.Normal <:3.25-26>), "x"),
                               ((Range.Normal <:3.25-26>), (UTPVariable "x")));
                               (((Range.Normal <:3.28-29>), "y"),
                                ((Range.Normal <:3.28-29>), (UTPVariable "y")))
                               ]
                             ))),
                         None)),
                      (UTTuple
                         (UTContentOf ([], ((Range.Normal <:4.6-7>), "x")))
                         (UTContentOf ([], ((Range.Normal <:4.9-10>), "y"))) )
                      ))))
               )))
            ]))) |}]
