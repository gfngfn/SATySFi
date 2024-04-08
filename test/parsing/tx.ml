open P

let%expect_test _ =
  p {|
module Toplevel = struct
  type t = a -> b
  and s = A | B
  and u =
    | A
    | B
end
|}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-16>), "Toplevel"), None,
          [((Range.Normal <:3.3-7>),
            (UTBindType
               [(((Range.Normal <:3.8-9>), "t"), [],
                 (UTBindSynonym
                    ((Range.Normal <:3.12-18>),
                     (Types.MFuncType ([], None,
                        ((Range.Normal <:3.12-13>),
                         (Types.MTypeName ([],
                            ((Range.Normal <:3.12-13>), "a"), []))),
                        ((Range.Normal <:3.17-18>),
                         (Types.MTypeName ([],
                            ((Range.Normal <:3.17-18>), "b"), [])))
                        )))));
                 (((Range.Normal <:4.7-8>), "s"), [],
                  (UTBindVariant
                     [(UTConstructorBranch (((Range.Normal <:4.11-12>), "A"),
                         None));
                       (UTConstructorBranch (((Range.Normal <:4.15-16>), "B"),
                          None))
                       ]));
                 (((Range.Normal <:5.7-8>), "u"), [],
                  (UTBindVariant
                     [(UTConstructorBranch (((Range.Normal <:6.7-8>), "A"),
                         None));
                       (UTConstructorBranch (((Range.Normal <:7.7-8>), "B"),
                          None))
                       ]))
                 ]))
            ]))) |}]

let%expect_test _ =
  p {|
module Txlist = struct
  type t = inline [?(foo : u,) s,]
  and t = inline []
  and t = inline [s]
  and t = inline [s,] % cannot write this
end
|}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-14>), "Txlist"), None,
          [((Range.Normal <:3.3-7>),
            (UTBindType
               [(((Range.Normal <:3.8-9>), "t"), [],
                 (UTBindSynonym
                    ((Range.Normal <:3.12-35>),
                     (Types.MInlineCommandType
                        [(Types.MArgType (
                            [(((Range.Normal <:3.22-25>), "foo"),
                              ((Range.Normal <:3.28-29>),
                               (Types.MTypeName ([],
                                  ((Range.Normal <:3.28-29>), "u"), []))))
                              ],
                            ((Range.Normal <:3.32-33>),
                             (Types.MTypeName ([],
                                ((Range.Normal <:3.32-33>), "s"), [])))
                            ))
                          ]))));
                 (((Range.Normal <:4.7-8>), "t"), [],
                  (UTBindSynonym
                     ((Range.Normal <:4.11-20>), (Types.MInlineCommandType []))));
                 (((Range.Normal <:5.7-8>), "t"), [],
                  (UTBindSynonym
                     ((Range.Normal <:5.11-21>),
                      (Types.MInlineCommandType
                         [(Types.MArgType ([],
                             ((Range.Normal <:5.19-20>),
                              (Types.MTypeName ([],
                                 ((Range.Normal <:5.19-20>), "s"), [])))
                             ))
                           ]))));
                 (((Range.Normal <:6.7-8>), "t"), [],
                  (UTBindSynonym
                     ((Range.Normal <:6.11-22>),
                      (Types.MInlineCommandType
                         [(Types.MArgType ([],
                             ((Range.Normal <:6.19-20>),
                              (Types.MTypeName ([],
                                 ((Range.Normal <:6.19-20>), "s"), [])))
                             ))
                           ]))))
                 ]))
            ]))) |}]

let%expect_test _ =
  p {|
module Txprod = struct
  type t = a * b
  and s = a * b * c
end
|}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-14>), "Txprod"), None,
          [((Range.Normal <:3.3-7>),
            (UTBindType
               [(((Range.Normal <:3.8-9>), "t"), [],
                 (UTBindSynonym
                    ((Range.Normal <:3.12-17>),
                     (Types.MProductType
                        ((Range.Normal <:3.12-13>),
                         (Types.MTypeName ([],
                            ((Range.Normal <:3.12-13>), "a"), [])))
                        ((Range.Normal <:3.16-17>),
                         (Types.MTypeName ([],
                            ((Range.Normal <:3.16-17>), "b"), [])))
                        ))));
                 (((Range.Normal <:4.7-8>), "s"), [],
                  (UTBindSynonym
                     ((Range.Normal <:4.11-20>),
                      (Types.MProductType
                         ((Range.Normal <:4.11-12>),
                          (Types.MTypeName ([],
                             ((Range.Normal <:4.11-12>), "a"), [])))
                         ((Range.Normal <:4.15-16>),
                          (Types.MTypeName ([],
                             ((Range.Normal <:4.15-16>), "b"), [])))
                         ((Range.Normal <:4.19-20>),
                          (Types.MTypeName ([],
                             ((Range.Normal <:4.19-20>), "c"), [])))))))
                 ]))
            ]))) |}]

let%expect_test _ =
  p {|
module Txrecord = struct
  type t = (| x : a, y : b |)
  and t = (| x : a, y : b, |)
  and t = (| x : a |)
  % and t = (| |) % cannot write this
end
|}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-16>), "Txrecord"), None,
          [((Range.Normal <:3.3-7>),
            (UTBindType
               [(((Range.Normal <:3.8-9>), "t"), [],
                 (UTBindSynonym
                    ((Range.Normal <:3.12-30>),
                     (Types.MRecordType (
                        [(((Range.Normal <:3.15-16>), "x"),
                          ((Range.Normal <:3.19-20>),
                           (Types.MTypeName ([],
                              ((Range.Normal <:3.19-20>), "a"), []))));
                          (((Range.Normal <:3.22-23>), "y"),
                           ((Range.Normal <:3.26-27>),
                            (Types.MTypeName ([],
                               ((Range.Normal <:3.26-27>), "b"), []))))
                          ],
                        None)))));
                 (((Range.Normal <:4.7-8>), "t"), [],
                  (UTBindSynonym
                     ((Range.Normal <:4.11-30>),
                      (Types.MRecordType (
                         [(((Range.Normal <:4.14-15>), "x"),
                           ((Range.Normal <:4.18-19>),
                            (Types.MTypeName ([],
                               ((Range.Normal <:4.18-19>), "a"), []))));
                           (((Range.Normal <:4.21-22>), "y"),
                            ((Range.Normal <:4.25-26>),
                             (Types.MTypeName ([],
                                ((Range.Normal <:4.25-26>), "b"), []))))
                           ],
                         None)))));
                 (((Range.Normal <:5.7-8>), "t"), [],
                  (UTBindSynonym
                     ((Range.Normal <:5.11-22>),
                      (Types.MRecordType (
                         [(((Range.Normal <:5.14-15>), "x"),
                           ((Range.Normal <:5.18-19>),
                            (Types.MTypeName ([],
                               ((Range.Normal <:5.18-19>), "a"), []))))
                           ],
                         None)))))
                 ]))
            ]))) |}]

let%expect_test _ =
  p {|
module Variants = struct
  type t = X | Y
  and s = Z of t | W of u
end
|}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-16>), "Variants"), None,
          [((Range.Normal <:3.3-7>),
            (UTBindType
               [(((Range.Normal <:3.8-9>), "t"), [],
                 (UTBindVariant
                    [(UTConstructorBranch (((Range.Normal <:3.12-13>), "X"),
                        None));
                      (UTConstructorBranch (((Range.Normal <:3.16-17>), "Y"),
                         None))
                      ]));
                 (((Range.Normal <:4.7-8>), "s"), [],
                  (UTBindVariant
                     [(UTConstructorBranch (((Range.Normal <:4.11-12>), "Z"),
                         (Some ((Range.Normal <:4.16-17>),
                                (Types.MTypeName ([],
                                   ((Range.Normal <:4.16-17>), "t"), []))))
                         ));
                       (UTConstructorBranch (((Range.Normal <:4.20-21>), "W"),
                          (Some ((Range.Normal <:4.25-26>),
                                 (Types.MTypeName ([],
                                    ((Range.Normal <:4.25-26>), "u"), []))))
                          ))
                       ]))
                 ]))
            ]))) |}]
