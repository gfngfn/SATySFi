open P

let%expect_test _ =
  p {|
module Nx = struct
  val xs = [1, 2, 3]
end
|}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-10>), "Nx"), None,
          [((Range.Normal <:3.3-6>),
            (UTBindValue (Stage1,
               (UTNonRec
                  (((Range.Normal <:3.7-9>), "xs"),
                   (UTListCons ((UTIntegerConstant 1),
                      (UTListCons ((UTIntegerConstant 2),
                         (UTListCons ((UTIntegerConstant 3), UTEndOfList))))
                      ))))
               )))
            ]))) |}]

let%expect_test _ =
  p {|
module Nx = struct
  val ys = [1, 2, 3,]
end
|}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-10>), "Nx"), None,
          [((Range.Normal <:3.3-6>),
            (UTBindValue (Stage1,
               (UTNonRec
                  (((Range.Normal <:3.7-9>), "ys"),
                   (UTListCons ((UTIntegerConstant 1),
                      (UTListCons ((UTIntegerConstant 2),
                         (UTListCons ((UTIntegerConstant 3), UTEndOfList))))
                      ))))
               )))
            ]))) |}]

let%expect_test _ =
  p {|
module Nx = struct
  val r0 = (| |)
end
|}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-10>), "Nx"), None,
          [((Range.Normal <:3.3-6>),
            (UTBindValue (Stage1,
               (UTNonRec (((Range.Normal <:3.7-9>), "r0"), (UTRecord []))))))
            ]))) |}]

let%expect_test _ =
  p {|
module Nx = struct
  val r1 = (| x = 1 |)
end
|}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-10>), "Nx"), None,
          [((Range.Normal <:3.3-6>),
            (UTBindValue (Stage1,
               (UTNonRec
                  (((Range.Normal <:3.7-9>), "r1"),
                   (UTRecord
                      [(((Range.Normal <:3.15-16>), "x"), (UTIntegerConstant 1))
                        ])))
               )))
            ]))) |}]

let%expect_test _ =
  p {|
module Nx = struct
  val r2     = (| x = 1, y = 2 |)
end
|}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-10>), "Nx"), None,
          [((Range.Normal <:3.3-6>),
            (UTBindValue (Stage1,
               (UTNonRec
                  (((Range.Normal <:3.7-9>), "r2"),
                   (UTRecord
                      [(((Range.Normal <:3.19-20>), "x"), (UTIntegerConstant 1));
                        (((Range.Normal <:3.26-27>), "y"),
                         (UTIntegerConstant 2))
                        ])))
               )))
            ]))) |}]

let%expect_test _ =
  p {|
module Nx = struct
  val r2semi = (| x = 1, y = 2, |)
end
|}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-10>), "Nx"), None,
          [((Range.Normal <:3.3-6>),
            (UTBindValue (Stage1,
               (UTNonRec
                  (((Range.Normal <:3.7-13>), "r2semi"),
                   (UTRecord
                      [(((Range.Normal <:3.19-20>), "x"), (UTIntegerConstant 1));
                        (((Range.Normal <:3.26-27>), "y"),
                         (UTIntegerConstant 2))
                        ])))
               )))
            ]))) |}]

let%expect_test _ =
  p {|
module Nx = struct
  val tp2 = (1, 2)
end
|}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-10>), "Nx"), None,
          [((Range.Normal <:3.3-6>),
            (UTBindValue (Stage1,
               (UTNonRec
                  (((Range.Normal <:3.7-10>), "tp2"),
                   (UTTuple (UTIntegerConstant 1) (UTIntegerConstant 2) )))
               )))
            ]))) |}]

let%expect_test _ =
  p {|
module Nx = struct
  val tp3 = (1, 2, 3)
end
|}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-10>), "Nx"), None,
          [((Range.Normal <:3.3-6>),
            (UTBindValue (Stage1,
               (UTNonRec
                  (((Range.Normal <:3.7-10>), "tp3"),
                   (UTTuple (UTIntegerConstant 1) (UTIntegerConstant 2)
                      (UTIntegerConstant 3))))
               )))
            ]))) |}]

let%expect_test _ =
  p {|
module Nx = struct
  val op-test =
    ( a || b || c,
      a && b && c,
      a == b > c < d,
      a ^ b :: c,
      a + b + c,
      a - b -' c,
      a + b - c + d,
      a * b *' c / d mod e,
      - a * b,
      a || a && b ^ c == d > e < f ^ g :: h + i - j -' k * l *' m / n mod o,
      - a * b,
      not a * b,
      F a * b,
      A * b,
      a + f b,
      f -b,
      not a && b,
      a || not b,
      a * not b,
      a * F b,
      a * B,
      not a * b,
      F a * b,
      A * b
    )
end
|}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-10>), "Nx"), None,
          [((Range.Normal <:3.3-6>),
            (UTBindValue (Stage1,
               (UTNonRec
                  (((Range.Normal <:3.7-14>), "op-test"),
                   (UTTuple
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:4.14-16>), "||")
                               )),
                            (UTApply ([],
                               (UTApply ([],
                                  (UTContentOf ([],
                                     ((Range.Normal <:4.9-11>), "||"))),
                                  (UTContentOf ([],
                                     ((Range.Normal <:4.7-8>), "a")))
                                  )),
                               (UTContentOf ([],
                                  ((Range.Normal <:4.12-13>), "b")))
                               ))
                            )),
                         (UTContentOf ([], ((Range.Normal <:4.17-18>), "c")))))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:5.14-16>), "&&")
                               )),
                            (UTApply ([],
                               (UTApply ([],
                                  (UTContentOf ([],
                                     ((Range.Normal <:5.9-11>), "&&"))),
                                  (UTContentOf ([],
                                     ((Range.Normal <:5.7-8>), "a")))
                                  )),
                               (UTContentOf ([],
                                  ((Range.Normal <:5.12-13>), "b")))
                               ))
                            )),
                         (UTContentOf ([], ((Range.Normal <:5.17-18>), "c")))))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:6.9-11>), "==")
                               )),
                            (UTContentOf ([], ((Range.Normal <:6.7-8>), "a")))
                            )),
                         (UTApply ([],
                            (UTApply ([],
                               (UTContentOf ([],
                                  ((Range.Normal <:6.14-15>), ">"))),
                               (UTContentOf ([],
                                  ((Range.Normal <:6.12-13>), "b")))
                               )),
                            (UTApply ([],
                               (UTApply ([],
                                  (UTContentOf ([],
                                     ((Range.Normal <:6.18-19>), "<"))),
                                  (UTContentOf ([],
                                     ((Range.Normal <:6.16-17>), "c")))
                                  )),
                               (UTContentOf ([],
                                  ((Range.Normal <:6.20-21>), "d")))
                               ))
                            ))
                         ))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:7.9-10>), "^"))),
                            (UTContentOf ([], ((Range.Normal <:7.7-8>), "a")))
                            )),
                         (UTApply ([],
                            (UTApply ([],
                               (UTContentOf ([],
                                  ((Range.Normal <:7.13-15>), "::"))),
                               (UTContentOf ([],
                                  ((Range.Normal <:7.11-12>), "b")))
                               )),
                            (UTContentOf ([], ((Range.Normal <:7.16-17>), "c")
                               ))
                            ))
                         ))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:8.9-10>), "+"))),
                            (UTContentOf ([], ((Range.Normal <:8.7-8>), "a")))
                            )),
                         (UTApply ([],
                            (UTApply ([],
                               (UTContentOf ([],
                                  ((Range.Normal <:8.13-14>), "+"))),
                               (UTContentOf ([],
                                  ((Range.Normal <:8.11-12>), "b")))
                               )),
                            (UTContentOf ([], ((Range.Normal <:8.15-16>), "c")
                               ))
                            ))
                         ))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:9.13-15>), "-'")
                               )),
                            (UTApply ([],
                               (UTApply ([],
                                  (UTContentOf ([],
                                     ((Range.Normal <:9.9-10>), "-"))),
                                  (UTContentOf ([],
                                     ((Range.Normal <:9.7-8>), "a")))
                                  )),
                               (UTContentOf ([],
                                  ((Range.Normal <:9.11-12>), "b")))
                               ))
                            )),
                         (UTContentOf ([], ((Range.Normal <:9.16-17>), "c")))))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:10.9-10>), "+")
                               )),
                            (UTContentOf ([], ((Range.Normal <:10.7-8>), "a")))
                            )),
                         (UTApply ([],
                            (UTApply ([],
                               (UTContentOf ([],
                                  ((Range.Normal <:10.17-18>), "+"))),
                               (UTApply ([],
                                  (UTApply ([],
                                     (UTContentOf ([],
                                        ((Range.Normal <:10.13-14>), "-"))),
                                     (UTContentOf ([],
                                        ((Range.Normal <:10.11-12>), "b")))
                                     )),
                                  (UTContentOf ([],
                                     ((Range.Normal <:10.15-16>), "c")))
                                  ))
                               )),
                            (UTContentOf ([], ((Range.Normal <:10.19-20>), "d")
                               ))
                            ))
                         ))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:11.9-10>), "*")
                               )),
                            (UTContentOf ([], ((Range.Normal <:11.7-8>), "a")))
                            )),
                         (UTApply ([],
                            (UTApply ([],
                               (UTContentOf ([],
                                  ((Range.Normal <:11.13-15>), "*'"))),
                               (UTContentOf ([],
                                  ((Range.Normal <:11.11-12>), "b")))
                               )),
                            (UTApply ([],
                               (UTApply ([],
                                  (UTContentOf ([],
                                     ((Range.Normal <:11.18-19>), "/"))),
                                  (UTContentOf ([],
                                     ((Range.Normal <:11.16-17>), "c")))
                                  )),
                               (UTApply ([],
                                  (UTApply ([],
                                     (UTContentOf ([],
                                        ((Range.Normal <:11.22-25>), "mod"))),
                                     (UTContentOf ([],
                                        ((Range.Normal <:11.20-21>), "d")))
                                     )),
                                  (UTContentOf ([],
                                     ((Range.Normal <:11.26-27>), "e")))
                                  ))
                               ))
                            ))
                         ))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:12.11-12>), "*")
                               )),
                            (UTApply ([],
                               (UTApply ([],
                                  (UTContentOf ([],
                                     ((Range.Normal <:12.7-8>), "-"))),
                                  (UTIntegerConstant 0))),
                               (UTContentOf ([],
                                  ((Range.Normal <:12.9-10>), "a")))
                               ))
                            )),
                         (UTContentOf ([], ((Range.Normal <:12.13-14>), "b")))
                         ))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:13.9-11>), "||")
                               )),
                            (UTContentOf ([], ((Range.Normal <:13.7-8>), "a")))
                            )),
                         (UTApply ([],
                            (UTApply ([],
                               (UTContentOf ([],
                                  ((Range.Normal <:13.14-16>), "&&"))),
                               (UTContentOf ([],
                                  ((Range.Normal <:13.12-13>), "a")))
                               )),
                            (UTApply ([],
                               (UTApply ([],
                                  (UTContentOf ([],
                                     ((Range.Normal <:13.23-25>), "=="))),
                                  (UTApply ([],
                                     (UTApply ([],
                                        (UTContentOf ([],
                                           ((Range.Normal <:13.19-20>), "^"))),
                                        (UTContentOf ([],
                                           ((Range.Normal <:13.17-18>), "b")))
                                        )),
                                     (UTContentOf ([],
                                        ((Range.Normal <:13.21-22>), "c")))
                                     ))
                                  )),
                               (UTApply ([],
                                  (UTApply ([],
                                     (UTContentOf ([],
                                        ((Range.Normal <:13.28-29>), ">"))),
                                     (UTContentOf ([],
                                        ((Range.Normal <:13.26-27>), "d")))
                                     )),
                                  (UTApply ([],
                                     (UTApply ([],
                                        (UTContentOf ([],
                                           ((Range.Normal <:13.32-33>), "<"))),
                                        (UTContentOf ([],
                                           ((Range.Normal <:13.30-31>), "e")))
                                        )),
                                     (UTApply ([],
                                        (UTApply ([],
                                           (UTContentOf ([],
                                              ((Range.Normal <:13.36-37>), "^")
                                              )),
                                           (UTContentOf ([],
                                              ((Range.Normal <:13.34-35>), "f")
                                              ))
                                           )),
                                        (UTApply ([],
                                           (UTApply ([],
                                              (UTContentOf ([],
                                                 ((Range.Normal <:13.40-42>),
                                                  "::")
                                                 )),
                                              (UTContentOf ([],
                                                 ((Range.Normal <:13.38-39>),
                                                  "g")
                                                 ))
                                              )),
                                           (UTApply ([],
                                              (UTApply ([],
                                                 (UTContentOf ([],
                                                    ((Range.Normal <:13.45-46>),
                                                     "+")
                                                    )),
                                                 (UTContentOf ([],
                                                    ((Range.Normal <:13.43-44>),
                                                     "h")
                                                    ))
                                                 )),
                                              (UTApply ([],
                                                 (UTApply ([],
                                                    (UTContentOf ([],
                                                       ((Range.Normal <:13.53-55>),
                                                        "-'")
                                                       )),
                                                    (UTApply ([],
                                                       (UTApply ([],
                                                          (UTContentOf (
                                                             [],
                                                             ((Range.Normal <:13.49-50>),
                                                              "-")
                                                             )),
                                                          (UTContentOf (
                                                             [],
                                                             ((Range.Normal <:13.47-48>),
                                                              "i")
                                                             ))
                                                          )),
                                                       (UTContentOf ([],
                                                          ((Range.Normal <:13.51-52>),
                                                           "j")
                                                          ))
                                                       ))
                                                    )),
                                                 (UTApply ([],
                                                    (UTApply ([],
                                                       (UTContentOf ([],
                                                          ((Range.Normal <:13.58-59>),
                                                           "*")
                                                          )),
                                                       (UTContentOf ([],
                                                          ((Range.Normal <:13.56-57>),
                                                           "k")
                                                          ))
                                                       )),
                                                    (UTApply ([],
                                                       (UTApply ([],
                                                          (UTContentOf (
                                                             [],
                                                             ((Range.Normal <:13.62-64>),
                                                              "*'")
                                                             )),
                                                          (UTContentOf (
                                                             [],
                                                             ((Range.Normal <:13.60-61>),
                                                              "l")
                                                             ))
                                                          )),
                                                       (UTApply ([],
                                                          (UTApply ([],
                                                             (UTContentOf (
                                                                [],
                                                                ((Range.Normal <:13.67-68>),
                                                                 "/")
                                                                )),
                                                             (UTContentOf (
                                                                [],
                                                                ((Range.Normal <:13.65-66>),
                                                                 "m")
                                                                ))
                                                             )),
                                                          (UTApply ([],
                                                             (UTApply (
                                                                [],
                                                                (UTContentOf (
                                                                   [],
                                                                   ((Range.Normal <:13.71-74>),
                                                                    "mod")
                                                                   )),
                                                                (UTContentOf (
                                                                   [],
                                                                   ((Range.Normal <:13.69-70>),
                                                                    "n")
                                                                   ))
                                                                )),
                                                             (UTContentOf (
                                                                [],
                                                                ((Range.Normal <:13.75-76>),
                                                                 "o")
                                                                ))
                                                             ))
                                                          ))
                                                       ))
                                                    ))
                                                 ))
                                              ))
                                           ))
                                        ))
                                     ))
                                  ))
                               ))
                            ))
                         ))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:14.11-12>), "*")
                               )),
                            (UTApply ([],
                               (UTApply ([],
                                  (UTContentOf ([],
                                     ((Range.Normal <:14.7-8>), "-"))),
                                  (UTIntegerConstant 0))),
                               (UTContentOf ([],
                                  ((Range.Normal <:14.9-10>), "a")))
                               ))
                            )),
                         (UTContentOf ([], ((Range.Normal <:14.13-14>), "b")))
                         ))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:15.13-14>), "*")
                               )),
                            (UTApply ([],
                               (UTContentOf ([],
                                  ((Range.Normal <:15.7-10>), "not"))),
                               (UTContentOf ([],
                                  ((Range.Normal <:15.11-12>), "a")))
                               ))
                            )),
                         (UTContentOf ([], ((Range.Normal <:15.15-16>), "b")))
                         ))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:16.11-12>), "*")
                               )),
                            (UTConstructor ("F",
                               (UTContentOf ([],
                                  ((Range.Normal <:16.9-10>), "a")))
                               ))
                            )),
                         (UTContentOf ([], ((Range.Normal <:16.13-14>), "b")))
                         ))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:17.9-10>), "*")
                               )),
                            (UTConstructor ("A", UTUnitConstant)))),
                         (UTContentOf ([], ((Range.Normal <:17.11-12>), "b")))
                         ))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:18.9-10>), "+")
                               )),
                            (UTContentOf ([], ((Range.Normal <:18.7-8>), "a")))
                            )),
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:18.11-12>), "f")
                               )),
                            (UTContentOf ([], ((Range.Normal <:18.13-14>), "b")
                               ))
                            ))
                         ))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:19.9-10>), "-")
                               )),
                            (UTContentOf ([], ((Range.Normal <:19.7-8>), "f")))
                            )),
                         (UTContentOf ([], ((Range.Normal <:19.10-11>), "b")))
                         ))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([],
                               ((Range.Normal <:20.13-15>), "&&"))),
                            (UTApply ([],
                               (UTContentOf ([],
                                  ((Range.Normal <:20.7-10>), "not"))),
                               (UTContentOf ([],
                                  ((Range.Normal <:20.11-12>), "a")))
                               ))
                            )),
                         (UTContentOf ([], ((Range.Normal <:20.16-17>), "b")))
                         ))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:21.9-11>), "||")
                               )),
                            (UTContentOf ([], ((Range.Normal <:21.7-8>), "a")))
                            )),
                         (UTApply ([],
                            (UTContentOf ([],
                               ((Range.Normal <:21.12-15>), "not"))),
                            (UTContentOf ([], ((Range.Normal <:21.16-17>), "b")
                               ))
                            ))
                         ))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:22.9-10>), "*")
                               )),
                            (UTContentOf ([], ((Range.Normal <:22.7-8>), "a")))
                            )),
                         (UTApply ([],
                            (UTContentOf ([],
                               ((Range.Normal <:22.11-14>), "not"))),
                            (UTContentOf ([], ((Range.Normal <:22.15-16>), "b")
                               ))
                            ))
                         ))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:23.9-10>), "*")
                               )),
                            (UTContentOf ([], ((Range.Normal <:23.7-8>), "a")))
                            )),
                         (UTConstructor ("F",
                            (UTContentOf ([], ((Range.Normal <:23.13-14>), "b")
                               ))
                            ))
                         ))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:24.9-10>), "*")
                               )),
                            (UTContentOf ([], ((Range.Normal <:24.7-8>), "a")))
                            )),
                         (UTConstructor ("B", UTUnitConstant))))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:25.13-14>), "*")
                               )),
                            (UTApply ([],
                               (UTContentOf ([],
                                  ((Range.Normal <:25.7-10>), "not"))),
                               (UTContentOf ([],
                                  ((Range.Normal <:25.11-12>), "a")))
                               ))
                            )),
                         (UTContentOf ([], ((Range.Normal <:25.15-16>), "b")))
                         ))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:26.11-12>), "*")
                               )),
                            (UTConstructor ("F",
                               (UTContentOf ([],
                                  ((Range.Normal <:26.9-10>), "a")))
                               ))
                            )),
                         (UTContentOf ([], ((Range.Normal <:26.13-14>), "b")))
                         ))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:27.9-10>), "*")
                               )),
                            (UTConstructor ("A", UTUnitConstant)))),
                         (UTContentOf ([], ((Range.Normal <:27.11-12>), "b")))
                         )))))
               )))
            ]))) |}]

let%expect_test _ =
  p {|
module Nx = struct
  val uminus = (-1, - 42, -1.0, - 3.14, -1mm, - 2.71828cm, -x, - x)
end
|}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-10>), "Nx"), None,
          [((Range.Normal <:3.3-6>),
            (UTBindValue (Stage1,
               (UTNonRec
                  (((Range.Normal <:3.7-13>), "uminus"),
                   (UTTuple
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:3.17-18>), "-")
                               )),
                            (UTIntegerConstant 0))),
                         (UTIntegerConstant 1)))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:3.21-22>), "-")
                               )),
                            (UTIntegerConstant 0))),
                         (UTIntegerConstant 42)))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:3.27-28>), "-.")
                               )),
                            (UTFloatConstant 0.))),
                         (UTFloatConstant 1.)))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:3.33-34>), "-.")
                               )),
                            (UTFloatConstant 0.))),
                         (UTFloatConstant 3.14)))
                      (UTLengthDescription (-1., "mm"))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:3.47-48>), "-'")
                               )),
                            (UTLengthDescription (0., "cm")))),
                         (UTLengthDescription (2.71828, "cm"))))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:3.60-61>), "-")
                               )),
                            (UTIntegerConstant 0))),
                         (UTContentOf ([], ((Range.Normal <:3.61-62>), "x")))))
                      (UTApply ([],
                         (UTApply ([],
                            (UTContentOf ([], ((Range.Normal <:3.64-65>), "-")
                               )),
                            (UTIntegerConstant 0))),
                         (UTContentOf ([], ((Range.Normal <:3.66-67>), "x"))))))))
               )))
            ]))) |}]

let%expect_test _ =
  p {|
module Nx = struct
  val rec-pun =
    let x = 0 in
    let y = 42 in
    let z = 3.14 in
    let xy = (| x, y |) in
    (| xy with z |)
end
|}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-10>), "Nx"), None,
          [((Range.Normal <:3.3-6>),
            (UTBindValue (Stage1,
               (UTNonRec
                  (((Range.Normal <:3.7-14>), "rec-pun"),
                   (UTLetIn (
                      (UTNonRec
                         (((Range.Normal <:4.9-10>), "x"),
                          (UTIntegerConstant 0))),
                      (UTLetIn (
                         (UTNonRec
                            (((Range.Normal <:5.9-10>), "y"),
                             (UTIntegerConstant 42))),
                         (UTLetIn (
                            (UTNonRec
                               (((Range.Normal <:6.9-10>), "z"),
                                (UTFloatConstant 3.14))),
                            (UTLetIn (
                               (UTNonRec
                                  (((Range.Normal <:7.9-11>), "xy"),
                                   (UTRecord
                                      [(((Range.Normal <:7.17-18>), "x"),
                                        (UTContentOf ([],
                                           ((Range.Normal <:7.17-18>), "x"))));
                                        (((Range.Normal <:7.20-21>), "y"),
                                         (UTContentOf ([],
                                            ((Range.Normal <:7.20-21>), "y"))))
                                        ]))),
                               (UTUpdateField (
                                  (UTContentOf ([],
                                     ((Range.Normal <:8.8-10>), "xy"))),
                                  ((Range.Normal <:8.16-17>), "z"),
                                  (UTContentOf ([],
                                     ((Range.Normal <:8.16-17>), "z")))
                                  ))
                               ))
                            ))
                         ))
                      ))))
               )))
            ]))) |}]
