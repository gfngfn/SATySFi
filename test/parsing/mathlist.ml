open P

let%expect_test _ =
  p {test|
module Mathlist = struct
  val x = ${|}
end
|test}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-16>), "Mathlist"), None,
          [((Range.Normal <:3.3-6>),
            (UTBindValue (Stage1,
               (UTNonRec (((Range.Normal <:3.7-8>), "x"), UTEndOfList)))))
            ]))) |}]

let%expect_test _ =
  p {test|
module Mathlist = struct
  val y = ${|a|ab|}
end
|test}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-16>), "Mathlist"), None,
          [((Range.Normal <:3.3-6>),
            (UTBindValue (Stage1,
               (UTNonRec
                  (((Range.Normal <:3.7-8>), "y"),
                   (UTListCons (
                      (UTMathText
                         [((Range.Normal <:3.14-15>),
                           UTMathTextElement {base = (UTMathTextChar "a");
                             sup = None; sub = None})
                           ]),
                      (UTListCons (
                         (UTMathText
                            [((Range.Normal <:3.16-17>),
                              UTMathTextElement {base = (UTMathTextChar "a");
                                sup = None; sub = None});
                              ((Range.Normal <:3.17-18>),
                               UTMathTextElement {base = (UTMathTextChar "b");
                                 sup = None; sub = None})
                              ]),
                         UTEndOfList))
                      ))))
               )))
            ]))) |}]

let%expect_test _ =
  p {test|
module Mathlist = struct
  val z = '<
    +p {
      ${a}
      ${a^1}
      ${a_2}
      ${a_2^1}
      ${a^1_2}
      ${a'}
      ${a'^1}
      ${a'_2}
      ${a'_2^1}
      ${a'^1_2}
    }
  >
end
|test}; [%expect{|
  (Ok (UTLibraryFile
         (((Range.Normal <:2.8-16>), "Mathlist"), None,
          [((Range.Normal <:3.3-6>),
            (UTBindValue (Stage1,
               (UTNonRec
                  (((Range.Normal <:3.7-8>), "z"),
                   (UTBlockText
                      [BC:(UTContentOf ([], ((Range.Normal <:4.5-7>), "+p"))) (
                        UTCommandArg ([],
                          (UTInlineText
                             [(UTInlineTextEmbeddedMath
                                 (UTMathText
                                    [((Range.Normal <:5.9-10>),
                                      UTMathTextElement {
                                        base = (UTMathTextChar "a");
                                        sup = None; sub = None})
                                      ]));
                               IT:
  ;
                               (UTInlineTextEmbeddedMath
                                  (UTMathText
                                     [((Range.Normal <:6.9-12>),
                                       UTMathTextElement {
                                         base = (UTMathTextChar "a");
                                         sup =
                                         (Some (false,
                                                [((Range.Normal <:6.11-12>),
                                                  UTMathTextElement {
                                                    base = (UTMathTextChar "1");
                                                    sup = None; sub = None})
                                                  ]));
                                         sub = None})
                                       ]));
                               IT:
  ;
                               (UTInlineTextEmbeddedMath
                                  (UTMathText
                                     [((Range.Normal <:7.9-12>),
                                       UTMathTextElement {
                                         base = (UTMathTextChar "a");
                                         sup = None;
                                         sub =
                                         (Some (false,
                                                [((Range.Normal <:7.11-12>),
                                                  UTMathTextElement {
                                                    base = (UTMathTextChar "2");
                                                    sup = None; sub = None})
                                                  ]))})
                                       ]));
                               IT:
  ;
                               (UTInlineTextEmbeddedMath
                                  (UTMathText
                                     [((Range.Normal <:8.9-14>),
                                       UTMathTextElement {
                                         base = (UTMathTextChar "a");
                                         sup =
                                         (Some (false,
                                                [((Range.Normal <:8.13-14>),
                                                  UTMathTextElement {
                                                    base = (UTMathTextChar "1");
                                                    sup = None; sub = None})
                                                  ]));
                                         sub =
                                         (Some (false,
                                                [((Range.Normal <:8.11-12>),
                                                  UTMathTextElement {
                                                    base = (UTMathTextChar "2");
                                                    sup = None; sub = None})
                                                  ]))})
                                       ]));
                               IT:
  ;
                               (UTInlineTextEmbeddedMath
                                  (UTMathText
                                     [((Range.Normal <:9.9-14>),
                                       UTMathTextElement {
                                         base = (UTMathTextChar "a");
                                         sup =
                                         (Some (false,
                                                [((Range.Normal <:9.11-12>),
                                                  UTMathTextElement {
                                                    base = (UTMathTextChar "1");
                                                    sup = None; sub = None})
                                                  ]));
                                         sub =
                                         (Some (false,
                                                [((Range.Normal <:9.13-14>),
                                                  UTMathTextElement {
                                                    base = (UTMathTextChar "2");
                                                    sup = None; sub = None})
                                                  ]))})
                                       ]));
                               IT:
  ;
                               (UTInlineTextEmbeddedMath
                                  (UTMathText
                                     [((Range.Normal <:10.9-11>),
                                       UTMathTextElement {
                                         base = (UTMathTextChar "a");
                                         sup =
                                         (Some (true,
                                                [((Range.Normal <:10.10-11>),
                                                  UTMathTextElement {
                                                    base =
                                                    (UTMathTextChar "′");
                                                    sup = None; sub = None})
                                                  ]));
                                         sub = None})
                                       ]));
                               IT:
  ;
                               (UTInlineTextEmbeddedMath
                                  (UTMathText
                                     [((Range.Normal <:11.9-13>),
                                       UTMathTextElement {
                                         base = (UTMathTextChar "a");
                                         sup =
                                         (Some (false,
                                                [((Range.Normal <:11.10-11>),
                                                  UTMathTextElement {
                                                    base =
                                                    (UTMathTextChar "′");
                                                    sup = None; sub = None});
                                                  ((Range.Normal <:11.12-13>),
                                                   UTMathTextElement {
                                                     base =
                                                     (UTMathTextChar "1");
                                                     sup = None; sub = None})
                                                  ]));
                                         sub = None})
                                       ]));
                               IT:
  ;
                               (UTInlineTextEmbeddedMath
                                  (UTMathText
                                     [((Range.Normal <:12.9-13>),
                                       UTMathTextElement {
                                         base = (UTMathTextChar "a");
                                         sup =
                                         (Some (true,
                                                [((Range.Normal <:12.10-11>),
                                                  UTMathTextElement {
                                                    base =
                                                    (UTMathTextChar "′");
                                                    sup = None; sub = None})
                                                  ]));
                                         sub =
                                         (Some (false,
                                                [((Range.Normal <:12.12-13>),
                                                  UTMathTextElement {
                                                    base = (UTMathTextChar "2");
                                                    sup = None; sub = None})
                                                  ]))})
                                       ]));
                               IT:
  ;
                               (UTInlineTextEmbeddedMath
                                  (UTMathText
                                     [((Range.Normal <:13.9-15>),
                                       UTMathTextElement {
                                         base = (UTMathTextChar "a");
                                         sup =
                                         (Some (false,
                                                [((Range.Normal <:13.10-11>),
                                                  UTMathTextElement {
                                                    base =
                                                    (UTMathTextChar "′");
                                                    sup = None; sub = None});
                                                  ((Range.Normal <:13.14-15>),
                                                   UTMathTextElement {
                                                     base =
                                                     (UTMathTextChar "1");
                                                     sup = None; sub = None})
                                                  ]));
                                         sub =
                                         (Some (false,
                                                [((Range.Normal <:13.12-13>),
                                                  UTMathTextElement {
                                                    base = (UTMathTextChar "2");
                                                    sup = None; sub = None})
                                                  ]))})
                                       ]));
                               IT:
  ;
                               (UTInlineTextEmbeddedMath
                                  (UTMathText
                                     [((Range.Normal <:14.9-15>),
                                       UTMathTextElement {
                                         base = (UTMathTextChar "a");
                                         sup =
                                         (Some (false,
                                                [((Range.Normal <:14.10-11>),
                                                  UTMathTextElement {
                                                    base =
                                                    (UTMathTextChar "′");
                                                    sup = None; sub = None});
                                                  ((Range.Normal <:14.12-13>),
                                                   UTMathTextElement {
                                                     base =
                                                     (UTMathTextChar "1");
                                                     sup = None; sub = None})
                                                  ]));
                                         sub =
                                         (Some (false,
                                                [((Range.Normal <:14.14-15>),
                                                  UTMathTextElement {
                                                    base = (UTMathTextChar "2");
                                                    sup = None; sub = None})
                                                  ]))})
                                       ]))
                               ])
                          ))
                        ])))
               )))
            ]))) |}]
