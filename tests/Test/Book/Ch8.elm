module Test.Book.Ch8 exposing (suite)

import Book.Ch7 exposing (..)
import Book.Ch8 exposing (..)
import Logic exposing (..)
import Test exposing (Test, describe)
import Test.Book.Fixtures exposing (testExample, toOutput)


suite : Test
suite =
    describe "Book.Ch8" <|
        List.indexedMap
            testExample
            --
            -- timeso
            --
            [ { input = run3AtMost 10 timeso
              , output =
                    toOutput
                        [ "(() _0 ())"
                        , "((_0 . _1) () ())"
                        , "((1) (_0 . _1) (_0 . _1))"
                        , "((_0 _1 . _2) (1) (_0 _1 . _2))"
                        , "((0 1) (_0 _1 . _2) (0 _0 _1 . _2))"
                        , "((0 0 1) (_0 _1 . _2) (0 0 _0 _1 . _2))"
                        , "((1 _0 . _1) (0 1) (0 1 _0 . _1))"
                        , "((0 0 0 1) (_0 _1 . _2) (0 0 0 _0 _1 . _2))"
                        , "((1 _0 . _1) (0 0 1) (0 0 1 _0 . _1))"
                        , "((0 1 _0 . _1) (0 1) (0 0 1 _0 . _1))"
                        ]
              }
            , { input = run (timeso (list [ zero, one ]) (list [ zero, zero, one ]))
              , output = "((0 0 0 1))"
              }
            , { input =
                    run3AtMost 1
                        (\x y r ->
                            conj
                                [ equals (list [ x, y, r ]) (list [ list [ one, one ], list [ one, one ], list [ one, zero, zero, one ] ])
                                , timeso x y r
                                ]
                        )
              , output = "(((1 1) (1 1) (1 0 0 1)))"
              }
            , { input = run2AtMost 1 (\n m -> timeso n m (list [ one ]))
              , output = "(((1) (1)))"
              }
            , { input =
                    run2AtMost 1
                        (\n m ->
                            conj
                                [ greaterThan1o n
                                , greaterThan1o m
                                , timeso n m (list [ one, one ])
                                ]
                        )
              , output = "()"
              }
            , { input = run2AtMost 2 (\n m -> timeso n m (list [ one ]))
              , output = "(((1) (1)))"
              }
            , { input = run (timeso (list [ one, one, one ]) (list [ one, one, one, one, one, one ]))
              , output = "((1 0 0 1 1 1 0 1 1))"
              }

            --
            -- equalLo
            --
            , { input = run3 (\w x y -> equalLo (dottedList one [ w, x ] y) (list [ zero, one, one, zero, one ]))
              , output = "((_0 _1 (_2 1)))"
              }
            , { input = run (\b -> equalLo (list [ one ]) (list [ b ]))
              , output = "(1)"
              }
            , { input = run (\n -> equalLo (dottedList one [ zero, one ] n) (list [ zero, one, one, zero, one ]))
              , output = "((_0 1))"
              }
            , { input = run2AtMost 5 (\y z -> equalLo (cons one y) (cons one z))
              , output =
                    toOutput
                        [ "(() ())"
                        , "((1) (1))"
                        , "((_0 1) (_1 1))"
                        , "((_0 _1 1) (_2 _3 1))"
                        , "((_0 _1 _2 1) (_3 _4 _5 1))"
                        ]
              }
            , { input = run2AtMost 5 (\y z -> equalLo (cons one y) (cons zero z))
              , output =
                    toOutput
                        [ "((1) (1))"
                        , "((_0 1) (_1 1))"
                        , "((_0 _1 1) (_2 _3 1))"
                        , "((_0 _1 _2 1) (_3 _4 _5 1))"
                        , "((_0 _1 _2 _3 1) (_4 _5 _6 _7 1))"
                        ]
              }
            , { input = run2AtMost 5 (\y z -> equalLo (cons one y) (dottedList zero [ one, one, zero, one ] z))
              , output =
                    toOutput
                        [ "((_0 _1 _2 1) ())"
                        , "((_0 _1 _2 _3 1) (1))"
                        , "((_0 _1 _2 _3 _4 1) (_5 1))"
                        , "((_0 _1 _2 _3 _4 _5 1) (_6 _7 1))"
                        , "((_0 _1 _2 _3 _4 _5 _6 1) (_7 _8 _9 1))"
                        ]
              }

            --
            -- lessThanLo
            --
            , { input = run2AtMost 8 (\y z -> lessThanLo (cons one y) (dottedList zero [ one, one, zero, one ] z))
              , output =
                    toOutput
                        [ "(() _0)"
                        , "((1) _0)"
                        , "((_0 1) _1)"
                        , "((_0 _1 1) _2)"
                        , "((_0 _1 _2 1) (_3 . _4))"
                        , "((_0 _1 _2 _3 1) (_4 _5 . _6))"
                        , "((_0 _1 _2 _3 _4 1) (_5 _6 _7 . _8))"
                        , "((_0 _1 _2 _3 _4 _5 1) (_6 _7 _8 _9 . _10))"
                        ]
              }

            --
            -- lessThanOrEqualLo
            --
            , { input = run2AtMost 8 lessThanOrEqualLo
              , output =
                    toOutput
                        [ "(() ())"
                        , "((1) (1))"
                        , "(() (_0 . _1))"
                        , "((1) (_0 _1 . _2))"
                        , "((_0 1) (_1 1))"
                        , "((_0 1) (_1 _2 _3 . _4))"
                        , "((_0 _1 1) (_2 _3 1))"
                        , "((_0 _1 1) (_2 _3 _4 _5 . _6))"
                        ]
              }
            , { input =
                    run2AtMost 1
                        (\n m ->
                            conj
                                [ lessThanOrEqualLo n m
                                , timeso n (list [ zero, one ]) m
                                ]
                        )
              , output = "((() ()))"
              }
            , { input =
                    run2AtMost 10
                        (\n m ->
                            conj
                                [ lessThanOrEqualLo n m
                                , timeso n (list [ zero, one ]) m
                                ]
                        )
              , output =
                    toOutput
                        [ "(() ())"
                        , "((1) (0 1))"
                        , "((0 1) (0 0 1))"
                        , "((1 1) (0 1 1))"
                        , "((1 _0 1) (0 1 _0 1))"
                        , "((0 0 1) (0 0 0 1))"
                        , "((0 1 1) (0 0 1 1))"
                        , "((1 _0 _1 1) (0 1 _0 _1 1))"
                        , "((0 1 _0 1) (0 0 1 _0 1))"
                        , "((0 0 0 1) (0 0 0 0 1))"
                        ]
              }
            , { input = run2AtMost 9 lessThanOrEqualLo
              , output =
                    toOutput
                        [ "(() ())"
                        , "((1) (1))"
                        , "(() (_0 . _1))"
                        , "((1) (_0 _1 . _2))"
                        , "((_0 1) (_1 1))"
                        , "((_0 1) (_1 _2 _3 . _4))"
                        , "((_0 _1 1) (_2 _3 1))"
                        , "((_0 _1 1) (_2 _3 _4 _5 . _6))"
                        , "((_0 _1 _2 1) (_3 _4 _5 1))"
                        ]
              }

            --
            -- lessThano
            --
            , { input = run (\_ -> lessThano (list [ one, zero, one ]) (list [ one, one, one ]))
              , output = "(_0)"
              }
            , { input = run (\_ -> lessThano (list [ one, one, one ]) (list [ one, zero, one ]))
              , output = "()"
              }
            , { input = run (\_ -> lessThano (list [ one, zero, one ]) (list [ one, zero, one ]))
              , output = "()"
              }
            , { input = runAtMost 6 (\n -> lessThano n (list [ one, zero, one ]))
              , output = "(() (1) (_0 1) (0 0 1))"
              }
            , { input = runAtMost 6 (\m -> lessThano (list [ one, zero, one ]) m)
              , output = "((_0 _1 _2 _3 . _4) (0 1 1) (1 1 1))"
              }

            --
            -- lessThanOrEqualo
            --
            , { input = run (\_ -> lessThanOrEqualo (list [ one, zero, one ]) (list [ one, zero, one ]))
              , output = "(_0)"
              }

            --
            -- originalDivo
            --
            , { input = run4AtMost 5 originalDivo
              , output =
                    toOutput
                        [ "(() (_0 . _1) () ())" -- 0 ÷ m where m >= 1 produces q = 0, r = 0
                        , "((1) (_0 _1 . _2) () (1))" -- 1 ÷ m where m >= 2 produces q = 0, r = 1
                        , "((_0 . _1) (_0 . _1) (1) ())" -- n ÷ n where n >= 1 produces q = 1, r = 0
                        , "((_0 1) (_1 _2 _3 . _4) () (_0 1))" -- (2|3) ÷ m where m >= 4 produces q = 0, r = 2|3
                        , "((_0 _1 1) (_2 _3 _4 _5 . _6) () (_0 _1 1))" -- (4|5|6|7) ÷ m where m >= 8 produces q = 0, r = 4|5|6|7
                        ]
              }
            , { input =
                    run
                        (\m ->
                            fresh
                                (\r ->
                                    -- Q: Find m and r such that 5 ÷ m produces q = 7 and r.
                                    -- A: No such m and r exists.
                                    originalDivo (list [ one, zero, one ]) m (list [ one, one, one ]) r
                                )
                        )
              , output = "()"
              }

            --
            -- The following has no value.
            --
            -- We cannot divide an odd number by 2 and get a remainder of 0. However, originalDivo never stops looking
            -- for values of y and z that satisfy the division relation, although there are no such values.
            -- Instead, we would like originalDivo to fail immediately.
            --
            --, { input = run2AtMost 3 (\y z -> originalDivo (cons one (cons zero y)) (list [ zero, one ]) z numZero)
            --  , output = "()"
            --  }
            --
            --
            -- splito
            --
            , { input = run2 (splito (list [ zero, zero, one, zero, one ]) numZero)
              , output = "((() (0 1 0 1)))"
              }
            , { input = run2 (splito (list [ zero, zero, one, zero, one ]) numOne)
              , output = "((() (1 0 1)))"
              }
            , { input = run2 (splito (list [ zero, zero, one, zero, one ]) (list [ zero, one ]))
              , output = "(((0 0 1) (0 1)))"
              }
            , { input = run2 (splito (list [ zero, zero, one, zero, one ]) (list [ one, one ]))
              , output = "(((0 0 1) (0 1)))"
              }
            , { input = run2 (splito (list [ zero, zero, one, zero, one ]) (list [ zero, zero, one ]))
              , output = "(((0 0 1) (1)))"
              }
            , { input = run3 (splito (list [ zero, zero, one, zero, one ]))
              , output =
                    toOutput
                        [ "(() () (0 1 0 1))"
                        , "((_0) () (1 0 1))"
                        , "((_0 _1) (0 0 1) (0 1))"
                        , "((_0 _1 _2) (0 0 1) (1))"
                        , "((_0 _1 _2 _3) (0 0 1 0 1) ())"
                        , "((_0 _1 _2 _3 _4 . _5) (0 0 1 0 1) ())"
                        ]
              }
            , { input = run4AtMost 5 divo
              , output =
                    toOutput
                        [ "(() (_0 . _1) () ())" -- 0 ÷ m where m >= 1 produces q = 0, r = 0
                        , "((1) (_0 _1 . _2) () (1))" -- 1 ÷ m where m >= 2 produces q = 0, r = 1
                        , "((1) (1) (1) ())" -- 1 ÷ 1 produces q = 1, r = 0
                        , "((_0 1) (_1 _2 _3 . _4) () (_0 1))" -- (2|3) ÷ m where m >= 4 produces q = 0, r = 2|3
                        , "((_0 1) (_0 1) (1) ())" -- (2 ÷ 2|3 ÷ 3) produces q = 1, r = 0
                        ]
              }
            , { input =
                    run
                        (\m ->
                            fresh
                                (\r ->
                                    -- Q: Find m and r such that 5 ÷ m produces q = 7 and r.
                                    -- A: No such m and r exists.
                                    divo (list [ one, zero, one ]) m (list [ one, one, one ]) r
                                )
                        )
              , output = "()"
              }
            , { input = run2AtMost 3 (\y z -> divo (cons one (cons zero y)) (list [ zero, one ]) z numZero)
              , output = "()"
              }

            --
            -- (6 + 8k) ÷ 4 does not have a remainder of 0 or 1, for all possible values of k
            --
            -- N.B. dottedList zero [ one, one ] k = 6 + 8k for k >= 0
            --
            , { input = run2 (\k q -> divo (dottedList zero [ one, one ] k) (list [ zero, zero, one ]) q numZero)
              , output = "()"
              }
            , { input = run2 (\k q -> divo (dottedList zero [ one, one ] k) (list [ zero, zero, one ]) q numOne)
              , output = "()"
              }

            --
            -- logo
            --
            , { input =
                    -- n = 14, b = 2, q = 3
                    --
                    -- 14 = 2^3 + r => r = 6
                    run (logo (list [ zero, one, one, one ]) (list [ zero, one ]) (list [ one, one ]))
              , output = "((0 1 1))"
              }

            --
            -- This doesn't pass.
            --
            --, { input =
            --        run3AtMost 9
            --            (\b q r ->
            --                conj
            --                    [ logo (list [zero, zero, one, zero, zero, zero, one]) b q r
            --                    , greaterThan1o q
            --                    ]
            --            )
            --  , output =
            --        toOutput
            --            [ "(() (_0 _1 . _2) (0 0 1 0 0 0 1))"
            --            , "((1) (_0 _1 . _2) (1 1 0 0 0 0 1))"
            --            , "((0 1) (0 1 1) (0 0 1))"
            --            , "((1 1) (1 1) (1 0 0 1 0 1))"
            --            , "((0 0 1) (1 1) (0 0 1))"
            --            , "((0 0 0 1) (0 1) (0 0 1))"
            --            , "((1 0 1) (0 1) (1 1 0 1 0 1))"
            --            , "((0 1 1) (0 1) (0 0 0 0 0 1))"
            --            , "((1 1 1) (0 1) (1 1 0 0 1))"
            --            ]
            --
            -- since,
            --
            -- 68 = 0^q + 68 where q > 1 (Q: Shouldn't it be for q >= 1?)
            -- 68 = 1^q + 67 where q > 1 (Q: Same. Shouldn't it also work when q == 1?)
            -- 68 = 2^6 + 4
            -- 68 = 3^3 + 41
            -- 68 = 4^3 + 4
            -- 68 = 8^2 + 4
            -- 68 = 5^2 + 43
            -- 68 = 6^2 + 32, and
            -- 68 = 7^2 + 19.
            --}
            --
            -- expo
            --
            , { input =
                    -- 3^0 = 1
                    run (expo (list [ one, one ]) numZero)
              , output = "((1))"
              }
            , { input =
                    -- 3^1 = 3
                    run (expo (list [ one, one ]) numOne)
              , output = "((1 1))"
              }

            --
            -- These don't pass.
            --
            --, { input =
            --        -- 3^2 = 9
            --        run (expo (list [one, one]) (list [zero, one]))
            --  , output = "((1 0 0 1))"
            --  }
            --, { input =
            --        -- 3^5 = 243
            --        run (expo (list [one, one]) (list [one, zero, one]))
            --  , output = "((1 1 0 0 1 1 1 1))"
            --  }
            ]
