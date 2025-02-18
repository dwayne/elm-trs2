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
            ]
