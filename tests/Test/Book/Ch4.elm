module Test.Book.Ch4 exposing (suite)

import Book.Ch4 exposing (..)
import Logic exposing (..)
import Test exposing (Test, describe)
import Test.Book.Fixtures exposing (testExample)


suite : Test
suite =
    describe "Book.Ch4" <|
        List.indexedMap
            testExample
            --
            -- appendo
            --
            [ { input =
                    runAtMost 6
                        (\x ->
                            fresh2
                                (\y z ->
                                    appendo x y z
                                )
                        )
              , output =
                    String.join " "
                        [ "(()"
                        , "(_0)"
                        , "(_0 _1)"
                        , "(_0 _1 _2)"
                        , "(_0 _1 _2 _3)"
                        , "(_0 _1 _2 _3 _4))"
                        ]
              }
            , { input =
                    runAtMost 6
                        (\y ->
                            fresh2
                                (\x z ->
                                    appendo x y z
                                )
                        )
              , output =
                    String.join " "
                        [ "(_0"
                        , "_0"
                        , "_0"
                        , "_0"
                        , "_0"
                        , "_0)"
                        ]
              }
            , { input =
                    runAtMost 6
                        (\z ->
                            fresh2
                                (\x y ->
                                    appendo x y z
                                )
                        )
              , output =
                    String.join " "
                        [ "(_0"
                        , "(_0 . _1)"
                        , "(_0 _1 . _2)"
                        , "(_0 _1 _2 . _3)"
                        , "(_0 _1 _2 _3 . _4)"
                        , "(_0 _1 _2 _3 _4 . _5))"
                        ]
              }
            , { input =
                    --
                    -- Or, run3AtMost 6 appendo
                    --
                    run3AtMost 6
                        (\x y z ->
                            appendo x y z
                        )
              , output =
                    String.join " "
                        [ "((() _0 _0)"
                        , "((_0) _1 (_0 . _1))"
                        , "((_0 _1) _2 (_0 _1 . _2))"
                        , "((_0 _1 _2) _3 (_0 _1 _2 . _3))"
                        , "((_0 _1 _2 _3) _4 (_0 _1 _2 _3 . _4))"
                        , "((_0 _1 _2 _3 _4) _5 (_0 _1 _2 _3 _4 . _5)))"
                        ]
              }
            , { input =
                    run
                        (\x ->
                            appendo
                                (list [ cake ])
                                (list [ tastes, yummy ])
                                x
                        )
              , output = "((cake tastes yummy))"
              }
            , { input =
                    run
                        (\x ->
                            fresh
                                (\y ->
                                    appendo
                                        (list [ cake, and, ice, y ])
                                        (list [ tastes, yummy ])
                                        x
                                )
                        )
              , output = "((cake & ice _0 tastes yummy))"
              }
            , { input =
                    run
                        (\x ->
                            fresh
                                (\y ->
                                    appendo
                                        (list [ cake, and, ice, cream ])
                                        y
                                        x
                                )
                        )
              , output = "((cake & ice cream . _0))"
              }
            , { input =
                    runAtMost 1
                        (\x ->
                            fresh
                                (\y ->
                                    appendo
                                        (dottedList cake [ and, ice ] y)
                                        (list [ d, t ])
                                        x
                                )
                        )
              , output = "((cake & ice d t))"
              }
            , { input =
                    runAtMost 5
                        (\x ->
                            fresh
                                (\y ->
                                    appendo
                                        (dottedList cake [ and, ice ] y)
                                        (list [ d, t ])
                                        x
                                )
                        )
              , output =
                    String.join " "
                        [ "((cake & ice d t)"
                        , "(cake & ice _0 d t)"
                        , "(cake & ice _0 _1 d t)"
                        , "(cake & ice _0 _1 _2 d t)"
                        , "(cake & ice _0 _1 _2 _3 d t))"
                        ]
              }
            , { input =
                    runAtMost 5
                        (\y ->
                            fresh
                                (\x ->
                                    appendo
                                        (dottedList cake [ and, ice ] y)
                                        (list [ d, t ])
                                        x
                                )
                        )
              , output =
                    String.join " "
                        [ "(()"
                        , "(_0)"
                        , "(_0 _1)"
                        , "(_0 _1 _2)"
                        , "(_0 _1 _2 _3))"
                        ]
              }
            , { input =
                    runAtMost 5
                        (\x ->
                            fresh
                                (\y ->
                                    appendo
                                        (dottedList cake [ and, ice ] y)
                                        (dottedList d [ t ] y)
                                        x
                                )
                        )
              , output =
                    String.join " "
                        [ "((cake & ice d t)"
                        , "(cake & ice _0 d t _0)"
                        , "(cake & ice _0 _1 d t _0 _1)"
                        , "(cake & ice _0 _1 _2 d t _0 _1 _2)"
                        , "(cake & ice _0 _1 _2 _3 d t _0 _1 _2 _3))"
                        ]
              }
            , { input =
                    run
                        (\x ->
                            fresh
                                (\z ->
                                    appendo
                                        (list [ cake, and, ice, cream ])
                                        (dottedList d [ t ] z)
                                        x
                                )
                        )
              , output = "((cake & ice cream d t . _0))"
              }
            , { input =
                    runAtMost 6
                        (\x ->
                            fresh
                                (\y ->
                                    appendo x y (list [ cake, and, ice, d, t ])
                                )
                        )
              , output =
                    String.join " "
                        [ "(()"
                        , "(cake)"
                        , "(cake &)"
                        , "(cake & ice)"
                        , "(cake & ice d)"
                        , "(cake & ice d t))"
                        ]
              }
            , { input =
                    runAtMost 6
                        (\y ->
                            fresh
                                (\x ->
                                    appendo x y (list [ cake, and, ice, d, t ])
                                )
                        )
              , output =
                    String.join " "
                        [ "((cake & ice d t)"
                        , "(& ice d t)"
                        , "(ice d t)"
                        , "(d t)"
                        , "(t)"
                        , "())"
                        ]
              }
            , { input =
                    run2AtMost 6
                        (\x y ->
                            appendo x y (list [ cake, and, ice, d, t ])
                        )
              , output =
                    String.join " "
                        [ "((() (cake & ice d t))"
                        , "((cake) (& ice d t))"
                        , "((cake &) (ice d t))"
                        , "((cake & ice) (d t))"
                        , "((cake & ice d) (t))"
                        , "((cake & ice d t) ()))"
                        ]
              }
            , { input =
                    run2AtMost 7
                        (\x y ->
                            appendo x y (list [ cake, and, ice, d, t ])
                        )
              , output =
                    String.join " "
                        [ "((() (cake & ice d t))"
                        , "((cake) (& ice d t))"
                        , "((cake &) (ice d t))"
                        , "((cake & ice) (d t))"
                        , "((cake & ice d) (t))"
                        , "((cake & ice d t) ()))"
                        ]
              }
            , { input =
                    run2
                        (\x y ->
                            appendo x y (list [ cake, and, ice, d, t ])
                        )
              , output =
                    String.join " "
                        [ "((() (cake & ice d t))"
                        , "((cake) (& ice d t))"
                        , "((cake &) (ice d t))"
                        , "((cake & ice) (d t))"
                        , "((cake & ice d) (t))"
                        , "((cake & ice d t) ()))"
                        ]
              }

            --
            -- swappendo
            --
            , { input =
                    run2
                        (\x y ->
                            swappendo x y (list [ cake, and, ice, d, t ])
                        )
              , output =
                    String.join " "
                        [ "((() (cake & ice d t))"
                        , "((cake) (& ice d t))"
                        , "((cake &) (ice d t))"
                        , "((cake & ice) (d t))"
                        , "((cake & ice d) (t))"
                        , "((cake & ice d t) ()))"
                        ]
              }

            --
            -- unwrapo
            --
            , { input = run (unwrapo (list [ list [ list [ pizza ] ] ]))
              , output =
                    String.join " "
                        [ "((((pizza)))"
                        , "((pizza))"
                        , "(pizza)"
                        , "pizza)"
                        ]
              }
            , { input = runAtMost 1 (\x -> unwrapo x pizza)
              , output = "(pizza)"
              }
            , { input = runAtMost 1 (\x -> unwrapo (list [ list [ x ] ]) pizza)
              , output = "(pizza)"
              }
            , { input = runAtMost 5 (\x -> unwrapo x pizza)
              , output =
                    String.join " "
                        [ "(pizza"
                        , "(pizza . _0)"
                        , "((pizza . _0) . _1)"
                        , "(((pizza . _0) . _1) . _2)"
                        , "((((pizza . _0) . _1) . _2) . _3))"
                        ]
              }
            , { input = runAtMost 5 (\x -> unwrapo x (list [ list [ pizza ] ]))
              , output =
                    String.join " "
                        [ "(((pizza))"
                        , "(((pizza)) . _0)"
                        , "((((pizza)) . _0) . _1)"
                        , "(((((pizza)) . _0) . _1) . _2)"
                        , "((((((pizza)) . _0) . _1) . _2) . _3))"
                        ]
              }
            , { input = runAtMost 5 (\x -> unwrapo (list [ list [ x ] ]) pizza)
              , output =
                    String.join " "
                        [ "(pizza"
                        , "(pizza . _0)"
                        , "((pizza . _0) . _1)"
                        , "(((pizza . _0) . _1) . _2)"
                        , "((((pizza . _0) . _1) . _2) . _3))"
                        ]
              }
            ]



-- VALUES


and =
    string "&"


cake =
    string "cake"


cream =
    string "cream"


d =
    string "d"


ice =
    string "ice"


pizza =
    string "pizza"


t =
    string "t"


tastes =
    string "tastes"


yummy =
    string "yummy"
