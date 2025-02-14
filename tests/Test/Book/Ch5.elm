module Test.Book.Ch5 exposing (suite)

import Book.Ch5 exposing (..)
import Logic exposing (..)
import Test exposing (Test, describe)
import Test.Book.Fixtures exposing (testExample)


suite : Test
suite =
    describe "Book.Ch5" <|
        List.indexedMap
            testExample
            --
            -- memo
            --
            [ { input = run (\_ -> memo fig (list [ pea ]) (list [ pea ]))
              , output = "()"
              }
            , { input = run (\out -> memo fig (list [ fig ]) out)
              , output = "((fig))"
              }
            , { input = run (\out -> memo fig (list [ fig, pea ]) out)
              , output = "((fig pea))"
              }
            , { input =
                    run
                        (\r ->
                            memo r (list [ roll, okra, fig, beet, fig, pea ]) (list [ fig, beet, fig, pea ])
                        )
              , output = "(fig)"
              }
            , { input = run (\x -> memo fig (list [ fig, pea ]) (list [ pea, x ]))
              , output = "()"
              }
            , { input = run (\x -> memo fig (list [ fig, pea ]) (list [ x, pea ]))
              , output = "(fig)"
              }
            , { input = run (\out -> memo fig (list [ beet, fig, pea ]) out)
              , output = "((fig pea))"
              }
            , { input = runAtMost 1 (\out -> memo fig (list [ fig, fig, pea ]) out)
              , output = "((fig fig pea))"
              }
            , { input = run (\out -> memo fig (list [ fig, fig, pea ]) out)
              , output = "((fig fig pea) (fig pea))"
              }
            , { input =
                    run
                        (\out ->
                            fresh
                                (\x ->
                                    memo fig (list [ a, x, c, fig, e ]) out
                                )
                        )
              , output = "((fig c fig e) (fig e))"
              }
            , { input =
                    run2AtMost 5
                        (\x y ->
                            memo fig (dottedList fig [ d, fig, e ] y) x
                        )
              , output =
                    String.join " "
                        [ "(((fig d fig e . _0) _0)"
                        , "((fig e . _0) _0)"
                        , "((fig . _0) (fig . _0))"
                        , "((fig . _0) (_1 fig . _0))"
                        , "((fig . _0) (_1 _2 fig . _0)))"
                        ]
              }

            --
            -- rembero
            --
            , { input = run (\out -> rembero pea (list [ pea ]) out)
              , output = "(() (pea))"
              }
            , { input = run (\out -> rembero pea (list [ pea, pea ]) out)
              , output = "((pea) (pea) (pea pea))"
              }
            , { input =
                    run
                        (\out ->
                            fresh2
                                (\y z ->
                                    rembero y (list [ a, b, y, d, z, e ]) out
                                )
                        )
              , output =
                    String.join " "
                        [ "((b a d _0 e)"
                        , "(a b d _0 e)"
                        , "(a b d _0 e)"
                        , "(a b d _0 e)"
                        , "(a b _0 d e)"
                        , "(a b e d _0)"
                        , "(a b _0 d _1 e))"
                        ]
              }
            , { input =
                    run2
                        (\y z ->
                            rembero y (list [ y, d, z, e ]) (list [ y, d, e ])
                        )
              , output =
                    String.join " "
                        [ "((d d)"
                        , "(d d)"
                        , "(_0 _0)"
                        , "(e e))"
                        ]
              }
            , { input =
                    run4AtMost 4
                        (\y z w out ->
                            rembero y (cons z w) out
                        )
              , output =
                    String.join " "
                        [ "((_0 _0 _1 _1)"
                        , "(_0 _1 () (_1))"
                        , "(_0 _1 (_0 . _2) (_1 . _2))"
                        , "(_0 _1 (_2) (_1 _2)))"
                        ]
              }
            , { input =
                    run4AtMost 5
                        (\y z w out ->
                            rembero y (cons z w) out
                        )
              , output =
                    String.join " "
                        [ "((_0 _0 _1 _1)"
                        , "(_0 _1 () (_1))"
                        , "(_0 _1 (_0 . _2) (_1 . _2))"
                        , "(_0 _1 (_2) (_1 _2))"
                        , "(_0 _1 (_2 _0 . _3) (_1 _2 . _3)))"
                        ]
              }
            ]



-- VALUES


a =
    string "a"


b =
    string "b"


beet =
    string "beet"


c =
    string "c"


d =
    string "d"


e =
    string "e"


fig =
    string "fig"


okra =
    string "okra"


pea =
    string "pea"


roll =
    string "roll"
