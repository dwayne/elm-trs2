module Test.Book.Ch2 exposing (suite)

import Book.Ch2 exposing (..)
import Logic exposing (..)
import Test exposing (Test, describe)
import Test.Book.Fixtures exposing (testExample)


suite : Test
suite =
    describe "Book.Ch2" <|
        List.indexedMap
            testExample
            --
            -- caro
            --
            [ { input = run (\q -> caro acorn q)
              , output = "(a)"
              }
            , { input = run (\q -> caro acorn (string "a"))
              , output = "(_0)"
              }
            , { input =
                    run
                        (\r ->
                            fresh2
                                (\x y ->
                                    conj
                                        [ caro (list [ r, y ]) x
                                        , equals (string "pear") x
                                        ]
                                )
                        )
              , output = "(pear)"
              }
            , { input =
                    run
                        (\r ->
                            fresh2
                                (\x y ->
                                    conj
                                        [ caro (list [ string "grape", string "raisin", string "pear" ]) x
                                        , caro (list [ list [ string "a" ], list [ string "b" ], list [ string "c" ] ]) y
                                        , equals (cons x y) r
                                        ]
                                )
                        )
              , output = "((grape a))"
              }

            --
            -- cdro
            --
            , { input =
                    run
                        (\r ->
                            fresh
                                (\v ->
                                    conj
                                        [ cdro acorn v
                                        , fresh
                                            (\w ->
                                                conj
                                                    [ cdro v w
                                                    , caro w r
                                                    ]
                                            )
                                        ]
                                )
                        )
              , output = "(o)"
              }
            , { input =
                    run
                        (\r ->
                            fresh2
                                (\x y ->
                                    conj
                                        [ cdro (list [ string "grape", string "raisin", string "pear" ]) x
                                        , caro (list [ list [ string "a" ], list [ string "b" ], list [ string "c" ] ]) y
                                        , equals (cons x y) r
                                        ]
                                )
                        )
              , output = "(((raisin pear) a))"
              }
            , { input = run (\q -> cdro acorn corn)
              , output = "(_0)"
              }
            , { input = run (\x -> cdro corn (xrn x))
              , output = "(o)"
              }
            , { input =
                    run
                        (\l ->
                            fresh
                                (\x ->
                                    conj
                                        [ cdro l corn
                                        , caro l x
                                        , equals (string "a") x
                                        ]
                                )
                        )
              , output = "((a c o r n))"
              }

            --
            -- conso
            --
            , { input =
                    run
                        (\l ->
                            conso (list [ string "a", string "b", string "c" ]) (list [ string "d", string "e" ]) l
                        )
              , output = "(((a b c) d e))"
              }
            , { input =
                    run
                        (\x ->
                            conso x (list [ string "a", string "b", string "c" ]) (list [ string "d", string "a", string "b", string "c" ])
                        )
              , output = "(d)"
              }
            , { input =
                    run
                        (\r ->
                            fresh3
                                (\x y z ->
                                    conj
                                        [ equals (list [ string "e", string "a", string "d", x ]) r
                                        , conso y (list [ string "a", z, string "c" ]) r
                                        ]
                                )
                        )
              , output = "((e a d c))"
              }
            , { input = run (\x -> conso x (list [ string "a", x, string "c" ]) (list [ string "d", string "a", x, string "c" ]))
              , output = "(d)"
              }
            , { input =
                    run
                        (\l ->
                            fresh
                                (\x ->
                                    conj
                                        [ equals (list [ string "d", string "a", x, string "c" ]) l
                                        , conso x (list [ string "a", x, string "c" ]) l
                                        ]
                                )
                        )
              , output = "((d a d c))"
              }
            , { input =
                    run
                        (\l ->
                            fresh
                                (\x ->
                                    conj
                                        [ conso x (list [ string "a", x, string "c" ]) l
                                        , equals (list [ string "d", string "a", x, string "c" ]) l
                                        ]
                                )
                        )
              , output = "((d a d c))"
              }
            , { input =
                    run
                        (\l ->
                            fresh5
                                (\d t x y w ->
                                    conj
                                        [ conso w (list [ string "n", string "u", string "s" ]) t
                                        , cdro l t
                                        , caro l x
                                        , equals (string "b") x
                                        , cdro l d
                                        , caro d y
                                        , equals (string "o") y
                                        ]
                                )
                        )
              , output = "((b o n u s))"
              }

            --
            -- nullo
            --
            , { input = run (\q -> nullo (list [ string "grape", string "raisin", string "pear" ]))
              , output = "()"
              }
            , { input = run (\q -> nullo null)
              , output = "(_0)"
              }
            , { input = run (\x -> nullo x)
              , output = "(())"
              }

            --
            -- pairo
            --
            , { input =
                    run
                        (\r ->
                            fresh2
                                (\x y ->
                                    equals (cons x (cons y (string "salad"))) r
                                )
                        )
              , output = "((_0 _1 . salad))"
              }
            , { input = run (\q -> pairo (cons q q))
              , output = "(_0)"
              }
            , { input = run (\q -> pairo null)
              , output = "()"
              }
            , { input = run (\x -> pairo x)
              , output = "((_0 . _1))"
              }
            , { input = run (\r -> pairo (cons r null))
              , output = "(_0)"
              }

            --
            -- singletono
            --
            , { input = run (\q -> singletono null)
              , output = "()"
              }
            , { input = run (\x -> singletono (cons x null))
              , output = "(_0)"
              }
            , { input = run (\x -> singletono (list [ x ]))
              , output = "(_0)"
              }
            , { input = run2 (\x y -> singletono (cons x y))
              , output = "((_0 ()))"
              }
            , { input = run2 (\x y -> singletono (list [ x, y ]))
              , output = "()"
              }
            ]



-- VALUES


acorn =
    -- '(a c o r n)
    list [ string "a", string "c", string "o", string "r", string "n" ]


corn =
    -- '(c o r n)
    list [ string "c", string "o", string "r", string "n" ]


xrn x =
    -- `(,x r n)
    list [ x, string "r", string "n" ]
