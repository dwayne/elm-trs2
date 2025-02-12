module Test.Book.Ch3 exposing (suite)

import Book.Ch3 exposing (..)
import Logic exposing (..)
import Test exposing (Test, describe)
import Test.Book.Fixtures exposing (testExample)


suite : Test
suite =
    describe "Book.Ch3" <|
        List.indexedMap
            testExample
            --
            -- listo
            --
            [ { input = run (\x -> listo (list [ string "a", string "b", x, string "d" ]))
              , output = "(_0)"
              }
            , { input = runAtMost 1 (\x -> listo (cons (string "a") (cons (string "b") (cons (string "c") x))))
              , output = "(())"
              }
            , { input = runAtMost 5 (\x -> listo (cons (string "a") (cons (string "b") (cons (string "c") x))))
              , output =
                    String.join " "
                        [ "(()"
                        , "(_0)"
                        , "(_0 _1)"
                        , "(_0 _1 _2)"
                        , "(_0 _1 _2 _3))"
                        ]
              }

            --
            -- lolo
            --
            , { input =
                    run
                        (\q ->
                            fresh2
                                (\x y ->
                                    lolo <|
                                        list
                                            [ list [ string "a", string "b" ]
                                            , list [ x, string "c" ]
                                            , list [ string "d", y ]
                                            ]
                                )
                        )
              , output = "(_0)"
              }
            , { input = runAtMost 1 lolo -- i.e. runAtMost 1 (\l -> lolo l)
              , output = "(())"
              }
            , { input =
                    runAtMost 1
                        (\q ->
                            fresh
                                (\x ->
                                    lolo (cons (list [ string "a", string "b" ]) x)
                                )
                        )
              , output = "(_0)"
              }
            , { input =
                    runAtMost 1
                        (\x ->
                            lolo (cons (list [ string "a", string "b" ]) (cons (list [ string "c", string "d" ]) x))
                        )
              , output = "(())"
              }
            , { input =
                    runAtMost 5
                        (\x ->
                            lolo (cons (list [ string "a", string "b" ]) (cons (list [ string "c", string "d" ]) x))
                        )
              , output =
                    --
                    -- This output is different to the output produced by frame 29 on page 42.
                    --
                    -- (()
                    --  (())
                    --  ((_0))
                    --  (() ())
                    --  ((_0 _1)))
                    --
                    -- However, the values I produce are correct and just in a different order.
                    --
                    String.join " "
                        [ "(()"
                        , "(())"
                        , "(() ())"
                        , "((_0))"
                        , "(() () ()))"
                        ]
              }
            , { input = runAtMost 5 lolo
              , output =
                    String.join " "
                        [ "(()"
                        , "(())"
                        , "(() ())"
                        , "((_0))"
                        , "(() () ()))"
                        ]
              }

            --
            -- loso
            --
            , { input =
                    runAtMost 1
                        (\z ->
                            loso (cons (list [ string "g" ]) z)
                        )
              , output = "(())"
              }
            , { input =
                    runAtMost 5
                        (\z ->
                            loso (cons (list [ string "g" ]) z)
                        )
              , output =
                    String.join " "
                        [ "(()"
                        , "((_0))"
                        , "((_0) (_1))"
                        , "((_0) (_1) (_2))"
                        , "((_0) (_1) (_2) (_3)))"
                        ]
              }
            , { input =
                    runAtMost 4
                        (\r ->
                            fresh4
                                (\w x y z ->
                                    conj
                                        [ loso (dottedList (list [ string "g" ]) [ cons (string "e") w, cons x y ] z)
                                        , equals (list [ w, cons x y, z ]) r
                                        ]
                                )
                        )
              , output =
                    String.join " "
                        [ "((() (_0) ())"
                        , "(() (_0) ((_1)))"
                        , "(() (_0) ((_1) (_2)))"
                        , "(() (_0) ((_1) (_2) (_3))))"
                        ]
              }
            , { input =
                    runAtMost 3
                        (\out ->
                            fresh4
                                (\w x y z ->
                                    conj
                                        [ equals (dottedList (list [ string "g" ]) [ cons (string "e") w, cons x y ] z) out
                                        , loso out
                                        ]
                                )
                        )
              , output =
                    String.join " "
                        [ "(((g) (e) (_0))"
                        , "((g) (e) (_0) (_1))"
                        , "((g) (e) (_0) (_1) (_2)))"
                        ]
              }

            --
            -- membero
            --
            , { input = run (\q -> membero olive (list [ virgin, olive, oil ]))
              , output = "(_0)"
              }
            , { input = runAtMost 1 (\y -> membero y (list [ hummus, with, pita ]))
              , output = "(hummus)"
              }
            , { input = runAtMost 1 (\y -> membero y (list [ with, pita ]))
              , output = "(with)"
              }
            , { input = runAtMost 1 (\y -> membero y (list [ pita ]))
              , output = "(pita)"
              }
            , { input = run (\y -> membero y (list []))
              , output = "()"
              }
            , { input = run (\y -> membero y (list [ hummus, with, pita ]))
              , output = "(hummus with pita)"
              }
            , { input = run (\y -> membero y (dottedList pear [ grape ] peaches))
              , output = "(pear grape)"
              }
            , { input = run (\x -> membero (string "e") (list [ pasta, x, fagioli ]))
              , output = "(e)"
              }
            , { input = runAtMost 1 (\x -> membero (string "e") (list [ pasta, string "e", x, fagioli ]))
              , output = "(_0)"
              }
            , { input = runAtMost 1 (\x -> membero (string "e") (list [ pasta, x, string "e", fagioli ]))
              , output = "(e)"
              }
            , { input = run2 (\x y -> membero (string "e") (list [ pasta, x, fagioli, y ]))
              , output = "((e _0) (_0 e))"
              }
            , { input =
                    run
                        (\q ->
                            fresh2
                                (\x y ->
                                    conj
                                        [ equals (list [ pasta, x, fagioli, y ]) q
                                        , membero (string "e") q
                                        ]
                                )
                        )
              , output = "((pasta e fagioli _0) (pasta _0 fagioli e))"
              }
            , { input = runAtMost 1 (membero tofu)
              , output = "((tofu . _0))"
              }
            , { input = runAtMost 5 (membero tofu)
              , output =
                    String.join " "
                        [ "((tofu . _0)"
                        , "(_0 tofu . _1)"
                        , "(_0 _1 tofu . _2)"
                        , "(_0 _1 _2 tofu . _3)"
                        , "(_0 _1 _2 _3 tofu . _4))"
                        ]
              }

            --
            -- properMembero
            --
            , { input = runAtMost 12 (properMembero tofu)
              , output =
                    String.join " "
                        [ "((tofu)"
                        , "(tofu _0)"
                        , "(_0 tofu)"
                        , "(tofu _0 _1)"
                        , "(tofu _0 _1 _2)"
                        , "(_0 tofu _1)"
                        , "(tofu _0 _1 _2 _3)"
                        , "(_0 _1 tofu)"
                        , "(tofu _0 _1 _2 _3 _4)"
                        , "(_0 tofu _1 _2)"
                        , "(tofu _0 _1 _2 _3 _4 _5)"
                        , "(tofu _0 _1 _2 _3 _4 _5 _6))"
                        ]
              }
            ]



-- VALUES


fagioli =
    string "fagioli"


grape =
    string "grape"


hummus =
    string "hummus"


oil =
    string "oil"


olive =
    string "olive"


pasta =
    string "pasta"


peaches =
    string "peaches"


pear =
    string "pear"


pita =
    string "pita"


tofu =
    string "tofu"


virgin =
    string "virgin"


with =
    string "with"
