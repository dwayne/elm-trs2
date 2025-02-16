module Test.Book.Ch7 exposing (suite)

import Book.Ch7 exposing (..)
import Expect
import Logic exposing (..)
import Test exposing (Test, describe, test)
import Test.Book.Fixtures exposing (testExample, toOutput)


suite : Test
suite =
    describe "Book.Ch7"
        [ examplesSuite
        , buildNumSuite
        ]


examplesSuite : Test
examplesSuite =
    describe "Examples" <|
        List.indexedMap
            testExample
            --
            -- bitXoro
            --
            [ { input =
                    run2
                        (\x y ->
                            bitXoro x y zero
                        )
              , output =
                    toOutput
                        [ "(0 0)"
                        , "(1 1)"
                        ]
              }
            , { input =
                    run2
                        (\x y ->
                            bitXoro x y one
                        )
              , output =
                    toOutput
                        [ "(0 1)"
                        , "(1 0)"
                        ]
              }
            , { input =
                    run3
                        (\x y r ->
                            bitXoro x y r
                        )
              , output =
                    toOutput
                        [ "(0 0 0)"
                        , "(0 1 1)"
                        , "(1 0 1)"
                        , "(1 1 0)"
                        ]
              }

            --
            -- bitAndo
            --
            , { input =
                    run2
                        (\x y ->
                            bitAndo x y one
                        )
              , output = "((1 1))"
              }

            --
            -- halfAddero
            --
            , { input =
                    run
                        (\r ->
                            halfAddero one one r one
                        )
              , output = "(0)"
              }
            , { input = run4 halfAddero
              , output =
                    toOutput
                        [ "(0 0 0 0)"
                        , "(0 1 1 0)"
                        , "(1 0 1 0)"
                        , "(1 1 0 1)"
                        ]
              }

            --
            -- fullAddero
            --
            , { input =
                    run2
                        (\r c ->
                            fullAddero zero one one r c
                        )
              , output = "((0 1))"
              }
            , { input =
                    run2
                        (\r c ->
                            fullAddero one one one r c
                        )
              , output = "((1 1))"
              }
            , { input = run5 fullAddero
              , output =
                    --
                    -- N.B. The order is different from the book but the answer is correct.
                    --
                    toOutput
                        [ "(0 0 0 0 0)"
                        , "(1 0 0 1 0)"
                        , "(0 0 1 1 0)"
                        , "(1 0 1 0 1)"
                        , "(0 1 0 1 0)"
                        , "(1 1 0 0 1)"
                        , "(0 1 1 0 1)"
                        , "(1 1 1 1 1)"
                        ]
              }

            --
            -- poso
            --
            , { input = run (\_ -> poso (list [ zero, one, one ]))
              , output = "(_0)"
              }
            , { input = run (\_ -> poso (buildNum 6))
              , output = "(_0)"
              }
            , { input = run (\_ -> poso (list [ one ]))
              , output = "(_0)"
              }
            , { input = run (\_ -> poso null)
              , output = "()"
              }
            , { input = run poso
              , output = "((_0 . _1))"
              }

            --
            -- greaterThan1o
            --
            , { input = run (\_ -> greaterThan1o (list [ zero, one, one ]))
              , output = "(_0)"
              }
            , { input = run (\_ -> greaterThan1o (list [ zero, one ]))
              , output = "(_0)"
              }
            , { input = run (\_ -> greaterThan1o (list [ one ]))
              , output = "()"
              }
            , { input = run (\_ -> greaterThan1o null)
              , output = "()"
              }
            , { input = run greaterThan1o
              , output = "((_0 _1 . _2))"
              }

            --
            -- Examples to help me understand genAddero
            --
            -- n >= 1
            --
            , { input =
                    run2
                        (\a x ->
                            equals (cons a x) (buildNum 0)
                        )
              , output = "()"
              }
            , { input =
                    run2
                        (\a x ->
                            equals (cons a x) (buildNum 1)
                        )
              , output = "((1 ()))"
              }
            , { input =
                    run2
                        (\a x ->
                            equals (cons a x) (buildNum 2)
                        )
              , output = "((0 (1)))"
              }
            , { input =
                    run2
                        (\a x ->
                            equals (cons a x) (buildNum 3)
                        )
              , output = "((1 (1)))"
              }
            , { input =
                    run2
                        (\a x ->
                            equals (cons a x) (buildNum 4)
                        )
              , output = "((0 (0 1)))"
              }

            --
            -- m > 1
            --
            , { input =
                    run2
                        (\d y ->
                            conj
                                [ equals (cons d y) (buildNum 0)
                                , poso y
                                ]
                        )
              , output = "()"
              }
            , { input =
                    run2
                        (\d y ->
                            conj
                                [ equals (cons d y) (buildNum 1)
                                , poso y
                                ]
                        )
              , output = "()"
              }
            , { input =
                    run2
                        (\d y ->
                            conj
                                [ equals (cons d y) (buildNum 2)
                                , poso y
                                ]
                        )
              , output = "((0 (1)))"
              }
            , { input =
                    run2
                        (\d y ->
                            conj
                                [ equals (cons d y) (buildNum 3)
                                , poso y
                                ]
                        )
              , output = "((1 (1)))"
              }
            , { input =
                    run2
                        (\d y ->
                            conj
                                [ equals (cons d y) (buildNum 4)
                                , poso y
                                ]
                        )
              , output = "((0 (0 1)))"
              }
            , { input =
                    -- 1 + 0 + 3
                    run (genAddero one null (list [ one, one ]))
              , output =
                    -- n must be >= 1
                    "()"
              }
            , { input =
                    -- 1 + 1 + 1
                    run (genAddero one null (list [ one ]))
              , output =
                    -- m must be > 1
                    "()"
              }
            , { input =
                    -- 0 + 1 + 2
                    run (genAddero zero (list [ one ]) (list [ zero, one ]))
              , output =
                    -- 3
                    "((1 1))"
              }
            , { input =
                    -- 1 + 1 + 2
                    run (genAddero one (list [ one ]) (list [ zero, one ]))
              , output =
                    -- 4
                    "((0 0 1))"
              }
            , { input =
                    -- 1 + 1 + 3
                    run (genAddero one (list [ one ]) (list [ one, one ]))
              , output =
                    -- 5
                    "((1 0 1))"
              }
            , { input =
                    -- 1 + 2 + 3
                    run (genAddero one (list [ zero, one ]) (list [ one, one ]))
              , output =
                    -- 6
                    "((0 1 1))"
              }
            , { input =
                    -- 1 + 3 + 3
                    run (genAddero one (list [ one, one ]) (list [ one, one ]))
              , output =
                    -- 7
                    "((1 1 1))"
              }
            , { input =
                    -- 1 + 4 + 3
                    run (genAddero one (list [ zero, zero, one ]) (list [ one, one ]))
              , output =
                    -- 8
                    "((0 0 0 1))"

              --
              -- Why?
              --
              -- b = one
              -- n = list [ zero, zero, one ]
              -- m = list [ one, one ]
              --
              -- a = zero
              -- x = list [ zero, one ]
              -- d = one
              -- y = list [ one ]
              --
              -- fullAddero b a d c e
              -- = fullAddero one zero one c e
              --
              -- c = zero
              -- e = one
              --
              -- addero e x y z
              -- = addero one (list [ zero, one ]) (list [ one ]) z
              --
              -- z = list [ zero, zero, one ]
              --
              -- equals (cons c z) r
              --
              -- r = cons zero (list [ zero, zero, one ])
              --   = list [ zero, zero, zero, one ]
              --
              }
            , { input = run2 (\c e -> fullAddero one zero one c e)
              , output = "((0 1))"
              }
            , { input = run (\z -> addero one (list [ zero, one ]) (list [ one ]) z)
              , output = "((0 0 1))"
              }

            --
            -- genAddero
            --
            , { input =
                    -- 1 + 6 + 3
                    run (genAddero one (list [ zero, one, one ]) (list [ one, one ]))
              , output =
                    -- 10
                    "((0 1 0 1))"
              }

            --
            -- addero
            --
            , { input =
                    -- Finds x, y such that x + y = 5
                    run2 (\x y -> addero zero x y (list [ one, zero, one ]))
              , output =
                    toOutput
                        [ "((1 0 1) ())"
                        , "(() (1 0 1))"
                        , "((1) (0 0 1))"
                        , "((0 1) (1 1))"
                        , "((0 0 1) (1))"
                        , "((1 1) (0 1))"
                        ]
              }

            --
            -- pluso
            --
            , { input =
                    -- Finds x, y such that x + y = 5
                    run2 (\x y -> pluso x y (buildNum 5))
              , output =
                    toOutput
                        [ "((1 0 1) ())"
                        , "(() (1 0 1))"
                        , "((1) (0 0 1))"
                        , "((0 1) (1 1))"
                        , "((0 0 1) (1))"
                        , "((1 1) (0 1))"
                        ]
              }

            --
            -- minuso
            --
            , { input =
                    -- 8 - 5
                    run (minuso (list [ zero, zero, zero, one ]) (list [ one, zero, one ]))
              , output =
                    -- 3
                    "((1 1))"
              }
            , { input =
                    -- 6 - 6
                    run (minuso (list [ zero, one, one ]) (list [ zero, one, one ]))
              , output =
                    -- 0
                    "(())"
              }
            , { input =
                    -- 6 - 8
                    run (minuso (list [ zero, one, one ]) (list [ zero, zero, zero, one ]))
              , output =
                    -- 8 cannot be subtracted from 6 since we do not represent negative numbers
                    "()"
              }

            --
            -- lengtho
            --
            , { input = runAtMost 1 (lengtho (list [ jicama, rhubarb, guava ]))
              , output = "((1 1))"
              }
            , { input = run (\ls -> lengtho ls (list [ one, zero, one ]))
              , output = "((_0 _1 _2 _3 _4))"
              }
            , { input = run (\_ -> lengtho (list [ one, zero, one ]) (int 3))
              , output = "()" -- since (1 1) is not 3
              }
            , { input = runAtMost 3 (\q -> lengtho q q)
              , output = "(() (1) (0 1))" -- since these numbers are the same as their lengths
              }
            ]


buildNumSuite : Test
buildNumSuite =
    describe "buildNum" <|
        List.map
            testBuildNum
            [ { n = 0
              , binary = null
              }
            , { n = 36
              , binary = list [ zero, zero, one, zero, zero, one ]
              }
            , { n = 19
              , binary = list [ one, one, zero, zero, one ]
              }
            ]


testBuildNum : { n : Int, binary : Value a } -> Test
testBuildNum { n, binary } =
    test (String.fromInt n) <|
        \_ ->
            buildNum n
                |> Expect.equal binary



-- VALUES


guava =
    string "guava"


jicama =
    string "jicama"


rhubarb =
    string "rhubarb"
