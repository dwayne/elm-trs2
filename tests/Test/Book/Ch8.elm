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
            ]
