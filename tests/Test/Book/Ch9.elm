module Test.Book.Ch9 exposing (suite)

import Book.Ch1 exposing (..)
import Book.Ch6 exposing (..)
import Book.Ch7 exposing (..)
import Book.Ch9 exposing (..)
import Logic exposing (..)
import Test exposing (Test, describe)
import Test.Book.Fixtures exposing (testExample, toOutput)


suite : Test
suite =
    describe "Book.Ch9" <|
        List.indexedMap
            testExample
            --
            -- conda
            --
            [ { input =
                    run
                        (\x ->
                            conda
                                [ [ equals olive x, succeed ]
                                , [ succeed, equals oil x ]
                                ]
                        )
              , output = "(olive)"
              }
            , { input =
                    run
                        (\x ->
                            conda
                                [ [ equals virgin x, fail ]
                                , [ equals olive x, succeed ]
                                , [ succeed, equals oil x ]
                                ]
                        )
              , output = "()"
              }
            , { input =
                    run
                        (\_ ->
                            fresh2
                                (\x y ->
                                    conj
                                        [ equals split x
                                        , equals pea y
                                        , conda
                                            [ [ equals split x, equals x y ]
                                            , [ succeed, succeed ]
                                            ]
                                        ]
                                )
                        )
              , output = "()"
              }
            , { input =
                    run
                        (\_ ->
                            fresh2
                                (\x y ->
                                    conj
                                        [ equals split x
                                        , equals pea y
                                        , conda
                                            [ [ equals x y, equals split x ]
                                            , [ succeed, succeed ]
                                            ]
                                        ]
                                )
                        )
              , output = "(_0)"
              }

            --
            -- notPastao
            --
            , { input =
                    run
                        (\x ->
                            conda
                                [ [ notPastao x, fail ]
                                , [ equals spaghetti x, succeed ]
                                ]
                        )
              , output = "(spaghetti)"
              }
            , { input =
                    run
                        (\x ->
                            conj
                                [ equals spaghetti x
                                , conda
                                    [ [ notPastao x, fail ]
                                    , [ equals spaghetti x, succeed ]
                                    ]
                                ]
                        )
              , output = "()"
              }

            --
            -- condu
            --
            , { input =
                    runAtMost 1
                        (\_ ->
                            conj
                                [ condu
                                    [ [ alwayso, succeed ]
                                    , [ succeed, fail ]
                                    ]
                                , fail
                                ]
                        )
              , output = "()"
              }
            , { input = run (onceo << teacupo)
              , output = "(tea)"
              }
            , { input =
                    run
                        (\r ->
                            conde
                                [ [ teacupo r, succeed ]
                                , [ equals false r, succeed ]
                                ]
                        )
              , output = "(tea cup #f)"
              }
            , { input =
                    run
                        (\r ->
                            conda
                                [ [ teacupo r, succeed ]
                                , [ succeed, equals false r ]
                                ]
                        )
              , output = "(tea cup)"
              }
            , { input =
                    run
                        (\r ->
                            conj
                                [ equals false r
                                , conda
                                    [ [ teacupo r, succeed ]
                                    , [ equals false r, succeed ]
                                    , [ succeed, fail ]
                                    ]
                                ]
                        )
              , output = "(#f)"
              }
            , { input =
                    run
                        (\r ->
                            conj
                                [ equals false r
                                , condu
                                    [ [ teacupo r, succeed ]
                                    , [ equals false r, succeed ]
                                    , [ succeed, fail ]
                                    ]
                                ]
                        )
              , output = "(#f)"
              }

            --
            -- bumpo
            --
            , { input = run (bumpo (list [ one, one, one ]))
              , output =
                    toOutput
                        [ "(1 1 1)"
                        , "(0 1 1)"
                        , "(1 0 1)"
                        , "(0 0 1)"
                        , "(1 1)"
                        , "(0 1)"
                        , "(1)"
                        , "()"
                        ]
              }
            ]



-- VALUES


false =
    string "#f"


oil =
    string "oil"


olive =
    string "olive"


pea =
    string "pea"


spaghetti =
    string "spaghetti"


split =
    string "split"


virgin =
    string "virgin"
