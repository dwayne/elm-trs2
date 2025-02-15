module Test.Book.Ch6 exposing (suite)

import Book.Ch6 exposing (..)
import Logic exposing (..)
import Test exposing (Test, describe)
import Test.Book.Fixtures exposing (testExample)


suite : Test
suite =
    describe "Book.Ch6" <|
        List.indexedMap
            testExample
            --
            -- alwayso
            --
            [ { input = runAtMost 1 (\_ -> alwayso)
              , output = "(_0)"
              }
            , { input =
                    runAtMost 1
                        (\_ ->
                            conde
                                [ [ succeed ]
                                , [ alwayso ]
                                ]
                        )
              , output = "(_0)"
              }
            , { input = runAtMost 5 (\_ -> alwayso)
              , output = "(_0 _0 _0 _0 _0)"
              }
            , { input =
                    runAtMost 5
                        (\q ->
                            conj
                                [ equals onion q
                                , alwayso
                                ]
                        )
              , output = "(onion onion onion onion onion)"
              }
            , { input =
                    runAtMost 1
                        (\q ->
                            conj
                                [ equals garlic q
                                , succeed
                                , equals onion q
                                ]
                        )
              , output = "()"
              }
            , { input =
                    runAtMost 1
                        (\q ->
                            conj
                                [ conde
                                    [ [ equals garlic q, alwayso ]
                                    , [ equals onion q ]
                                    ]
                                , equals onion q
                                ]
                        )
              , output = "(onion)"
              }
            , { input =
                    runAtMost 5
                        (\q ->
                            conj
                                [ conde
                                    [ [ equals garlic q, alwayso ]
                                    , [ equals onion q, alwayso ]
                                    ]
                                , equals onion q
                                ]
                        )
              , output = "(onion onion onion onion onion)"
              }

            --
            -- nevero
            --
            , { input =
                    runAtMost 1
                        (\_ ->
                            conj
                                [ fail
                                , nevero
                                ]
                        )
              , output = "()"
              }
            , { input =
                    runAtMost 1
                        (\_ ->
                            conde
                                [ [ succeed ]
                                , [ nevero ]
                                ]
                        )
              , output = "(_0)"
              }
            , { input =
                    runAtMost 1
                        (\_ ->
                            conde
                                --
                                -- N.B. This gives the same values as the previous test
                                --      thanks to "The Law of Swapping conde Lines".
                                --
                                [ [ nevero ]
                                , [ succeed ]
                                ]
                        )
              , output = "(_0)"
              }
            , { input =
                    runAtMost 5
                        (\_ ->
                            conde
                                [ [ nevero ]
                                , [ alwayso ]
                                , [ nevero ]
                                ]
                        )
              , output = "(_0 _0 _0 _0 _0)"
              }
            , { input =
                    runAtMost 6
                        (\q ->
                            conde
                                [ [ equals spicy q, nevero ]
                                , [ equals hot q, nevero ]
                                , [ equals apple q, alwayso ]
                                , [ equals cider q, alwayso ]
                                ]
                        )
              , output = "(apple cider apple cider apple cider)"
              }
            , { input = runAtMost 10 (\_ -> veryRecursiveo)
              , output = "(_0 _0 _0 _0 _0 _0 _0 _0 _0 _0)"
              }
            ]



-- VALUES


apple =
    string "apple"


cider =
    string "cider"


garlic =
    string "garlic"


hot =
    string "hot"


onion =
    string "onion"


spicy =
    string "spicy"
