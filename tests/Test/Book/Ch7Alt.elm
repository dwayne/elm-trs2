module Test.Book.Ch7Alt exposing (suite)

import Book.Ch7Alt exposing (..)
import Logic exposing (..)
import Test exposing (Test, describe)
import Test.Book.Fixtures exposing (testExample, toOutput)


suite : Test
suite =
    describe "Book.Ch7Alt" <|
        List.indexedMap
            testExample
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
            , { input =
                    run2
                        (\x y ->
                            bitAndo x y one
                        )
              , output = "((1 1))"
              }
            ]
