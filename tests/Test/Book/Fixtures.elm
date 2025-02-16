module Test.Book.Fixtures exposing (testExample, toOutput)

import Expect
import Logic exposing (Value)
import Test exposing (Test, test)


testExample : Int -> { input : List (Value String), output : String } -> Test
testExample n { input, output } =
    test ("Example " ++ String.fromInt n) <|
        \_ ->
            Logic.toString input
                |> Expect.equal output


toOutput : List String -> String
toOutput strings =
    "(" ++ String.join " " strings ++ ")"
