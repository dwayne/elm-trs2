module Test.Logic.Stream exposing (suite)

import Expect
import Logic.Stream as Stream exposing (Stream(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Logic.Stream" <|
        List.indexedMap
            testExample
            --
            -- singleton
            --
            [ { input = Stream.singleton 1
              , length = Unbounded
              , output = [ 1 ]
              }

            --
            -- finite lists
            --
            , { input = evensLessThan10
              , length = Unbounded
              , output = [ 0, 2, 4, 6, 8 ]
              }
            , { input = oddsLessThan10
              , length = Unbounded
              , output = [ 1, 3, 5, 7, 9 ]
              }

            --
            -- append
            --
            , { input = Stream.append evensLessThan10 oddsLessThan10
              , length = Unbounded
              , output = [ 0, 2, 4, 6, 8, 1, 3, 5, 7, 9 ]
              }
            , { input = Stream.append evensLessThan10 odds
              , length = AtMost 10
              , output = [ 0, 2, 4, 6, 8, 1, 3, 5, 7, 9 ]
              }
            , { input = Stream.append evens oddsLessThan10
              , length = AtMost 10
              , output = [ 0, 1, 3, 5, 7, 9, 2, 4, 6, 8 ]
              }
            , { input = naturals
              , length = AtMost 10
              , output = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
              }

            --
            -- appendMap
            --
            , { input = Stream.appendMap repeat naturals
              , length = AtMost 100
              , output = [ 0, 0, 1, 0, 0, 1, 0, 2, 0, 1, 0, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0 ]
              }
            ]


type Length
    = AtMost Int
    | Unbounded


testExample : Int -> { input : Stream Int, length : Length, output : List Int } -> Test
testExample i { input, length, output } =
    test ("Example " ++ String.fromInt i) <|
        \_ ->
            let
                toList =
                    case length of
                        AtMost n ->
                            Stream.take n

                        Unbounded ->
                            Stream.toList
            in
            input
                |> toList
                |> Expect.equal output



-- HELPERS


evensLessThan10 : Stream Int
evensLessThan10 =
    Cons 0 (Cons 2 (Cons 4 (Cons 6 (Cons 8 Empty))))


oddsLessThan10 : Stream Int
oddsLessThan10 =
    Cons 1 (Cons 3 (Cons 5 (Cons 7 (Cons 9 Empty))))


arithmeticProgression : Int -> Int -> Stream Int
arithmeticProgression a d =
    Cons a <| Suspend (\_ -> arithmeticProgression (a + d) d)


evens : Stream Int
evens =
    arithmeticProgression 0 2


odds : Stream Int
odds =
    arithmeticProgression 1 2


naturals : Stream Int
naturals =
    Stream.append evens odds


repeat : a -> Stream a
repeat a =
    Cons a <| Suspend (\_ -> repeat a)
