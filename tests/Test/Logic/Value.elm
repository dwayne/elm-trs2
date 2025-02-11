module Test.Logic.Value exposing (suite)

import Expect
import Logic.Value as Value exposing (Value(..))
import Test exposing (Test, describe, test)
import Test.Logic.Fixtures exposing (..)


suite : Test
suite =
    describe "Logic.Value"
        [ toStringSuite
        ]


toStringSuite : Test
toStringSuite =
    describe "toString" <|
        List.map
            (testToString identity)
            [ { value = vu
              , expected = "u.0"
              }
            , { value = a
              , expected = "a"
              }
            , { value = ReifiedVar 0
              , expected = "_0"
              }
            , { value = Null
              , expected = "()"
              }
            , { value = Pair vv b
              , expected = "(v.1 . b)"
              }
            , { value = Pair Null Null
              , expected = "(())"
              }
            , { value = Pair Null (Pair vw c)
              , expected = "(() w.2 . c)"
              }
            , { value = Pair (Pair vx vy) (Pair vz Null)
              , expected = "((x.3 . y.4) z.5)"
              }
            ]


testToString : (a -> String) -> { value : Value a, expected : String } -> Test
testToString stringify { value, expected } =
    test (Debug.toString { value = value }) <|
        \_ ->
            Value.toString stringify value
                |> Expect.equal expected
