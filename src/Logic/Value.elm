module Logic.Value exposing (Value(..), toString)

{-| Represents all the values that can be associated with variables.

@docs Value, toString

-}

import Logic.Variable as Variable exposing (Variable)


{-| It is polymorphic in `a` so that you can specify your own constants.

    type alias ValueS =
        Value String

    tea : ValueS
    tea =
        Const "tea"

The constant type can be as rich as you need it to be.

-}
type Value a
    = Const a
    | Var Variable
    | ReifiedVar Int
    | Null
    | Pair (Value a) (Value a)


{-| Convert to a representation that is useful for debugging and testing.

    u : ValueS
    u =
        Var (Variable "u" 0)

    r : ValueS
    r =
        ReifiedVar 9

    toString identity tea == "tea"
    toString identity u == "u.0"
    toString identity r == "_9"
    toString identity Null == "()"
    toString identity (Pair tea u) == "(tea . u.0)"
    toString identity (Pair tea (Pair u (Pair r Null))) == "(tea u.0 _9)"
    toString identity (Pair (Pair tea u) (Pair r Null)) == "((tea . u.0) _9)"

-}
toString : (a -> String) -> Value a -> String
toString =
    toStringHelper "("


toStringHelper : String -> (a -> String) -> Value a -> String
toStringHelper prefix stringify val =
    case val of
        Const a ->
            stringify a

        Var var ->
            Variable.toString var

        ReifiedVar n ->
            "_" ++ String.fromInt n

        Null ->
            "()"

        Pair left right ->
            String.concat
                [ prefix
                , toStringHelper "(" stringify left
                , case right of
                    Null ->
                        ")"

                    Pair _ _ ->
                        toStringHelper " " stringify right

                    _ ->
                        " . " ++ toStringHelper "(" stringify right ++ ")"
                ]
