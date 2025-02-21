module Logic.Value exposing (Value(..), toString)

{-| Values.

@docs Value, toString

-}

import Logic.Variable as Variable exposing (Variable)


{-| -}
type Value a
    = Const a
    | Var Variable
    | ReifiedVar Int
    | Null
    | Pair (Value a) (Value a)


{-| -}
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
