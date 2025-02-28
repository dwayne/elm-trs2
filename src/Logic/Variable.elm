module Logic.Variable exposing
    ( Variable, Id
    , toString
    )

{-| Variables or logic variables.

@docs Variable, Id


# Debug

@docs toString

-}


{-| Variables are uniquely identified by their `name` and `id`.

**N.B.** _The empty string name is reserved for use internally._

-}
type alias Variable =
    { name : String
    , id : Id
    }


{-| Any integral value can serve as an identifier.
-}
type alias Id =
    Int


{-| Convert to a representation that's useful for debugging and testing.

    toString (Variable "x" 3) == "x.3"

-}
toString : Variable -> String
toString { name, id } =
    name ++ "." ++ String.fromInt id
