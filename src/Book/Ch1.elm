module Book.Ch1 exposing (teacupo, tea, cup)

{-| Relations from Chapter 1 of The Reasoned Schemer (2nd Edition).

@docs teacupo, tea, cup

-}

import Logic exposing (..)


{-| -}
teacupo : Value a -> Goal a
teacupo t =
    disj2 (equals tea t) (equals cup t)


{-| -}
tea : Value a
tea =
    string "tea"


{-| -}
cup : Value a
cup =
    string "cup"
