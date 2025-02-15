module Book.Ch1 exposing
    ( cup
    , tea
    , teacupo
    )

import Logic exposing (..)


teacupo : Value a -> Goal a
teacupo t =
    disj2 (equals tea t) (equals cup t)


cup : Value a
cup =
    string "cup"


tea : Value a
tea =
    string "tea"
