module Book.Ch5 exposing (memo, rembero)

{-| Relations from Chapter 5 of The Reasoned Schemer (2nd Edition).

@docs memo, rembero

-}

import Book.Ch2 exposing (..)
import Logic exposing (..)


{-| -}
memo : Value a -> Value a -> Value a -> Goal a
memo x l out =
    conde
        [ [ caro l x, equals l out ]
        , [ fresh
                (\d ->
                    conj
                        [ cdro l d
                        , lazy (\_ -> memo x d out)
                        ]
                )
          ]
        ]


{-| -}
rembero : Value a -> Value a -> Value a -> Goal a
rembero x l out =
    conde
        [ [ nullo l, equals null out ]
        , [ conso x out l ]
        , [ fresh3
                (\a d res ->
                    conj
                        [ conso a d l
                        , conso a res out
                        , lazy (\_ -> rembero x d res)
                        ]
                )
          ]
        ]
