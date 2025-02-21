module Book.Ch4 exposing (appendo, swappendo, unwrapo)

{-| Relations from Chapter 4 of The Reasoned Schemer (2nd Edition).

@docs appendo, swappendo, unwrapo

-}

import Book.Ch2 exposing (..)
import Logic exposing (..)



--appendo : Value a -> Value a -> Value a -> Goal a
--appendo l t out =
--    conde
--        [ [ nullo l, equals t out ]
--        , [ fresh3
--                (\a d res ->
--                    conj
--                        [ conso a d l
--                        , lazy (\_ -> appendo d t res)
--                        , conso a res out
--                        ]
--                )
--          ]
--        ]


{-| -}
appendo : Value a -> Value a -> Value a -> Goal a
appendo l t out =
    --
    -- Apply "The First Commandment", i.e.
    --
    -- Move non-recursive goals before recursive goals.
    --
    -- This fixes frame 39 on page 61.
    --
    conde
        [ [ nullo l, equals t out ]
        , [ fresh3
                (\a d res ->
                    conj
                        [ conso a d l
                        , conso a res out
                        , lazy (\_ -> appendo d t res)
                        ]
                )
          ]
        ]


{-| -}
swappendo : Value a -> Value a -> Value a -> Goal a
swappendo l t out =
    conde
        [ [ fresh3
                (\a d res ->
                    conj
                        [ conso a d l
                        , conso a res out
                        , lazy (\_ -> swappendo d t res)
                        ]
                )
          ]
        , [ nullo l, equals t out ]
        ]


{-| -}
unwrapo : Value a -> Value a -> Goal a
unwrapo x out =
    conde
        [ [ fresh
                (\a ->
                    conj
                        [ caro x a
                        , lazy (\_ -> unwrapo a out)
                        ]
                )
          ]
        , [ equals x out ]
        ]
