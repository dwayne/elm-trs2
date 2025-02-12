module Book.Ch3 exposing
    ( listo
    , lolo
    , loso
    , membero
    , properMembero
    )

import Book.Ch2 exposing (..)
import Logic exposing (..)


listo : Value a -> Goal a
listo l =
    conde
        [ [ nullo l ]
        , [ fresh
                (\d ->
                    conj
                        [ cdro l d
                        , lazy (\_ -> listo d)
                        ]
                )
          ]
        ]


lolo : Value a -> Goal a
lolo l =
    conde
        [ [ nullo l ]
        , [ fresh
                (\a ->
                    conj
                        [ caro l a
                        , listo a
                        ]
                )
          , fresh
                (\d ->
                    conj
                        [ cdro l d
                        , lazy (\_ -> lolo d)
                        ]
                )
          ]
        ]


loso : Value a -> Goal a
loso l =
    conde
        [ [ nullo l ]
        , [ fresh
                (\a ->
                    conj
                        [ caro l a
                        , singletono a
                        ]
                )
          , fresh
                (\d ->
                    conj
                        [ cdro l d
                        , lazy (\_ -> loso d)
                        ]
                )
          ]
        ]


membero : Value a -> Value a -> Goal a
membero x l =
    conde
        [ [ caro l x ]
        , [ fresh
                (\d ->
                    conj
                        [ cdro l d
                        , lazy (\_ -> membero x d)
                        ]
                )
          ]
        ]


properMembero : Value a -> Value a -> Goal a
properMembero x l =
    conde
        [ [ caro l x
          , fresh
                (\d ->
                    conj
                        [ cdro l d
                        , listo d
                        ]
                )
          ]
        , [ fresh
                (\d ->
                    conj
                        [ cdro l d
                        , lazy (\_ -> properMembero x d)
                        ]
                )
          ]
        ]
