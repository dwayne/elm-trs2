module Book.Ch9 exposing
    ( notPastao, pasta
    , onceo
    , bumpo, genAndTestPluso, enumeratePluso, enumerateo
    )

{-| Relations from Chapter 9 of The Reasoned Schemer (2nd Edition).

@docs notPastao, pasta
@docs onceo
@docs bumpo, genAndTestPluso, enumeratePluso, enumerateo

-}

import Book.Ch7 exposing (..)
import Logic exposing (..)


{-| -}
notPastao : Value a -> Goal a
notPastao x =
    conda
        [ [ equals pasta x, fail ]
        , [ succeed, succeed ]
        ]


{-| -}
pasta : Value a
pasta =
    string "pasta"


{-| -}
onceo : Goal a -> Goal a
onceo g =
    --
    -- N.B. onceo is equivalent to Logic.once.
    --
    --      We only define it here to show that it
    --      can be derived from other primitives.
    --
    condu
        [ [ g, succeed ]
        , [ succeed, fail ]
        ]


{-| -}
bumpo : Value a -> Value a -> Goal a
bumpo n x =
    disj2
        (equals n x)
        (fresh
            (\m ->
                conj
                    [ minuso n numOne m
                    , lazy (\_ -> bumpo m x)
                    ]
            )
        )


{-| -}
genAndTestPluso : Value a -> Value a -> Value a -> Goal a
genAndTestPluso i j k =
    onceo <|
        fresh3
            (\x y z ->
                conj
                    [ pluso x y z
                    , equals i x
                    , equals j y
                    , equals k z
                    ]
            )


{-| -}
enumeratePluso : Value a -> Value a -> Goal a
enumeratePluso r n =
    fresh3
        (\i j k ->
            conj
                [ bumpo n i
                , bumpo n j
                , pluso i j k
                , genAndTestPluso i j k
                , equals (list [ i, j, k ]) r
                ]
        )


{-| -}
enumerateo : (Value a -> Value a -> Value a -> Goal a) -> Value a -> Value a -> Goal a
enumerateo op r n =
    fresh3
        (\i j k ->
            conj
                [ bumpo n i
                , bumpo n j
                , op i j k
                , onceo <|
                    fresh3
                        (\x y z ->
                            conj
                                [ op x y z
                                , equals i x
                                , equals j y
                                , equals k z
                                ]
                        )
                , equals (list [ i, j, k ]) r
                ]
        )
