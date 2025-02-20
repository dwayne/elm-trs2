module Book.Ch9 exposing
    ( bumpo
    , genAndTestPluso
    , notPastao
    , onceo
    , pasta
    )

import Book.Ch7 exposing (..)
import Logic exposing (..)



--
-- TODO:
--
-- - enumeratePluso
-- - enumerateo
--


notPastao : Value a -> Goal a
notPastao x =
    conda
        [ [ equals pasta x, fail ]
        , [ succeed, succeed ]
        ]


pasta : Value a
pasta =
    string "pasta"


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
