module Book.Ch9 exposing
    ( bumpo
    , notPastao
    , onceo
    , pasta
    )

import Book.Ch7 exposing (..)
import Logic exposing (..)



--
-- TODO:
--
-- - genAndTestPluso
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
