module Book.Ch9 exposing
    ( notPastao
    , onceo
    , pasta
    )

import Logic exposing (..)


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
