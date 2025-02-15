module Book.Ch6 exposing
    ( alwayso
    , nevero
    , veryRecursiveo
    )

import Logic exposing (..)


alwayso : Goal a
alwayso =
    --
    -- N.B. alwayso is equivalent to Logic.always.
    --
    --      We only define it here to show that it
    --      can be derived from other primitives.
    --
    conde
        [ [ succeed ]
        , [ lazy (\_ -> alwayso) ]
        ]


nevero : Goal a
nevero =
    --
    -- N.B. nevero is equivalent to Logic.never.
    --
    --      We only define it here to show that it
    --      can be derived from other primitives.
    --
    lazy (\_ -> nevero)


veryRecursiveo : Goal a
veryRecursiveo =
    conde
        [ [ nevero ]
        , [ lazy (\_ -> veryRecursiveo) ]
        , [ alwayso ]
        , [ lazy (\_ -> veryRecursiveo) ]
        , [ nevero ]
        ]
