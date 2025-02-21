module Book.Ch2 exposing
    ( caro, cdro
    , conso, consoUsingEquals
    , nullo, pairo, singletono
    , caroUsingConso, cdroUsingConso
    )

{-| Relations from Chapter 2 of The Reasoned Schemer (2nd Edition).

@docs caro, cdro
@docs conso, consoUsingEquals
@docs nullo, pairo, singletono
@docs caroUsingConso, cdroUsingConso

-}

import Logic exposing (..)


{-| -}
caro : Value a -> Value a -> Goal a
caro p a =
    fresh
        (\d ->
            equals (cons a d) p
        )


{-| -}
cdro : Value a -> Value a -> Goal a
cdro p d =
    fresh
        (\a ->
            equals (cons a d) p
        )


{-| -}
conso : Value a -> Value a -> Value a -> Goal a
conso a d p =
    conj
        [ caro p a
        , cdro p d
        ]


{-| -}
consoUsingEquals : Value a -> Value a -> Value a -> Goal a
consoUsingEquals a d p =
    equals (cons a d) p


{-| -}
nullo : Value a -> Goal a
nullo x =
    equals null x


{-| -}
pairo : Value a -> Goal a
pairo p =
    fresh2
        (\a d ->
            conso a d p
        )



--singletono : Value a -> Goal a
--singletono l =
--    fresh
--        (\d ->
--            conj
--                [ cdro l d
--                , nullo d
--                ]
--        )


{-| -}
singletono : Value a -> Goal a
singletono l =
    --
    -- An alternative definition of singletono.
    -- See frame 32 on page 43.
    --
    fresh (\a -> equals (list [ a ]) l)



--
-- Define both caro and cdro using conso.
--


{-| -}
caroUsingConso : Value a -> Value a -> Goal a
caroUsingConso p a =
    fresh
        (\d ->
            conso a d p
        )


{-| -}
cdroUsingConso : Value a -> Value a -> Goal a
cdroUsingConso p d =
    fresh
        (\a ->
            conso a d p
        )
