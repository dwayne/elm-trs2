module Book.Ch7Alt exposing
    ( bitAndo
    , bitXoro
    , one
    , zero
    )

import Logic exposing (..)



--
-- Using bitNando as the starting primitive relation.
--


zero : Value a
zero =
    int 0


one : Value a
one =
    int 1


bitXoro : Value a -> Value a -> Value a -> Goal a
bitXoro x y r =
    fresh3
        (\s t u ->
            conj
                [ bitNando x y s
                , bitNando s y u
                , bitNando x s t
                , bitNando t u r
                ]
        )


bitNando : Value a -> Value a -> Value a -> Goal a
bitNando x y r =
    conde
        [ [ equals zero x, equals zero y, equals one r ]
        , [ equals zero x, equals one y, equals one r ]
        , [ equals one x, equals zero y, equals one r ]
        , [ equals one x, equals one y, equals zero r ]
        ]


bitAndo : Value a -> Value a -> Value a -> Goal a
bitAndo x y r =
    fresh
        (\s ->
            conj
                [ bitNando x y s
                , bitNoto s r
                ]
        )


bitNoto : Value a -> Value a -> Goal a
bitNoto x r =
    bitNando x x r
