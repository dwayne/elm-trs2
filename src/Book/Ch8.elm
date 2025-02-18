module Book.Ch8 exposing
    ( equalLo
    , lessThanLo
    , lessThanOrEqualLo
    , lessThanOrEqualo
    , lessThano
    , timeso
    )

import Book.Ch7 exposing (..)
import Logic exposing (..)



--
-- TODO:
--
-- - Division
-- - Logarithm
-- - Exponentiation
--


timeso : Value a -> Value a -> Value a -> Goal a
timeso n m p =
    --
    -- n * m = p
    --
    conde
        -- n == 0, m
        --
        -- 0 * m = 0 = p
        [ [ equals numZero n, equals numZero p ]

        -- n >= 1, m == 0
        --
        -- n * 0 = 0 = p
        , [ poso n, equals numZero m, equals numZero p ]

        -- n == 1, m >= 1
        --
        -- 1 * m = m = p
        , [ equals numOne n, poso m, equals m p ]

        -- n >= 2, m == 1
        --
        -- n * 1 = n = p
        , [ greaterThan1o n, equals numOne m, equals n p ]

        -- n is even, n >= 2, m >= 2
        --
        -- n = 2x where x >= 1
        --
        -- 2x * m = 2(x * m) = 2z = p where z >= 1
        , [ fresh2
                (\x z ->
                    conj
                        [ equals (cons zero x) n
                        , poso x
                        , equals (cons zero z) p
                        , poso z
                        , greaterThan1o m
                        , lazy (\_ -> timeso x m z)
                        ]
                )
          ]

        -- n is odd, n >= 3, m is even, m >= 2
        --
        -- n * m = m * n = p
        , [ fresh2
                (\x y ->
                    conj
                        [ equals (cons one x) n
                        , poso x
                        , equals (cons zero y) m
                        , poso y

                        --
                        -- Why use recursion here?
                        --
                        -- We could just as well do what we did in the
                        -- previous conde line, i.e.
                        --
                        -- m = 2y where y >= 1
                        --
                        -- n * 2y = 2(n * y) = 2z = p where z >= 1
                        --
                        , lazy (\_ -> timeso m n p)
                        ]
                )
          ]

        -- n is odd, n >= 3, m is odd, m >= 3
        --
        -- n = 2x + 1 where x >= 1
        -- m = 2y + 1 where y >= 1
        --
        -- n * m = (2x + 1) * m = 2xm + m = 2(x * m) + m = 2z + m = p where z >= 3
        , [ fresh2
                (\x y ->
                    conj
                        [ equals (cons one x) n
                        , poso x
                        , equals (cons one y) m
                        , poso y
                        , lazy (\_ -> oddTimeso x n m p)
                        ]
                )
          ]
        ]


oddTimeso : Value a -> Value a -> Value a -> Value a -> Goal a
oddTimeso x n m p =
    --
    -- n is odd, n >= 3, m is odd, m >= 3
    --
    -- n = 2x + 1 where x >= 1
    -- m = 2y + 1 where y >= 1
    --
    -- n * m = (2x + 1) * m = 2xm + m = 2(x * m) + m = 2q + m = p where q >= 3
    --
    fresh
        (\q ->
            conj
                [ boundTimeso q p n m -- Why is this needed?
                , timeso x m q -- x * m = q
                , pluso (cons zero q) m p -- 2q + m = p
                ]
        )


boundTimeso : Value a -> Value a -> Value a -> Value a -> Goal a
boundTimeso q p n m =
    --
    -- If we are trying to see if n * m = p, then
    -- any n > p will not work. So, we can stop
    -- searching when n is equal to p. Or, to
    -- make it easier to test: (timeso n m p) can
    -- only succeed if the lengths (in bits) of n
    -- and m do not exceed the length (in bits)
    -- of p.
    --
    -- i.e. Let |a| = the length (in bits) of a. Then,
    -- we need:
    --
    -- |n| + |m| <= |p|
    --
    conde
        -- q == 0, p >= 1
        --
        -- |q| < |p|
        [ [ equals numZero q, poso p ]

        -- q >= 1, p >= 1
        , [ fresh7
                (\a0 a1 a2 a3 x y z ->
                    conj
                        [ equals (cons a0 x) q
                        , equals (cons a1 y) p
                        , conde
                            -- n == 0, m >= 1
                            --
                            -- We've run out of bits in n so start using bits from m.
                            [ [ equals numZero n
                              , equals (cons a2 z) m
                              , lazy (\_ -> boundTimeso x y z numZero)
                              ]

                            -- n >= 1
                            , [ equals (cons a3 z) n
                              , lazy (\_ -> boundTimeso x y z m)
                              ]
                            ]
                        ]
                )
          ]
        ]


equalLo : Value a -> Value a -> Goal a
equalLo n m =
    conde
        [ [ equals numZero n, equals numZero m ]
        , [ equals numOne n, equals numOne m ]
        , [ fresh4
                (\a x b y ->
                    conj
                        -- n is of the form `(,a . ,x) with x >= 1
                        [ equals (cons a x) n
                        , poso x

                        -- m is of the form `(,b . ,y) with y >= 1
                        , equals (cons b y) m
                        , poso y

                        -- x and y should have the same length
                        , lazy (\_ -> equalLo x y)

                        -- N.B. a and b are not related in any way
                        ]
                )
          ]
        ]


lessThanLo : Value a -> Value a -> Goal a
lessThanLo n m =
    --
    -- N.B. lessThanLo n n, has no value.
    --
    conde
        [ [ equals numZero n, poso m ]
        , [ equals numOne n, greaterThan1o m ]
        , [ fresh4
                (\a x b y ->
                    conj
                        -- n is of the form `(,a . ,x) with x >= 1
                        [ equals (cons a x) n
                        , poso x

                        -- m is of the form `(,b . ,y) with y >= 1
                        , equals (cons b y) m
                        , poso y

                        -- the length of x should be less than the length of y
                        , lazy (\_ -> lessThanLo x y)

                        -- N.B. a and b are not related in any way
                        ]
                )
          ]
        ]


lessThanOrEqualLo : Value a -> Value a -> Goal a
lessThanOrEqualLo n m =
    --
    --conde
    --    [ [ equalLo n m ]
    --    , [ lessThanLo n m ]
    --    ]
    --
    -- or,
    --
    --disj
    --    [ equalLo n m
    --    , lessThanLo n m
    --    ]
    --
    -- or,
    --
    disj2
        (equalLo n m)
        (lessThanLo n m)


lessThano : Value a -> Value a -> Goal a
lessThano n m =
    --
    -- n < m iff either
    --
    -- 1. |n| < |m|, or
    -- 2. |n| = |m| and there exists an x >= 1 such that n + x = m
    --
    -- N.B. lessThano n n, has no value since it uses lessThanLo.
    --
    conde
        [ [ lessThanLo n m ]
        , [ equalLo n m
          , fresh
                (\x ->
                    conj
                        [ poso x
                        , pluso n x m
                        ]
                )
          ]
        ]


lessThanOrEqualo : Value a -> Value a -> Goal a
lessThanOrEqualo n m =
    disj2
        (equals n m)
        (lessThano n m)
