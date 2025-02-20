module Book.Ch8 exposing
    ( divo
    , equalLo
    , expo
    , lessThanLo
    , lessThanOrEqualLo
    , lessThanOrEqualo
    , lessThano
    , logo
    , originalDivo
    , splito
    , timeso
    )

import Book.Ch4 exposing (..)
import Book.Ch7 exposing (..)
import Logic exposing (..)


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


originalDivo : Value a -> Value a -> Value a -> Value a -> Goal a
originalDivo n m q r =
    --
    -- n ÷ m produces q and r such that n = m * q + r and 0 <= r < m
    --
    conde
        -- Case 1: n < m
        --
        -- q == 0, n == r, n < m
        [ [ equals numZero q, equals n r, lessThano n m ]

        -- Case 2: n == m
        --
        -- q == 1, r == 0, n == m, r < m
        --
        -- Q: Why is r < m needed?
        -- A: 1. r < m => m > 0 so that division by zero is avoided
        --    2. r < m => n > r, i.e. n /= r so that Case 2 does not overlap with Case 1
        --
        , [ equals numOne q, equals numZero r, equals n m, lessThano r m ]

        -- Case 3: n > m
        --
        -- m < n, r < m
        , [ lessThano m n
          , lessThano r m
          , fresh
                (\mq ->
                    conj
                        -- Q: Why not use lessThano?
                        -- A: lessThanOrEqualLo is a closer approximation of lessThano.
                        --    If mq <= n then certainly |mq| <= |n|.
                        --
                        [ lessThanOrEqualLo mq n -- |mq| <= |n|
                        , timeso m q mq -- mq = m * q
                        , pluso mq r n -- n = mq + r
                        ]
                )
          ]
        ]


splito : Value a -> Value a -> Value a -> Value a -> Goal a
splito n r l h =
    --
    -- Let p = |r| + 1.
    --
    -- Split n into l and h such that n = h * 2^p + l,
    -- where l has at most p bits.
    --
    -- N.B. The complexity appears because we must not allow
    --      the list (0) to represent a number.
    --
    conde
        -- n == 0, h == 0, l == 0
        --
        -- It doesn't matter the |r|+1.
        [ [ equals numZero n, equals numZero h, equals numZero l ]

        -- n is even, n >= 2, |r|+1 == 1
        , [ fresh2
                (\b nn ->
                    conj
                        [ equals (cons zero (cons b nn)) n
                        , equals numZero r
                        , equals (cons b nn) h
                        , equals numZero l
                        ]
                )
          ]

        -- n is odd, n >= 1, |r|+1 == 1
        , [ fresh
                (\nn ->
                    conj
                        [ equals (cons one nn) n
                        , equals numZero r
                        , equals nn h
                        , equals numOne l
                        ]
                )
          ]

        -- n is even, n >= 2, |r|+1 >= 2, l == 0
        , [ fresh4
                (\b nn a rr ->
                    conj
                        [ equals (cons zero (cons b nn)) n
                        , equals (cons a rr) r
                        , equals numZero l
                        , lazy (\_ -> splito (cons b nn) rr numZero h)
                        ]
                )
          ]

        -- n is odd, n >= 1, |r|+1 >= 2, l == 1
        , [ fresh3
                (\nn a rr ->
                    conj
                        [ equals (cons one nn) n
                        , equals (cons a rr) r
                        , equals numOne l
                        , lazy (\_ -> splito nn rr numZero h)
                        ]
                )
          ]

        -- n >= 1, |r|+1 >= 2, l >= 2
        , [ fresh5
                (\b nn a rr ll ->
                    conj
                        [ equals (cons b nn) n
                        , equals (cons a rr) r
                        , equals (cons b ll) l
                        , poso ll
                        , lazy (\_ -> splito nn rr ll h)
                        ]
                )
          ]
        ]


divo : Value a -> Value a -> Value a -> Value a -> Goal a
divo n m q r =
    --
    -- n ÷ m produces q and r such that n = m * q + r and 0 <= r < m
    --
    conde
        -- Case 1: n < m
        --
        -- q == 0, r == n, n < m
        [ [ equals numZero q, equals r n, lessThano n m ]

        -- Case 2: |n| == |m|, i.e. n and m have the same number of bits
        --
        -- q == 1, |n| == |m|, n = m + r, r < m
        , [ equals numOne q
          , equalLo m n
          , pluso r m n
          , lessThano r m
          ]

        -- Case 3: |n| > |m|, i.e. n has more bits than m
        --
        -- q >= 1, |m| < |n|, r < m
        , [ poso q
          , lessThanLo m n
          , lessThano r m
          , lazy (\_ -> nWiderThanMo n m q r)
          ]
        ]


nWiderThanMo : Value a -> Value a -> Value a -> Value a -> Goal a
nWiderThanMo n m q r =
    fresh8
        (\nHigh nLow qHigh qLow mqLow mrqLow rr rHigh ->
            conj
                [ splito n r nLow nHigh
                , splito q r qLow qHigh
                , conde
                    -- nHigh == 0
                    [ [ equals numZero nHigh
                      , equals numZero qHigh
                      , minuso nLow r mqLow
                      , timeso m qLow mqLow
                      ]

                    -- nHigh >= 1
                    , [ poso nHigh
                      , timeso m qLow mqLow
                      , pluso r mqLow mrqLow
                      , minuso mrqLow nLow rr
                      , splito rr r numZero rHigh
                      , divo nHigh m qHigh rHigh
                      ]
                    ]
                ]
        )


logo : Value a -> Value a -> Value a -> Value a -> Goal a
logo n b q r =
    --
    -- logo n b q r holds if n = b^q + r,
    --
    -- where r >= 0 and q is the largest number that satisfies the relation.
    --
    -- N.B. I currently don't fully understand all the details of logo and
    --      its associated helper functions, exp2o and baseThreeOrMoreo.
    --
    conde
        -- q == 0, n <= b
        --
        -- n = b^0 + r
        --   = 1 + r
        [ [ equals numZero q
          , lessThano n b
          , pluso r numOne n
          ]

        -- q == 1, b > 1, |n| = |b|
        --
        -- n = b^1 + r
        --   = b + r
        , [ equals numOne q
          , greaterThan1o b
          , equalLo n b
          , pluso r b n
          ]

        -- q >= 1, b == 1
        --
        -- n = 1^q + r
        --   = 1 + r
        , [ equals numOne b
          , poso q
          , pluso r numOne n
          ]

        -- q >= 1, b == 0
        --
        -- n = 0^q + r
        --   = 0 + r
        --   = r
        , [ equals numZero b
          , poso q
          , equals r n
          ]

        -- b == 2
        , [ equals numTwo b
          , fresh3
                (\a ad dd ->
                    conj
                        -- dd >= 1, n >= 4
                        [ poso dd
                        , equals (cons a (cons ad dd)) n
                        , exp2o n numZero q
                        , fresh
                            (\s ->
                                splito n dd r s
                            )
                        ]
                )
          ]

        -- b >= 3, |b| < |n|
        , [ lessThanOrEqualo numThree b
          , lessThanLo b n
          , baseThreeOrMoreo n b q r
          ]
        ]


numTwo : Value a
numTwo =
    list [ zero, one ]


numThree : Value a
numThree =
    list [ one, one ]


exp2o : Value a -> Value a -> Value a -> Goal a
exp2o n b q =
    conde
        [ [ equals numOne n, equals numZero q ]
        , [ greaterThan1o n
          , equals numOne q
          , fresh
                (\s ->
                    splito n b s numOne
                )
          ]
        , [ fresh2
                (\q1 b2 ->
                    conj
                        [ equals (cons zero q1) q
                        , poso q1
                        , lessThanLo b n
                        , appendo b (cons one b) b2
                        , lazy (\_ -> exp2o n b2 q1)
                        ]
                )
          ]
        , [ fresh4
                (\q1 nHigh b2 s ->
                    conj
                        [ equals (cons one q1) q
                        , poso q1
                        , poso nHigh
                        , splito n b s nHigh
                        , appendo b (cons one b) b2
                        , lazy (\_ -> exp2o nHigh b2 q1)
                        ]
                )
          ]
        ]


baseThreeOrMoreo : Value a -> Value a -> Value a -> Value a -> Goal a
baseThreeOrMoreo n b q r =
    fresh7
        (\bw1 bw nw nw1 qLow1 qLow s ->
            conj
                [ exp2o b numZero bw1
                , pluso bw1 numOne bw
                , lessThanLo q n
                , fresh2
                    (\q1 bwq1 ->
                        conj
                            [ pluso q numOne q1
                            , timeso bw q1 bwq1
                            , lessThano nw1 bwq1
                            ]
                    )
                , exp2o n numZero nw1
                , pluso nw1 numOne nw
                , divo nw bw qLow1 s
                , pluso qLow numOne qLow1
                , lessThanOrEqualLo qLow q
                , fresh5
                    (\bqLow qHigh t qdHigh qd ->
                        conj
                            [ repeatedMulo b qLow bqLow
                            , divo nw bw1 qHigh t
                            , pluso qLow qdHigh qHigh
                            , pluso qLow qd q
                            , lessThanOrEqualo qd qdHigh
                            , fresh3
                                (\bqd bq1 bq ->
                                    conj
                                        [ repeatedMulo b qd bqd
                                        , timeso bqLow bqd bq
                                        , timeso b bq bq1
                                        , pluso bq r n
                                        , lessThano n bq1
                                        ]
                                )
                            ]
                    )
                ]
        )


repeatedMulo : Value a -> Value a -> Value a -> Goal a
repeatedMulo n q nq =
    --
    -- n * n * ... * n (q times)
    --
    conde
        -- q == 0, n >= 1
        --
        -- nq = 1
        [ [ poso n, equals numZero q, equals numOne nq ]

        -- q = 1
        --
        -- nq = n
        , [ equals numOne q, equals n nq ]

        -- q > 1
        , [ fresh2
                (\q1 nq1 ->
                    conj
                        -- q1 + 1 = q, i.e. q1 = q - 1
                        [ pluso q1 numOne q

                        -- nq1 = n * n * ... * n (q-1 times)
                        , lazy (\_ -> repeatedMulo n q1 nq1)

                        -- nq = n * nq1
                        --    = n * n * n * ... * n (q times)
                        , timeso nq1 n nq
                        ]
                )
          ]
        ]


expo : Value a -> Value a -> Value a -> Goal a
expo b q n =
    logo n b q numZero
