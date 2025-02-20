module Book.Ch7 exposing
    ( addero
    , bitAndo
    , bitXoro
    , buildNum
    , fullAddero
    , genAddero
    , greaterThan1o
    , halfAddero
    , lengtho
    , minuso
    , numOne
    , numZero
    , one
    , pluso
    , poso
    , zero
    )

import Bitwise
import Book.Ch2 exposing (..)
import Logic exposing (..)



--
-- Using bitXoro as the starting primitive relation.
--


zero : Value a
zero =
    int 0


one : Value a
one =
    int 1


bitXoro : Value a -> Value a -> Value a -> Goal a
bitXoro x y r =
    conde
        [ [ equals zero x, equals zero y, equals zero r ]
        , [ equals zero x, equals one y, equals one r ]
        , [ equals one x, equals zero y, equals one r ]
        , [ equals one x, equals one y, equals zero r ]
        ]


bitAndo : Value a -> Value a -> Value a -> Goal a
bitAndo x y r =
    conde
        [ [ equals zero x, equals zero y, equals zero r ]
        , [ equals one x, equals zero y, equals zero r ]
        , [ equals zero x, equals one y, equals zero r ]
        , [ equals one x, equals one y, equals one r ]
        ]


halfAddero : Value a -> Value a -> Value a -> Value a -> Goal a
halfAddero x y r c =
    --
    -- x + y = r + 2c
    --
    conj
        [ bitXoro x y r
        , bitAndo x y c
        ]


fullAddero : Value a -> Value a -> Value a -> Value a -> Value a -> Goal a
fullAddero b x y r c =
    --
    -- b + x + y = r + 2c
    --
    fresh3
        (\w xy wz ->
            conj
                [ halfAddero x y w xy
                , halfAddero w b r wz
                , bitXoro xy wz c
                ]
        )


buildNum : Int -> Value a
buildNum =
    --
    -- Here's how we can improve upon the API:
    --
    -- type Natural =
    --     Natural (Value ())
    --
    -- buildNum : Int -> Natural -- or fromInt
    -- poso : Natural -> Bool    -- or isPositive
    -- ...
    --
    list << buildNumHelper



--buildNumHelper : Int -> List (Value a)
--buildNumHelper n =
--    if n <= 0 then
--        []
--    else if isEven n then
--        --zero :: buildNumHelper (n // 2)
--        zero :: buildNumHelper (Bitwise.shiftRightBy 1 n)
--    else
--        --one :: buildNumHelper ((n - 1) // 2)
--        --one :: buildNumHelper (n // 2)
--        one :: buildNumHelper (Bitwise.shiftRightBy 1 n)


buildNumHelper : Int -> List (Value a)
buildNumHelper n =
    --
    -- This definition has the non-overlapping property.
    --
    if n < 0 then
        []

    else if n >= 0 && isOdd n then
        one :: buildNumHelper (Bitwise.shiftRightBy 1 n)

    else if n > 0 && isEven n then
        zero :: buildNumHelper (Bitwise.shiftRightBy 1 n)

    else
        -- n == 0
        []


isEven : Int -> Bool
isEven =
    modBy 2 >> (==) 0


isOdd : Int -> Bool
isOdd =
    not << isEven


numZero : Value a
numZero =
    null


numOne : Value a
numOne =
    list [ one ]


poso : Value a -> Goal a
poso n =
    fresh2
        (\a d ->
            equals (cons a d) n
        )


greaterThan1o : Value a -> Goal a
greaterThan1o n =
    fresh3
        (\a ad dd ->
            equals (cons a (cons ad dd)) n
        )


addero : Value a -> Value a -> Value a -> Value a -> Goal a
addero b n m r =
    --
    -- b + n + m = r
    --
    -- where b is a bit, and n, m, and r are numbers.
    --
    conde
        -- b == 0, n >= 0, m == 0
        --
        -- 0 + n + 0 = n = r
        [ [ equals zero b, equals numZero m, equals n r ]

        -- b == 0, n == 0, m > 0
        --
        -- 0 + 0 + m = m = r
        , [ equals zero b
          , equals numZero n
          , equals m r
          , poso m
          ]

        -- b == 1, n >= 0, m == 0
        --
        -- 1 + n + 0 = 0 + n + 1 = r
        , [ equals one b
          , equals numZero m
          , lazy (\_ -> addero zero n numOne r)
          ]

        -- b == 1, n == 0, m > 0
        --
        -- 1 + 0 + m = 0 + 1 + m = r
        , [ equals one b
          , equals numZero n
          , poso m
          , lazy (\_ -> addero zero numOne m r)
          ]

        -- b, n == 1, m == 1
        --
        -- b + 1 + 1 = r
        , [ equals numOne n
          , equals numOne m
          , fresh2
                (\a c ->
                    conj
                        [ equals (list [ a, c ]) r
                        , fullAddero b one one a c
                        ]
                )
          ]

        -- b, n == 1, m > 1, r > 1
        --
        -- b + 1 + m = r
        , [ equals numOne n, lazy (\_ -> genAddero b n m r) ]

        -- b, n > 1, m == 1, r > 1
        --
        -- b + n + 1 = b + 1 + n = r
        , [ equals numOne m
          , greaterThan1o n
          , greaterThan1o r
          , lazy (\_ -> addero b numOne n r)
          ]

        -- b, n > 1, m > 1, r > 1
        --
        -- b + n + m = r
        , [ greaterThan1o n, lazy (\_ -> genAddero b n m r) ]
        ]


genAddero : Value a -> Value a -> Value a -> Value a -> Goal a
genAddero b n m r =
    --
    -- b + n + m = r
    --
    -- where b is a bit, n, m, and r are numbers
    -- such that n >= 1, m > 1, and r > 1.
    --
    fresh7
        (\a c d e x y z ->
            conj
                --
                -- n >= 1
                --
                -- if n == 1 then a = 1 and x = '()
                -- if n == 2 then a = 0 and x = '(1)
                -- if n == 3 then a = 1 and x = '(1)
                -- if n == 4 then a = 0 and x = '(0 1)
                --
                [ equals (cons a x) n

                --
                -- m > 1
                --
                -- if m == 2 then d = 0 and y = '(1)
                -- if m == 3 then d = 1 and y = '(1)
                -- if m == 4 then d = 0 and y = '(0 1)
                --
                , equals (cons d y) m
                , poso y

                --
                -- r > 1
                --
                -- if r == 2 then c = 0 and z = '(1)
                -- if r == 3 then c = 1 and z = '(1)
                -- if r == 4 then c = 0 and z = '(0 1)
                --
                , equals (cons c z) r
                , poso z

                --
                -- b + a + d = c + 2e
                --
                , fullAddero b a d c e

                --
                -- e + x + y = z
                --
                , addero e x y z
                ]
        )


pluso : Value a -> Value a -> Value a -> Goal a
pluso =
    addero zero


minuso : Value a -> Value a -> Value a -> Goal a
minuso n m k =
    --
    -- n - m = k
    -- => n = m + k
    --
    pluso m k n


lengtho : Value a -> Value a -> Goal a
lengtho l n =
    conde
        [ [ nullo l, equals numZero n ]
        , [ fresh2
                (\d res ->
                    conj
                        [ cdro l d
                        , pluso numOne res n
                        , lazy (\_ -> lengtho d res)
                        ]
                )
          ]
        ]
