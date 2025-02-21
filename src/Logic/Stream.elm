module Logic.Stream exposing
    ( Stream(..)
    , singleton
    , append, appendMap
    , take, toList
    )

{-| A combination of a list and a lazy list.

@docs Stream


# Create

@docs singleton


# Combine

@docs append, appendMap


# Convert

@docs take, toList

-}


{-| If you remove the `Suspend` constructor then you have a list.

    type List a
        = Empty
        | Cons a (List a)

If you combine the `Cons` and `Suspend` constructors then you have a lazy list.

    type LazyList a
        = Empty
        | Cons a (() -> LazyList a)

Normal list streams:

    Empty

    Cons 3 Empty

    Cons 2 (Cons 3 Empty)

    Cons 1 (Cons 2 (Cons 3 Empty))

Partial lazy list streams:

    Suspend (\_ -> Cons 1 (Cons 2 (Cons 3 Empty)))

    Cons 1 (Cons 2 (Suspend (\_ -> Cons 3 (Cons 4 (Suspend (\_ -> Cons 5 Empty))))))

A full lazy list stream:

    Suspend (\_ -> Cons 1 (Suspend (\_ -> Cons 2 (Suspend (\_ -> Cons 3 (Suspend (\_ -> Empty)))))))

-}
type Stream a
    = Empty
    | Cons a (Stream a)
    | Suspend (() -> Stream a)



-- CREATE


{-| Create a one element stream.
-}
singleton : a -> Stream a
singleton x =
    Cons x Empty



-- COMBINE


{-| Put two streams together into one stream such that you will be able to eventually access any element from either stream,
regardless if the first stream is infinite, the second stream is infinite, or both streams are infinite.

    evensLessThan10 =
        Cons 0 (Cons 2 (Cons 4 (Cons 6 (Cons 8 Empty))))

    oddsLessThan10 =
        Cons 1 (Cons 3 (Cons 5 (Cons 7 (Cons 9 Empty))))

    take 10 (append evensLessThan10 oddsLessThan10) == [ 0, 2, 4, 6, 8, 1, 3, 5, 7, 9 ]

    arithmeticProgression a d =
        Cons a <| Suspend (\_ -> arithmeticProgression (a + d) d)

    evens =
        arithmeticProgression 0 2

    odds =
        arithmeticProgression 1 2

    naturals =
        append evens odds

    take 10 (append evensLessThan10 odds) == [ 0, 2, 4, 6, 8, 1, 3, 5, 7, 9 ]

    take 10 (append evens oddsLessThan10) == [ 0, 1, 3, 5, 7, 9, 2, 4, 6, 8 ]

    take 10 naturals == [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]

**N.B.** _In the last example, the two infinite streams are completely interleaved. `append` is the basis upon which
it is said that [miniKanren](https://minikanren.org/) uses a **complete interleaving search**._

-}
append : Stream a -> Stream a -> Stream a
append s t =
    case s of
        Empty ->
            t

        Cons head tail ->
            Cons head (append tail t)

        Suspend f ->
            Suspend (\_ -> append t (f ()))


{-| Map a function over a stream and [`append`](#append) the resulting streams.

    repeat n =
        Cons n <| Suspend (\_ -> repeat n)

    take 100 (appendMap repeat naturals) == [ 0, 0, 1, 0, 0, 1, 0, 2, 0, 1, 0, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0 ]

    type alias Occurrences =
        Int

    statistics : List ( Int, Occurrences )
    statistics =
        [ ( 0, 53 )
        , ( 1, 26 )
        , ( 2, 12 )
        , ( 3, 6 )
        , ( 4, 2 )
        , ( 5, 1 )
        ]

**N.B.** _It takes a while to get to each number but each number will eventually be reached, thanks to how [`append`](#append) works._

-}
appendMap : (a -> Stream b) -> Stream a -> Stream b
appendMap g s =
    case s of
        Empty ->
            Empty

        Cons head tail ->
            append
                (g head)
                (appendMap g tail)

        Suspend f ->
            Suspend (\_ -> appendMap g (f ()))



-- CONVERT


{-| Take the first `n` elements of a stream and put them into a list.
-}
take : Int -> Stream a -> List a
take n s =
    if n > 0 then
        case s of
            Empty ->
                []

            Cons head tail ->
                head :: take (n - 1) tail

            Suspend f ->
                take n (f ())

    else
        []


{-| Create a list of elements from a stream.

    toList evensLessThan10 == [ 0, 2, 4, 6, 8 ]

**N.B.** _It doesn't handle infinite streams. If you know your stream is infinite, use [`take`](#take) instead._

-}
toList : Stream a -> List a
toList s =
    case s of
        Empty ->
            []

        Cons head tail ->
            head :: toList tail

        Suspend f ->
            toList (f ())
