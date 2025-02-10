module Logic.Stream exposing
    ( Stream(..)
    , append
    , appendMap
    , singleton
    , take
    , toList
    )


type Stream a
    = Empty
    | Cons a (Stream a)
    | Suspension (() -> Stream a)



-- CREATE


singleton : a -> Stream a
singleton x =
    Cons x Empty



-- COMBINE


append : Stream a -> Stream a -> Stream a
append s t =
    case s of
        Empty ->
            t

        Cons head tail ->
            Cons head (append tail t)

        Suspension f ->
            Suspension (\_ -> append t (f ()))


appendMap : (a -> Stream b) -> Stream a -> Stream b
appendMap g s =
    case s of
        Empty ->
            Empty

        Cons head tail ->
            append
                (g head)
                (appendMap g tail)

        Suspension f ->
            Suspension (\_ -> appendMap g (f ()))



-- CONVERT


take : Int -> Stream a -> List a
take n s =
    if n > 0 then
        case s of
            Empty ->
                []

            Cons head tail ->
                head :: take (n - 1) tail

            Suspension f ->
                take n (f ())

    else
        []


toList : Stream a -> List a
toList s =
    case s of
        Empty ->
            []

        Cons head tail ->
            head :: toList tail

        Suspension f ->
            toList (f ())
