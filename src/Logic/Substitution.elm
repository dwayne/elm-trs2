module Logic.Substitution exposing
    ( Substitution
    , walk, occurs, extend, unify
    , walkAll, reifiedNameSubstitution, reify
    , toString
    )

{-| A substitution is a mapping between variables and values.

We'll use the following variables and values throughout the examples in this module:

    import Logic.Value exposing (..)
    import Logic.Variable exposing (..)

    u =
        Variable "u" 0

    v =
        Variable "v" 1

    w =
        Variable "w" 2

    x =
        Variable "x" 3

    y =
        Variable "y" 4

    z =
        Variable "z" 5

    vu =
        Var u

    vv =
        Var v

    vw =
        Var w

    vx =
        Var x

    vy =
        Var y

    vz =
        Var z

    five =
        Const 5

    a =
        Const "a"

    b =
        Const "b"

    c =
        Const "c"

    d =
        Const "d"

@docs Substitution


# Unify

The **unification** of two values is the process of finding zero or more associations
that would make the two values equal.

[`walk`](#walk), [`occurs`](#occurs), and [`extend`](#extend) are used to implement [`unify`](#unify).

@docs walk, occurs, extend, unify


# Reify

Reification is the process of removing all logic variables from a value.

[`walkAll`](#walkAll) and [`reifiedNameSubstitution`](#reifiedNameSubstitution) are used to implement [`reify`](#reify).

@docs walkAll, reifiedNameSubstitution, reify


# Convert

@docs toString

-}

import Logic.Value as Value exposing (Value(..))
import Logic.Variable as Variable exposing (Variable)


{-| A variable is allowed to be associated with another variable. If a variable that appears on
the left-hand side of an association never appears on the right-hand side of an association,
the substitution is called an **idempotent substitution**. Otherwise, it is called a
**triangular substitution**.

The representation that's used allows for triangular substitutions.

For e.g.

    [(y, five), (x, vy)]
    -- is a triangular substitution

    -- whereas,
    [(y, five), (x, five)]
    -- is an idempotent substitution

-}
type alias Substitution a =
    List ( Variable, Value a )



-- UNIFY


{-| Variable lookup.

    sub1 = [ ( z, a ), ( x, vw ), ( y, vz ) ]

    walk vz sub1 == a

    walk vy sub1 == a

    walk vx sub1 == vw

    sub2 = [ ( x, vy ), ( v, vx ), ( w, vx ) ]

    walk vx sub2 == vy

    walk vv sub2 == vy

    walk vw sub2 == vy

If the resulting value is a variable then the variable is fresh.

A **fresh** variable is one that could be associated with any value.

-}
walk : Value a -> Substitution a -> Value a
walk val sub =
    case val of
        Var var ->
            case find var sub of
                Just foundVal ->
                    walk foundVal sub

                Nothing ->
                    val

        _ ->
            val


{-| Check whether or not the variable occurs within the value.

    occurs u vv [] == False

    occurs u vu [] == True

    occurs x vy [ ( w, a ), ( z, vx ), ( y, vz ) ] == True

    occurs x (Pair a (Pair vy Null)) [ ( z, vw ), ( y, Pair vx Null ) ] == True

-}
occurs : Variable -> Value a -> Substitution a -> Bool
occurs x val sub =
    case walk val sub of
        Var y ->
            y == x

        Pair left right ->
            occurs x left sub || occurs x right sub

        _ ->
            False


{-| Extend a substitution with a new association between a variable and a value.

    extend u a [] == Just [ ( u, a ) ]

    extend u a [ ( v, b ) ] == Just [ ( u, a ), ( v, b ) ]

Cycles aren't allowed.

    extend x vy [ ( w, a ), ( z, vx ), ( y, vz ) ] == Nothing

-}
extend : Variable -> Value a -> Substitution a -> Maybe (Substitution a)
extend var val sub =
    if occurs var val sub then
        -- Cycles aren't allowed
        Nothing

    else
        Just (( var, val ) :: sub)


{-| Unifies two values with respect to a substitution.

It returns a, potentially extended, substitution if unification succeeds.
Otherwise, it returns `Nothing` if unification fails or would introduce
a circularity.

    unify vu a [] == Just [ ( u, a ) ]

    unify a vu [] == Just [ ( u, a ) ]

    unify vu vv [] == Just [ ( u, vv ) ]

    unify vv vu [] == Just [ ( v, vu ) ]

    unify vu vv [ ( w, b ), ( v, vw ) ] == Just [ ( u, b ), ( w, b ), ( v, vw ) ]

    unify vu a [ ( u, a ) ] == Just [ ( u, a ) ]

    unify vu b [ ( u, a ) ] == Nothing

    value1 =
        -- `(,x (,y))
        Pair
            vx
            (Pair (Pair vy Null) Null)

    value2 =
        -- `((a b) (c))
        Pair
            (Pair a (Pair b Null))
            (Pair (Pair c Null) Null)

    expectedSubstitution =
        -- `((,y . c) (,x . (a b)))
        [ ( y, c )
        , ( x, Pair a (Pair b Null) )
        ]

    unify value1 value2 [] == Just expectedSubstitution

-}
unify : Value a -> Value a -> Substitution a -> Maybe (Substitution a)
unify u v sub =
    let
        walkedU =
            walk u sub

        walkedV =
            walk v sub
    in
    if walkedU == walkedV then
        Just sub

    else
        case ( walkedU, walkedV ) of
            ( Var x, _ ) ->
                -- u is fresh
                extend x walkedV sub

            ( _, Var y ) ->
                -- v is fresh
                extend y walkedU sub

            ( Pair leftU rightU, Pair leftV rightV ) ->
                sub
                    |> unify leftU leftV
                    |> Maybe.andThen (unify rightU rightV)

            _ ->
                Nothing



-- REIFY


{-| Walk all the variables contained within the value.

    sub3 =
        [ ( x, b )
        , ( z, vy )
        , ( w, Pair vx (Pair d (Pair vz Null)) )
        ]

    walk vw sub3 == Pair vx (Pair d (Pair vz Null))

However,

    walkAll vw sub3 == Pair b (Pair d (Pair vy Null))

If after walking all the variables contained within the value the resulting
value still contains variables then those variables are fresh.

-}
walkAll : Value a -> Substitution a -> Value a
walkAll val sub =
    let
        walkedVal =
            walk val sub
    in
    case walkedVal of
        Pair left right ->
            Pair
                (walkAll left sub)
                (walkAll right sub)

        _ ->
            walkedVal


{-| A **reified-name** substitution is a substitution in which each fresh variable
is mapped to a `ReifiedVar i` for some `i >= 0`.

    reifiedNameSubstitution (Pair vx (Pair vy (Pair vx vz))) == [ ( z, ReifiedVar 2 ), ( y, ReifiedVar 1 ), ( x, ReifiedVar 0 ) ]

-}
reifiedNameSubstitution : Value a -> Substitution a
reifiedNameSubstitution val =
    reifiedNameSubstitutionHelper val []


reifiedNameSubstitutionHelper : Value a -> Substitution a -> Substitution a
reifiedNameSubstitutionHelper val rnSub =
    case walk val rnSub of
        Var var ->
            -- var is fresh
            let
                n =
                    List.length rnSub
            in
            ( var, ReifiedVar n ) :: rnSub

        Pair left right ->
            rnSub
                |> reifiedNameSubstitutionHelper left
                |> reifiedNameSubstitutionHelper right

        _ ->
            rnSub


{-| Remove all logic variables from a value.

The fresh variables are replaced with reified variables.

    corn =
        Const "corn"

    ice =
        Const "ice"

    substitution =
        let
            a1 =
                -- `(,x . (,u ,w ,y ,z ((ice) ,z)))
                ( x, Pair vu (Pair vw (Pair vy (Pair vz (Pair (Pair (Pair ice Null) (Pair vz Null)) Null)))) )

            a2 =
                -- `(,y . corn)
                ( y, corn )

            a3 =
                -- `(,w . (,v ,u))
                ( w, Pair vv (Pair vu Null) )
        in
        [ a1, a2, a3 ]

    expectedValue =
        let
            u0 =
                ReifiedVar 0

            u1 =
                ReifiedVar 1

            u2 =
                ReifiedVar 2
        in
        -- (_0 (_1 _0) corn _2 ((ice) _2))
        Pair u0 (Pair (Pair u1 (Pair u0 Null)) (Pair corn (Pair u2 (Pair (Pair (Pair ice Null) (Pair u2 Null)) Null))))

    reify vx substitution == expectedValue

-}
reify : Value a -> Substitution a -> Value a
reify v sub =
    let
        wav =
            walkAll v sub

        rnSub =
            reifiedNameSubstitution wav
    in
    walkAll wav rnSub



-- CONVERT


{-| Convert to a representation that is useful for debugging and testing.

    toString identity [] == "()"

    toString identity [ ( u, vx ), ( v, a ), ( w, Pair b c ) ] == "((u.0 . x.3) (v.1 . a) (w.2 . (b . c)))"

-}
toString : (a -> String) -> Substitution a -> String
toString stringify sub =
    String.concat
        [ "("
        , String.join " " <|
            List.map
                (\( var, val ) ->
                    "(" ++ Variable.toString var ++ " . " ++ Value.toString stringify val ++ ")"
                )
                sub
        , ")"
        ]



-- HELPERS


find : a -> List ( a, b ) -> Maybe b
find needle haystack =
    case haystack of
        ( a, b ) :: rest ->
            if needle == a then
                Just b

            else
                find needle rest

        [] ->
            Nothing
