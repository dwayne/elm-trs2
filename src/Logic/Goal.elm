module Logic.Goal exposing
    ( Goal
    , succeed, fail
    , always, never
    , equals, disj2, disj, conj2, conj, conde
    , lazy
    , callFresh, fresh, fresh2, fresh3, fresh4, fresh5, fresh6, fresh7, fresh8
    , ifte, once, conda, condu
    , Length(..), runS, run, run2, run3, run4, run5, run6, run7, run8
    , toString
    )

{-| A low-level API for constructing, combining, and running goals.


# Goal

In general, a **goal** is a function that takes a substitution and, if it returns,
produces a stream of substitutions. Under the hood, there's slightly more to it.

@docs Goal
@docs succeed, fail
@docs always, never


# Relational operators

@docs equals, disj2, disj, conj2, conj, conde


# Define

@docs lazy


# Lexically-scoped logic variables

[`callFresh`](#callFresh) and the [`fresh`](#fresh) family of functions create lexically-scoped logic variables.

@docs callFresh, fresh, fresh2, fresh3, fresh4, fresh5, fresh6, fresh7, fresh8


# Non-relational operators

**BEWARE:** These operators may not work correctly for every goal ordering of a program.

[`ifte`](#ifte) and [`once`](#once) are used to implement [`conda`](#conda) and [`condu`](#condu).

`conda` and `condu` are used to prune a program's search tree, and can be used
in place of [Prolog's cut](https://en.wikipedia.org/wiki/Cut_%28logic_programming%29) (`!`).

@docs ifte, once, conda, condu


# Run

@docs Length, runS, run, run2, run3, run4, run5, run6, run7, run8


# Convert

@docs toString

-}

import Logic.Stream as Stream exposing (Stream)
import Logic.Substitution as Substitution exposing (Substitution)
import Logic.Value as Value exposing (Value(..))
import Logic.Variable exposing (Id, Variable)



--
-- # Base API
--
-- Goal
-- succeed, fail
-- equals
-- disj2, conj2
-- always, never
-- callFresh
-- ifte, once
-- Length, runS
--
-- # High-level API
--
-- disj, conj, conde
-- fresh
-- conda, condu
-- lazy
-- run
--
-- # Miscellaneous
--
-- State, initState
-- toString
--


{-| It either **succeeds**, **fails**, or **has no value**.

A goal that **succeeds** returns a stream of one or more substitutions.

A goal that **fails** returns an empty stream.

A goal that **has no value** never returns.

-}
type Goal a
    = Goal (State a -> Stream (State a))


type alias State a =
    { substitution : Substitution a
    , nextId : Id
    }


initState : State a
initState =
    { substitution = [], nextId = startId }



-- RELATIONAL


{-| A goal that succeeds only once, i.e. it produces a singleton stream containing
the current substitution. Contrast that with [`always`](#always).

**N.B.** _The **current substitution** is the substitution that's passed to the goal._

-}
succeed : Goal a
succeed =
    Goal Stream.singleton


{-| A goal that fails.
-}
fail : Goal a
fail =
    Goal (\_ -> Stream.Empty)


{-| Try to unify two values. It succeeds if unification is possible, and fails otherwise.
-}
equals : Value a -> Value a -> Goal a
equals u v =
    Goal
        (\state ->
            case Substitution.unify u v state.substitution of
                Just substitution ->
                    Stream.singleton { state | substitution = substitution }

                Nothing ->
                    Stream.Empty
        )


{-| The **disjunction** of two goals. It succeeds if and only if either of the goals succeed.
-}
disj2 : Goal a -> Goal a -> Goal a
disj2 (Goal g1) (Goal g2) =
    Goal
        (\state ->
            Stream.append (g1 state) (g2 state)
        )


{-| The disjunction of zero or more goals. It succeeds if and only if there is at least one goal that succeeds.
-}
disj : List (Goal a) -> Goal a
disj body =
    case body of
        [] ->
            fail

        [ g ] ->
            g

        g :: gs ->
            disj2 g (disj gs)


{-| The **conjunction** of two goals. It succeeds if and only if both goals succeed.
-}
conj2 : Goal a -> Goal a -> Goal a
conj2 (Goal g1) (Goal g2) =
    Goal
        (\state ->
            Stream.appendMap g2 (g1 state)
        )


{-| The conjunction of zero or more goals. It succeeds if and only if all the goals succeed.
-}
conj : List (Goal a) -> Goal a
conj body =
    case body of
        [] ->
            succeed

        [ g ] ->
            g

        g :: gs ->
            conj2 g (conj gs)


{-| The disjunction of conjunctions.

    conde
        [ [ g11, ..., g1N1 ]
        , [ g21, ..., g2N2 ]
        , ...
        , [ gM1, ..., gMNM ]
        ]

is equivalent to

    disj
        [ conj [ g11, ..., g1N1 ]
        , conj [ g21, ..., g2N2 ]
        , ...
        , conj [ gM1, ..., gMNM ]
        ]

where `M >= 0` and `Ni >= 0` for all `1 <= i <= M`.

If `M = 0` then we have `conde []`.

If `Ni = 0` for some `i` then the `(i-1)`th element of the list given to `conde` is the empty list.

-}
conde : List (List (Goal a)) -> Goal a
conde =
    disj << List.map conj


{-| A goal that succeeds an unbounded number of times.

Contrast that with [`succeed`](#succeed), which succeeds only once.

-}
always : Goal a
always =
    Goal
        (\state ->
            Stream.Suspend (\_ -> apply (disj2 succeed always) state)
        )


{-| A goal that neither succeeds nor fails.
-}
never : Goal a
never =
    Goal
        (\state ->
            Stream.Suspend (\_ -> apply never state)
        )


{-| Useful for defining recursive goals.

For e.g. here's a possible definition of [`always`](#always).

    always : Goal a
    always =
        disj2 succeed (lazy (\_ -> always))

**Challenge:** Define [`never`](#never) using `lazy`.

-}
lazy : (() -> Goal a) -> Goal a
lazy f =
    Goal
        (\state ->
            Stream.Suspend
                (\_ ->
                    apply (f ()) state
                )
        )


apply : Goal a -> State a -> Stream (State a)
apply (Goal g) state =
    g state



-- FRESH


{-| Create a lexically-scoped logic variable, with a given name.

    callFresh "fruit"
        (\fruit ->
            equals (Const "plum") fruit
        )

**N.B.** _You only have access to the variable within the function you provide._

-}
callFresh : String -> (Value a -> Goal a) -> Goal a
callFresh name f =
    Goal
        (\state ->
            let
                var =
                    Var (Variable name state.nextId)
            in
            apply (f var) { state | nextId = state.nextId + 1 }
        )


{-| Create a lexically-scoped logic variable.

    fresh
        (\fruit ->
            equals (Const "plum") fruit
        )

**N.B.** _It uses [`callFresh`](#callFresh) under the hood but
you have no control over the name of the variable. The name is
only useful for debugging purposes anyway._

-}
fresh : (Value a -> Goal a) -> Goal a
fresh =
    callFresh internalName


{-| Create two lexicaly-scoped logic variables.
-}
fresh2 : (Value a -> Value a -> Goal a) -> Goal a
fresh2 f2 =
    fresh (fresh << f2)


{-| Create three lexically-scoped logic variables.
-}
fresh3 : (Value a -> Value a -> Value a -> Goal a) -> Goal a
fresh3 f3 =
    fresh (fresh2 << f3)


{-| Create four lexically-scoped logic variables.
-}
fresh4 : (Value a -> Value a -> Value a -> Value a -> Goal a) -> Goal a
fresh4 f4 =
    fresh (fresh3 << f4)


{-| Create five lexically-scoped logic variables.
-}
fresh5 : (Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> Goal a
fresh5 f5 =
    fresh (fresh4 << f5)


{-| Create six lexically-scoped logic variables.
-}
fresh6 : (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> Goal a
fresh6 f6 =
    fresh (fresh5 << f6)


{-| Create seven lexically-scoped logic variables.
-}
fresh7 : (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> Goal a
fresh7 f7 =
    fresh (fresh6 << f7)


{-| Create eight lexically-scoped logic variables.
-}
fresh8 : (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> Goal a
fresh8 f8 =
    fresh (fresh7 << f8)



-- NON-RELATIONAL


{-| `ifte` stands for **if-then-else**.

`ifte g1 g2 g3` is a goal that applies `g1` to the current substitution. If `g1` succeeds, it applies `g2` to each of the
substitutions produced by `g1`. If `g1` fails, it applies `g3` to the current substitution. Otherwise, if `g1` has no value,
the goal, `ifte g1 g2 g3`, also has no value.

-}
ifte : Goal a -> Goal a -> Goal a -> Goal a
ifte (Goal g1) (Goal g2) (Goal g3) =
    Goal
        (\state ->
            let
                loop s =
                    case s of
                        Stream.Empty ->
                            g3 state

                        Stream.Cons _ _ ->
                            Stream.appendMap g2 s

                        Stream.Suspend f ->
                            Stream.Suspend (\_ -> loop (f ()))
            in
            loop (g1 state)
        )


{-| `once g` is a goal that applies `g` to the current substitution. If `g` succeeds, the first substitution produced by `g`
is used to create a singleton stream which becomes the stream produced by `once g`. The remaining substitutions produced by `g`
are dropped, i.e. we cutoff all other possibilities. If `g` fails, `once g` fails. And, if `g` has no value, `once g` also
has no value.
-}
once : Goal a -> Goal a
once (Goal g) =
    Goal
        (\state ->
            let
                loop s =
                    case s of
                        Stream.Empty ->
                            Stream.Empty

                        Stream.Cons car _ ->
                            Stream.singleton car

                        Stream.Suspend f ->
                            Stream.Suspend (\_ -> loop (f ()))
            in
            loop (g state)
        )


{-|

    conda
        [ [ g11, ..., g1N1 ]
        , [ g21, ..., g2N2 ]
        , ...
        , [ gM1, ..., gMNM ]
        ]

is equivalent to

    ifte
        g11
        (conj [ g12, ..., g1N1 ])
        (conda
            [ [ g21, ..., g2N2 ]
            , ...
            , [ gM1, ..., gMNM ]
            ]
        )

where `M >= 1`, `N1 >= 1`, and `Ni >= 0` for all `2 <= i <= M`.

For the case when `M = 0` or when `M >= 1` and `N1 = 0`, `conda` fails.

**N.B.** _`conda` corresponds to Prolog's [soft-cut](https://www.swi-prolog.org/pldoc/doc%5Ffor?object=%28%2A-%3E%29/2)._

-}
conda : List (List (Goal a)) -> Goal a
conda arms =
    case arms of
        [ condition :: body ] ->
            conj (condition :: body)

        (condition :: body) :: restArms ->
            ifte condition (conj body) (conda restArms)

        _ ->
            fail


{-|

    condu
        [ [ g11, g12, ..., g1N1 ]
        , [ g21, g22, ..., g2N2 ]
        , ...
        , [ gM1, gM2, ..., gMNM ]
        ]

is equivalent to

    conda
        [ [ once g11, g12, ..., g1N1 ]
        , [ once g21, g22, ..., g2N2 ]
        , ...
        , [ once gM1, gM2, ..., gMNM ]
        ]

where `M >= 0` and `Ni >= 0` for all `1 <= i <= M`.

If a `condu` line is empty, the equivalent `conda` line is also empty.

**N.B.** _`condu` corresponds to Mercury's [committed choice](https://www.mercurylang.org/information/doc-release/mercury%5Fref/Committed-choice-nondeterminism.html)._

-}
condu : List (List (Goal a)) -> Goal a
condu arms =
    let
        onceArms =
            --
            -- [ [ a1, a2, a3, ... ]
            -- , [ b1, b2, ... ]
            -- , [ c1, ... ]
            -- , []
            -- , ...
            -- ]
            --
            -- becomes
            --
            -- [ [ once a1, a2, a3, ... ]
            -- , [ once b1, b2, ... ]
            -- , [ once c1, ... ]
            -- , []
            -- , ...
            -- ]
            --
            List.map
                (\arm ->
                    case arm of
                        condition :: body ->
                            once condition :: body

                        [] ->
                            []
                )
                arms
    in
    conda onceArms



-- RUN


{-| Used by the various run functions to determine the number of substitutions
to take from a stream of substitutions.

`Unbounded` means to take the entire stream of substitutions, which may be finite or infinite.

-}
type Length
    = AtMost Int
    | Unbounded


{-| Apply a goal to the empty substitution and return the list of substitutions produced by that goal.

The length of the list is determined by the argument of type [`Length`](#Length).

-}
runS : Length -> Goal a -> List (Substitution a)
runS length (Goal g) =
    let
        takeOrToList =
            case length of
                AtMost n ->
                    Stream.take n

                Unbounded ->
                    Stream.toList
    in
    initState
        |> g
        |> takeOrToList
        |> List.map .substitution


{-| It's similar to [`runS`](#runS), except that for each substitution that would be in the list returned by `runS`,
it reifies each substitution with a fresh variable.
-}
run : Length -> (Value a -> Goal a) -> List (Value a)
run length f =
    runS length (f q)
        |> List.map (Substitution.reify q)


{-| It's similar to [`run`](#run), except that the value used for reification is a list containing two fresh variables.
-}
run2 : Length -> (Value a -> Value a -> Goal a) -> List (Value a)
run2 length f2 =
    let
        xs =
            Pair x1 (Pair x2 Null)
    in
    runS length (conj2 (equals xs q) (f2 x1 x2))
        |> List.map (Substitution.reify q)


{-| It's similar to [`run`](#run), except that the value used for reification is a list containing three fresh variables.
-}
run3 : Length -> (Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run3 length f3 =
    let
        xs =
            Pair x1 (Pair x2 (Pair x3 Null))
    in
    runS length (conj2 (equals xs q) (f3 x1 x2 x3))
        |> List.map (Substitution.reify q)


{-| It's similar to [`run`](#run), except that the value used for reification is a list containing four fresh variables.
-}
run4 : Length -> (Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run4 length f4 =
    let
        xs =
            Pair x1 (Pair x2 (Pair x3 (Pair x4 Null)))
    in
    runS length (conj2 (equals xs q) (f4 x1 x2 x3 x4))
        |> List.map (Substitution.reify q)


{-| It's similar to [`run`](#run), except that the value used for reification is a list containing five fresh variables.
-}
run5 : Length -> (Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run5 length f5 =
    let
        xs =
            Pair x1 (Pair x2 (Pair x3 (Pair x4 (Pair x5 Null))))
    in
    runS length (conj2 (equals xs q) (f5 x1 x2 x3 x4 x5))
        |> List.map (Substitution.reify q)


{-| It's similar to [`run`](#run), except that the value used for reification is a list containing six fresh variables.
-}
run6 : Length -> (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run6 length f6 =
    let
        xs =
            Pair x1 (Pair x2 (Pair x3 (Pair x4 (Pair x5 (Pair x6 Null)))))
    in
    runS length (conj2 (equals xs q) (f6 x1 x2 x3 x4 x5 x6))
        |> List.map (Substitution.reify q)


{-| It's similar to [`run`](#run), except that the value used for reification is a list containing seven fresh variables.
-}
run7 : Length -> (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run7 length f7 =
    let
        xs =
            Pair x1 (Pair x2 (Pair x3 (Pair x4 (Pair x5 (Pair x6 (Pair x7 Null))))))
    in
    runS length (conj2 (equals xs q) (f7 x1 x2 x3 x4 x5 x6 x7))
        |> List.map (Substitution.reify q)


{-| It's similar to [`run`](#run), except that the value used for reification is a list containing eight fresh variables.
-}
run8 : Length -> (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run8 length f8 =
    let
        xs =
            Pair x1 (Pair x2 (Pair x3 (Pair x4 (Pair x5 (Pair x6 (Pair x7 (Pair x8 Null)))))))
    in
    runS length (conj2 (equals xs q) (f8 x1 x2 x3 x4 x5 x6 x7 x8))
        |> List.map (Substitution.reify q)



-- CONVERT


{-| Convert a list of values to a representation that is useful for debugging and testing.
-}
toString : (a -> String) -> List (Value a) -> String
toString stringify vs =
    "(" ++ String.join " " (List.map (Value.toString stringify) vs) ++ ")"



-- HELPERS


q =
    Var (Variable internalName 0)


x1 =
    Var (Variable internalName -1)


x2 =
    Var (Variable internalName -2)


x3 =
    Var (Variable internalName -3)


x4 =
    Var (Variable internalName -4)


x5 =
    Var (Variable internalName -5)


x6 =
    Var (Variable internalName -6)


x7 =
    Var (Variable internalName -7)


x8 =
    Var (Variable internalName -8)


internalName =
    --
    -- Reserve the empty string as the internal name for variables.
    --
    ""


startId =
    --
    -- The identifiers from -8 to 0 are already used up.
    --
    1
