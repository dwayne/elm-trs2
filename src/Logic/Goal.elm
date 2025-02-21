module Logic.Goal exposing
    ( Goal, succeed, fail, equals, never, disj2, always, conj2
    , disj, conj, conde, lazy
    , callFresh, fresh, fresh2, fresh3, fresh4, fresh5, fresh6, fresh7, fresh8
    , ifte, once, conda, condu
    , Length(..), runS, run, run2, run3, run4, run5, run6, run7, run8
    , State, initState, toString
    )

{-| Goals.

@docs Goal, succeed, fail, equals, never, disj2, always, conj2
@docs disj, conj, conde, lazy
@docs callFresh, fresh, fresh2, fresh3, fresh4, fresh5, fresh6, fresh7, fresh8
@docs ifte, once, conda, condu
@docs Length, runS, run, run2, run3, run4, run5, run6, run7, run8
@docs State, initState, toString

-}

import Logic.Stream as Stream exposing (Stream)
import Logic.Substitution as Substitution exposing (Substitution)
import Logic.Value as Value exposing (Value(..))
import Logic.Variable exposing (Id, Variable)



--
-- # Base API
--
-- Goal, succeed, fail
-- equals
-- never, disj2, always, conj2
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


{-| -}
type Goal a
    = Goal (State a -> Stream (State a))


{-| -}
type alias State a =
    { substitution : Substitution a
    , nextId : Id
    }


{-| -}
initState : State a
initState =
    { substitution = [], nextId = startId }



-- RELATIONAL


{-| -}
succeed : Goal a
succeed =
    Goal Stream.singleton


{-| -}
fail : Goal a
fail =
    Goal (\_ -> Stream.Empty)


{-| -}
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


{-| -}
never : Goal a
never =
    Goal
        (\state ->
            Stream.Suspend (\_ -> apply never state)
        )


{-| -}
disj2 : Goal a -> Goal a -> Goal a
disj2 (Goal g1) (Goal g2) =
    Goal
        (\state ->
            Stream.append (g1 state) (g2 state)
        )


{-| -}
always : Goal a
always =
    Goal
        (\state ->
            Stream.Suspend (\_ -> apply (disj2 succeed always) state)
        )


{-| -}
disj : List (Goal a) -> Goal a
disj body =
    case body of
        [] ->
            fail

        [ g ] ->
            g

        g :: gs ->
            disj2 g (disj gs)


{-| -}
conj2 : Goal a -> Goal a -> Goal a
conj2 (Goal g1) (Goal g2) =
    Goal
        (\state ->
            Stream.appendMap g2 (g1 state)
        )


{-| -}
conj : List (Goal a) -> Goal a
conj body =
    case body of
        [] ->
            succeed

        [ g ] ->
            g

        g :: gs ->
            conj2 g (conj gs)


{-| -}
conde : List (List (Goal a)) -> Goal a
conde =
    disj << List.map conj


{-| -}
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


{-| -}
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


{-| -}
fresh : (Value a -> Goal a) -> Goal a
fresh =
    callFresh internalName


{-| -}
fresh2 : (Value a -> Value a -> Goal a) -> Goal a
fresh2 f2 =
    fresh (fresh << f2)


{-| -}
fresh3 : (Value a -> Value a -> Value a -> Goal a) -> Goal a
fresh3 f3 =
    fresh (fresh2 << f3)


{-| -}
fresh4 : (Value a -> Value a -> Value a -> Value a -> Goal a) -> Goal a
fresh4 f4 =
    fresh (fresh3 << f4)


{-| -}
fresh5 : (Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> Goal a
fresh5 f5 =
    fresh (fresh4 << f5)


{-| -}
fresh6 : (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> Goal a
fresh6 f6 =
    fresh (fresh5 << f6)


{-| -}
fresh7 : (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> Goal a
fresh7 f7 =
    fresh (fresh6 << f7)


{-| -}
fresh8 : (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> Goal a
fresh8 f8 =
    fresh (fresh7 << f8)



-- NON-RELATIONAL


{-| -}
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


{-| -}
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


{-| -}
conda : List (List (Goal a)) -> Goal a
conda arms =
    case arms of
        [ condition :: body ] ->
            conj (condition :: body)

        (condition :: body) :: restArms ->
            ifte condition (conj body) (conda restArms)

        _ ->
            fail


{-| -}
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


{-| -}
type Length
    = AtMost Int
    | Unbounded


{-| -}
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


{-| -}
run : Length -> (Value a -> Goal a) -> List (Value a)
run length f =
    runS length (f q)
        |> List.map (Substitution.reify q)


{-| -}
run2 : Length -> (Value a -> Value a -> Goal a) -> List (Value a)
run2 length f2 =
    let
        xs =
            Pair x1 (Pair x2 Null)
    in
    runS length (conj2 (equals xs q) (f2 x1 x2))
        |> List.map (Substitution.reify q)


{-| -}
run3 : Length -> (Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run3 length f3 =
    let
        xs =
            Pair x1 (Pair x2 (Pair x3 Null))
    in
    runS length (conj2 (equals xs q) (f3 x1 x2 x3))
        |> List.map (Substitution.reify q)


{-| -}
run4 : Length -> (Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run4 length f4 =
    let
        xs =
            Pair x1 (Pair x2 (Pair x3 (Pair x4 Null)))
    in
    runS length (conj2 (equals xs q) (f4 x1 x2 x3 x4))
        |> List.map (Substitution.reify q)


{-| -}
run5 : Length -> (Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run5 length f5 =
    let
        xs =
            Pair x1 (Pair x2 (Pair x3 (Pair x4 (Pair x5 Null))))
    in
    runS length (conj2 (equals xs q) (f5 x1 x2 x3 x4 x5))
        |> List.map (Substitution.reify q)


{-| -}
run6 : Length -> (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run6 length f6 =
    let
        xs =
            Pair x1 (Pair x2 (Pair x3 (Pair x4 (Pair x5 (Pair x6 Null)))))
    in
    runS length (conj2 (equals xs q) (f6 x1 x2 x3 x4 x5 x6))
        |> List.map (Substitution.reify q)


{-| -}
run7 : Length -> (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run7 length f7 =
    let
        xs =
            Pair x1 (Pair x2 (Pair x3 (Pair x4 (Pair x5 (Pair x6 (Pair x7 Null))))))
    in
    runS length (conj2 (equals xs q) (f7 x1 x2 x3 x4 x5 x6 x7))
        |> List.map (Substitution.reify q)


{-| -}
run8 : Length -> (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run8 length f8 =
    let
        xs =
            Pair x1 (Pair x2 (Pair x3 (Pair x4 (Pair x5 (Pair x6 (Pair x7 (Pair x8 Null)))))))
    in
    runS length (conj2 (equals xs q) (f8 x1 x2 x3 x4 x5 x6 x7 x8))
        |> List.map (Substitution.reify q)



-- CONVERT


{-| -}
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
