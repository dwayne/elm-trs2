module Logic.Substitution exposing
    ( Substitution
    , extend
    , occurs
    , reifiedNameSubstitution
    , reify
    , toString
    , unify
    , walk
    , walkAll
    )

import Logic.Value as Value exposing (Value(..))
import Logic.Variable as Variable exposing (Variable)


type alias Substitution a =
    List ( Variable, Value a )



-- UNIFICATION


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


occurs : Variable -> Value a -> Substitution a -> Bool
occurs x val sub =
    case walk val sub of
        Var y ->
            y == x

        Pair left right ->
            occurs x left sub || occurs x right sub

        _ ->
            False


extend : Variable -> Value a -> Substitution a -> Maybe (Substitution a)
extend var val sub =
    if occurs var val sub then
        -- Cycles aren't allowed
        Nothing

    else
        Just (( var, val ) :: sub)


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



-- REIFICATION


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


reifiedNameSubstitution : Value a -> Substitution a
reifiedNameSubstitution val =
    reifiedNameSubstitutionHelper val []


reifiedNameSubstitutionHelper : Value a -> Substitution a -> Substitution a
reifiedNameSubstitutionHelper val rnSub =
    --
    -- rnSub stands for reified-name substitution.
    --
    -- A reified-name substitution is a substitution in which each fresh variable
    -- is mapped to a ReifiedVar i for some i >= 0.
    --
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
