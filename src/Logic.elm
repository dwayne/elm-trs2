module Logic exposing
    ( Value, Constant(..)
    , int, float, bool, char, string, custom, toConstant
    , null, cons, list, dottedList, car, cdr, uncons
    , Goal
    , succeed, fail
    , always, never
    , equals, disj2, disj, conj2, conj, conde
    , lazy
    , fresh, fresh2, fresh3, fresh4, fresh5, fresh6, fresh7, fresh8
    , once, conda, condu
    , run, run2, run3, run4, run5, run6, run7, run8
    , runAtMost, run2AtMost, run3AtMost, run4AtMost, run5AtMost, run6AtMost, run7AtMost, run8AtMost
    , toString
    , Presenter(..), customToString
    )

{-| A high-level API that brings together the `Logic.*` submodules into a cohesive module.


# Value

@docs Value, Constant
@docs int, float, bool, char, string, custom, toConstant
@docs null, cons, list, dottedList, car, cdr, uncons


# Goal

@docs Goal
@docs succeed, fail
@docs always, never


# Relational operators

@docs equals, disj2, disj, conj2, conj, conde


# Define

@docs lazy


# Lexically-scoped logic variables

@docs fresh, fresh2, fresh3, fresh4, fresh5, fresh6, fresh7, fresh8


# Non-relational operators

See `Logic.Goal` for more details on these operators.

@docs once, conda, condu


# Run

See `Logic.Goal` for more details on these runners.

@docs run, run2, run3, run4, run5, run6, run7, run8
@docs runAtMost, run2AtMost, run3AtMost, run4AtMost, run5AtMost, run6AtMost, run7AtMost, run8AtMost


# Debug

@docs toString
@docs Presenter, customToString

-}

import Logic.Goal
import Logic.Value



-- VALUE


{-| A rich default set of values that can be associated with (logic) variables.
-}
type alias Value a =
    Logic.Value.Value (Constant a)


{-| Work with integers, floats, booleans, characters, and strings out of the box. There's
also the possibility to bring in your own custom types.
-}
type Constant a
    = Int Int
    | Float Float
    | Bool Bool
    | Char Char
    | String String
    | Custom a


{-| Create an integer value.
-}
int : Int -> Value a
int =
    Logic.Value.Const << Int


{-| Create a floating-point value.
-}
float : Float -> Value a
float =
    Logic.Value.Const << Float


{-| Create a boolean value.
-}
bool : Bool -> Value a
bool =
    Logic.Value.Const << Bool


{-| Create a character value.
-}
char : Char -> Value a
char =
    Logic.Value.Const << Char


{-| Create a string value.
-}
string : String -> Value a
string =
    Logic.Value.Const << String


{-| Create a value out of your custom type.
-}
custom : a -> Value a
custom =
    Logic.Value.Const << Custom


{-| Convert a value to a constant, if possible.

    toConstant (int 5) == Just (Int 5)

    toConstant null == Nothing

-}
toConstant : Value a -> Maybe (Constant a)
toConstant val =
    case val of
        Logic.Value.Const c ->
            Just c

        _ ->
            Nothing


{-| The empty list value.
-}
null : Value a
null =
    Logic.Value.Null


{-| Create a dotted pair of any two values.
-}
cons : Value a -> Value a -> Value a
cons =
    Logic.Value.Pair


{-| Create a proper list of zero or more values.

    list [] == null

    list [ int 1 ] == cons (int 1) null

    list [ int 1, bool True ] == cons (int 1) (cons (bool True) null)

    list [ int 1, bool True, char 'a' ] == cons (int 1) (cons (bool True) (cons (char 'a') null))

-}
list : List (Value a) -> Value a
list =
    List.foldr cons null


{-| Create an improper list of two or more values.

    dottedList (int 1) [] (bool True) == cons (int 1) (bool True)

    dottedList (int 1) [ bool True ] (char 'a') == cons (int 1) (cons (bool True) (char 'a'))

-}
dottedList : Value a -> List (Value a) -> Value a -> Value a
dottedList head tail last =
    --
    -- dottedList h [ t1, ..., tn ] l, where n >= 0
    --
    -- => cons h (cons t1 (cons ... (cons tn l)))
    --
    tail
        |> List.foldr cons last
        |> cons head


{-| Get the first value from a dotted pair, if possible.

    car (cons x y) == Just x

    car null == Nothing

    car (string "(cons x y)") == Nothing

-}
car : Value a -> Maybe (Value a)
car val =
    case val of
        Logic.Value.Pair left _ ->
            Just left

        _ ->
            Nothing


{-| Get the second value from a dotted pair, if possible.

    cdr (cons x y) == Just y

    cdr null == Nothing

    cdr (string "(cons x y)") == Nothing

-}
cdr : Value a -> Maybe (Value a)
cdr val =
    case val of
        Logic.Value.Pair _ right ->
            Just right

        _ ->
            Nothing


{-| Get the first and second value from a dotted pair, if possible.

    uncons (cons x y) == Just ( x, y )

    uncons null == Nothing

    uncons (string "(cons x y)") == Nothing

-}
uncons : Value a -> Maybe ( Value a, Value a )
uncons val =
    case val of
        Logic.Value.Pair left right ->
            Just ( left, right )

        _ ->
            Nothing



-- GOAL


{-| Imbue the underlying `Goal` type with a richer set of constants.

See `Logic.Goal` for more details.

-}
type alias Goal a =
    Logic.Goal.Goal (Constant a)



-- RELATIONAL


{-| A goal that succeeds.
-}
succeed : Goal a
succeed =
    Logic.Goal.succeed


{-| A goal that fails.
-}
fail : Goal a
fail =
    Logic.Goal.fail


{-| A goal that succeeds an unbounded number of times.
-}
always : Goal a
always =
    Logic.Goal.always


{-| A goal that neither succeeds nor fails.
-}
never : Goal a
never =
    Logic.Goal.never


{-| Try to unify two values. It succeeds if unification is possible, and fails otherwise.
-}
equals : Value a -> Value a -> Goal a
equals =
    Logic.Goal.equals


{-| The **disjunction** of two goals. It succeeds if and only if either of the goals succeed.
-}
disj2 : Goal a -> Goal a -> Goal a
disj2 =
    Logic.Goal.disj2


{-| The disjunction of zero or more goals. It succeeds if and only if there is at least one goal that succeeds.
-}
disj : List (Goal a) -> Goal a
disj =
    Logic.Goal.disj


{-| The **conjunction** of two goals. It succeeds if and only if both goals succeed.
-}
conj2 : Goal a -> Goal a -> Goal a
conj2 =
    Logic.Goal.conj2


{-| The conjunction of zero or more goals. It succeeds if and only if all the goals succeed.
-}
conj : List (Goal a) -> Goal a
conj =
    Logic.Goal.conj


{-| The disjunction of conjunctions.

See `Logic.Goal.conde` for more details.

-}
conde : List (List (Goal a)) -> Goal a
conde =
    Logic.Goal.conde



-- DEFINE


{-| Useful for defining recursive goals.
-}
lazy : (() -> Goal a) -> Goal a
lazy =
    Logic.Goal.lazy



-- FRESH


{-| Create a lexically-scoped logic variable.

    fresh
        (\fruit ->
            equals (Const "plum") fruit
        )

-}
fresh : (Value a -> Goal a) -> Goal a
fresh =
    Logic.Goal.fresh


{-| Create two lexicaly-scoped logic variables.
-}
fresh2 : (Value a -> Value a -> Goal a) -> Goal a
fresh2 =
    Logic.Goal.fresh2


{-| Create three lexicaly-scoped logic variables.
-}
fresh3 : (Value a -> Value a -> Value a -> Goal a) -> Goal a
fresh3 =
    Logic.Goal.fresh3


{-| Create four lexicaly-scoped logic variables.
-}
fresh4 : (Value a -> Value a -> Value a -> Value a -> Goal a) -> Goal a
fresh4 =
    Logic.Goal.fresh4


{-| Create five lexicaly-scoped logic variables.
-}
fresh5 : (Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> Goal a
fresh5 =
    Logic.Goal.fresh5


{-| Create six lexicaly-scoped logic variables.
-}
fresh6 : (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> Goal a
fresh6 =
    Logic.Goal.fresh6


{-| Create seven lexicaly-scoped logic variables.
-}
fresh7 : (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> Goal a
fresh7 =
    Logic.Goal.fresh7


{-| Create eight lexicaly-scoped logic variables.
-}
fresh8 : (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> Goal a
fresh8 =
    Logic.Goal.fresh8



-- NON-RELATIONAL


{-| -}
once : Goal a -> Goal a
once =
    Logic.Goal.once


{-| -}
conda : List (List (Goal a)) -> Goal a
conda =
    Logic.Goal.conda


{-| -}
condu : List (List (Goal a)) -> Goal a
condu =
    Logic.Goal.condu



-- RUN


{-| -}
run : (Value a -> Goal a) -> List (Value a)
run =
    Logic.Goal.run Logic.Goal.Unbounded


{-| -}
run2 : (Value a -> Value a -> Goal a) -> List (Value a)
run2 =
    Logic.Goal.run2 Logic.Goal.Unbounded


{-| -}
run3 : (Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run3 =
    Logic.Goal.run3 Logic.Goal.Unbounded


{-| -}
run4 : (Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run4 =
    Logic.Goal.run4 Logic.Goal.Unbounded


{-| -}
run5 : (Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run5 =
    Logic.Goal.run5 Logic.Goal.Unbounded


{-| -}
run6 : (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run6 =
    Logic.Goal.run6 Logic.Goal.Unbounded


{-| -}
run7 : (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run7 =
    Logic.Goal.run7 Logic.Goal.Unbounded


{-| -}
run8 : (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run8 =
    Logic.Goal.run8 Logic.Goal.Unbounded


{-| -}
runAtMost : Int -> (Value a -> Goal a) -> List (Value a)
runAtMost =
    Logic.Goal.run << Logic.Goal.AtMost


{-| -}
run2AtMost : Int -> (Value a -> Value a -> Goal a) -> List (Value a)
run2AtMost =
    Logic.Goal.run2 << Logic.Goal.AtMost


{-| -}
run3AtMost : Int -> (Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run3AtMost =
    Logic.Goal.run3 << Logic.Goal.AtMost


{-| -}
run4AtMost : Int -> (Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run4AtMost =
    Logic.Goal.run4 << Logic.Goal.AtMost


{-| -}
run5AtMost : Int -> (Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run5AtMost =
    Logic.Goal.run5 << Logic.Goal.AtMost


{-| -}
run6AtMost : Int -> (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run6AtMost =
    Logic.Goal.run6 << Logic.Goal.AtMost


{-| -}
run7AtMost : Int -> (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run7AtMost =
    Logic.Goal.run7 << Logic.Goal.AtMost


{-| -}
run8AtMost : Int -> (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run8AtMost =
    Logic.Goal.run8 << Logic.Goal.AtMost



-- DEBUG


{-| Convert to a representation that's useful for debugging and testing.

    toString [ int 1, float 3.14, bool True, bool False, char 'c', string "apple", custom "banana" ] == "(1 3.14 #t #f c apple banana)"

    toString [ null, cons (int 1) (int 2), list [ int 1 ], list [ int 1, bool True ] ] == "(() (1 . 2) (1) (1 #t))"

    toString [ dottedList (int 1) [ bool True ] (char 'a') ] == "((1 #t . a))"

**N.B.** _For convenience, it assumes your custom type is `String`. If your custom type
is something else you can use [`customToString`](#customToString) instead._

-}
toString : List (Value String) -> String
toString =
    customToString (Default identity)


{-| Used by [`customToString`](#customToString) to determine how to represent your custom type.

With the `Default` constructor you get to use the default representation for the constants while
providing a function to represent your custom type.

The default representation for the constants is as follows:

    toString [ int 1 ] == "(1)"

    toString [ float 3.14 ] == "(3.14)"

    toString [ bool True ] == "(#t)"

    toString [ bool False ] == "(#f)"

    toString [ char 'a' ] == "(a)"

    toString [ string "xyz" ] == "(xyz)"

If you want to change the default representation for the constants you can do that by using
the `Alternative` constructor.

-}
type Presenter a
    = Default (a -> String)
    | Alternative (Constant a -> String)


{-| Convert to a representation that's useful for debugging and testing.

Additionally, you get to determine how to represent your custom type.

-}
customToString : Presenter a -> List (Value a) -> String
customToString presenter =
    Logic.Goal.toString <|
        case presenter of
            Default stringify ->
                constantToString stringify

            Alternative stringify ->
                stringify


constantToString : (a -> String) -> Constant a -> String
constantToString stringify const =
    case const of
        Int n ->
            String.fromInt n

        Float f ->
            String.fromFloat f

        Bool b ->
            if b then
                "#t"

            else
                "#f"

        Char c ->
            String.fromChar c

        String s ->
            s

        Custom a ->
            stringify a
