module Logic exposing
    ( Constant(..)
    , Goal
    , Presenter(..)
    , Value
    , always
    , bool
    , car
    , cdr
    , char
    , conda
    , conde
    , condu
    , conj
    , conj2
    , cons
    , custom
    , customToString
    , disj
    , disj2
    , equals
    , fail
    , float
    , fresh
    , fresh2
    , fresh3
    , fresh4
    , fresh5
    , fresh6
    , fresh7
    , fresh8
    , int
    , lazy
    , list
    , never
    , null
    , run
    , run2
    , run2AtMost
    , run3
    , run3AtMost
    , run4
    , run4AtMost
    , run5
    , run5AtMost
    , run6
    , run6AtMost
    , run7
    , run7AtMost
    , run8
    , run8AtMost
    , runAtMost
    , string
    , succeed
    , toConstant
    , toString
    , uncons
    )

import Logic.Goal
import Logic.Value



-- VALUE


type alias Value a =
    Logic.Value.Value (Constant a)


type Constant a
    = Int Int
    | Float Float
    | Bool Bool
    | Char Char
    | String String
    | Custom a


int : Int -> Value a
int =
    Logic.Value.Const << Int


float : Float -> Value a
float =
    Logic.Value.Const << Float


bool : Bool -> Value a
bool =
    Logic.Value.Const << Bool


char : Char -> Value a
char =
    Logic.Value.Const << Char


string : String -> Value a
string =
    Logic.Value.Const << String


custom : a -> Value a
custom =
    Logic.Value.Const << Custom


toConstant : Value a -> Maybe (Constant a)
toConstant val =
    case val of
        Logic.Value.Const c ->
            Just c

        _ ->
            Nothing


null : Value a
null =
    Logic.Value.Null


cons : Value a -> Value a -> Value a
cons =
    Logic.Value.Pair


list : List (Value a) -> Value a
list vals =
    case vals of
        [] ->
            null

        val :: restVals ->
            cons val (list restVals)


car : Value a -> Maybe (Value a)
car val =
    case val of
        Logic.Value.Pair left _ ->
            Just left

        _ ->
            Nothing


cdr : Value a -> Maybe (Value a)
cdr val =
    case val of
        Logic.Value.Pair _ right ->
            Just right

        _ ->
            Nothing


uncons : Value a -> Maybe ( Value a, Value a )
uncons val =
    case val of
        Logic.Value.Pair left right ->
            Just ( left, right )

        _ ->
            Nothing



-- GOAL


type alias Goal a =
    Logic.Goal.Goal (Constant a)



-- RELATIONAL


succeed : Goal a
succeed =
    Logic.Goal.succeed


fail : Goal a
fail =
    Logic.Goal.fail


equals : Value a -> Value a -> Goal a
equals =
    Logic.Goal.equals


never : Goal a
never =
    Logic.Goal.never


disj2 : Goal a -> Goal a -> Goal a
disj2 =
    Logic.Goal.disj2


always : Goal a
always =
    Logic.Goal.always


disj : List (Goal a) -> Goal a
disj =
    Logic.Goal.disj


conj2 : Goal a -> Goal a -> Goal a
conj2 =
    Logic.Goal.conj2


conj : List (Goal a) -> Goal a
conj =
    Logic.Goal.conj


conde : List (List (Goal a)) -> Goal a
conde =
    Logic.Goal.conde


lazy : (() -> Goal a) -> Goal a
lazy =
    Logic.Goal.lazy



-- FRESH


fresh : (Value a -> Goal a) -> Goal a
fresh =
    Logic.Goal.fresh


fresh2 : (Value a -> Value a -> Goal a) -> Goal a
fresh2 =
    Logic.Goal.fresh2


fresh3 : (Value a -> Value a -> Value a -> Goal a) -> Goal a
fresh3 =
    Logic.Goal.fresh3


fresh4 : (Value a -> Value a -> Value a -> Value a -> Goal a) -> Goal a
fresh4 =
    Logic.Goal.fresh4


fresh5 : (Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> Goal a
fresh5 =
    Logic.Goal.fresh5


fresh6 : (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> Goal a
fresh6 =
    Logic.Goal.fresh6


fresh7 : (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> Goal a
fresh7 =
    Logic.Goal.fresh7


fresh8 : (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> Goal a
fresh8 =
    Logic.Goal.fresh8



-- NON-RELATIONAL


conda : List (List (Goal a)) -> Goal a
conda =
    Logic.Goal.conda


condu : List (List (Goal a)) -> Goal a
condu =
    Logic.Goal.condu



-- RUN


run : (Value a -> Goal a) -> List (Value a)
run =
    Logic.Goal.run Logic.Goal.Unbounded


run2 : (Value a -> Value a -> Goal a) -> List (Value a)
run2 =
    Logic.Goal.run2 Logic.Goal.Unbounded


run3 : (Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run3 =
    Logic.Goal.run3 Logic.Goal.Unbounded


run4 : (Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run4 =
    Logic.Goal.run4 Logic.Goal.Unbounded


run5 : (Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run5 =
    Logic.Goal.run5 Logic.Goal.Unbounded


run6 : (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run6 =
    Logic.Goal.run6 Logic.Goal.Unbounded


run7 : (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run7 =
    Logic.Goal.run7 Logic.Goal.Unbounded


run8 : (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run8 =
    Logic.Goal.run8 Logic.Goal.Unbounded


runAtMost : Int -> (Value a -> Goal a) -> List (Value a)
runAtMost =
    Logic.Goal.run << Logic.Goal.AtMost


run2AtMost : Int -> (Value a -> Value a -> Goal a) -> List (Value a)
run2AtMost =
    Logic.Goal.run2 << Logic.Goal.AtMost


run3AtMost : Int -> (Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run3AtMost =
    Logic.Goal.run3 << Logic.Goal.AtMost


run4AtMost : Int -> (Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run4AtMost =
    Logic.Goal.run4 << Logic.Goal.AtMost


run5AtMost : Int -> (Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run5AtMost =
    Logic.Goal.run5 << Logic.Goal.AtMost


run6AtMost : Int -> (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run6AtMost =
    Logic.Goal.run6 << Logic.Goal.AtMost


run7AtMost : Int -> (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run7AtMost =
    Logic.Goal.run7 << Logic.Goal.AtMost


run8AtMost : Int -> (Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Value a -> Goal a) -> List (Value a)
run8AtMost =
    Logic.Goal.run8 << Logic.Goal.AtMost



-- CONVERT


toString : List (Value String) -> String
toString =
    customToString (Default identity)


type Presenter a
    = Default (a -> String)
    | Alternative (Constant a -> String)


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
