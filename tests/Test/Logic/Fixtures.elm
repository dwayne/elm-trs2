module Test.Logic.Fixtures exposing
    ( a
    , b
    , c
    , d
    , u
    , v
    , vu
    , vv
    , vw
    , vx
    , vy
    , vz
    , w
    , x
    , y
    , z
    )

import Logic.Value exposing (Value(..))
import Logic.Variable exposing (Variable)



-- VARIABLES


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



-- VALUES


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


a =
    Const "a"


b =
    Const "b"


c =
    Const "c"


d =
    Const "d"
