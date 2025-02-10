module Test.Logic.Substitution exposing (suite)

import Expect
import Logic.Substitution as Substitution exposing (Substitution)
import Logic.Value as Value exposing (Value(..))
import Logic.Variable as Variable exposing (Variable)
import Test exposing (Test, describe, test)
import Test.Fixtures exposing (..)


suite : Test
suite =
    describe "Logic.Substitution"
        [ walkSuite
        , occursSuite
        , extendSuite
        , unifySuite
        , walkAllSuite
        , reifiedNameSubstitutionSuite
        , reifySuite
        ]


walkSuite : Test
walkSuite =
    let
        sub1 =
            -- `((,z . a) (,x . ,w) (,y . ,z))
            [ ( z, a ), ( x, vw ), ( y, vz ) ]

        sub2 =
            -- `((,x . ,y) (,v . ,x) (,w . ,x))
            [ ( x, vy ), ( v, vx ), ( w, vx ) ]
    in
    describe "walk" <|
        List.map
            (testWalk identity)
            [ { value = vz
              , substitution = sub1
              , expected = a
              }
            , { value = vy
              , substitution = sub1
              , expected = a
              }
            , { value = vx
              , substitution = sub1
              , expected = vw
              }
            , { value = vu
              , substitution = sub1
              , expected = vu
              }
            , { value = vx
              , substitution = sub2
              , expected = vy
              }
            , { value = vv
              , substitution = sub2
              , expected = vy
              }
            , { value = vw
              , substitution = sub2
              , expected = vy
              }
            , { value = vw
              , substitution =
                    -- `((,x . b) (,z . ,y) (,w . (,x d ,z)))
                    [ ( x, b )
                    , ( z, vy )
                    , ( w, Pair vx (Pair d (Pair vz Null)) )
                    ]
              , expected = Pair vx (Pair d (Pair vz Null))
              }
            ]


testWalk :
    (a -> String)
    ->
        { value : Value a
        , substitution : Substitution a
        , expected : Value a
        }
    -> Test
testWalk stringify { value, substitution, expected } =
    let
        description =
            Debug.toString
                { value = Value.toString stringify value
                , substitution = Substitution.toString stringify substitution
                }
    in
    test description <|
        \_ ->
            Substitution.walk value substitution
                |> Expect.equal expected


occursSuite : Test
occursSuite =
    describe "occurs" <|
        List.map
            (testOccurs identity)
            [ { variable = u
              , value = vv
              , substitution = []
              , expected = False
              }
            , { variable = u
              , value = vu
              , substitution = []
              , expected = True
              }
            , { variable = x
              , value = vy
              , substitution =
                    -- `((,w . a) (,z . ,x) (,y . ,z))
                    [ ( w, a )
                    , ( z, vx )
                    , ( y, vz )
                    ]
              , expected = True
              }
            , { variable = x
              , value =
                    -- `(a ,y)
                    Pair a (Pair vy Null)
              , substitution =
                    -- `((,z . ,w) (,y . (,x)))
                    [ ( z, vw )
                    , ( y, Pair vx Null )
                    ]
              , expected = True
              }
            ]


testOccurs :
    (a -> String)
    ->
        { variable : Variable
        , value : Value a
        , substitution : Substitution a
        , expected : Bool
        }
    -> Test
testOccurs stringify { variable, value, substitution, expected } =
    let
        description =
            Debug.toString
                { variable = Variable.toString variable
                , value = Value.toString stringify value
                , substitution = Substitution.toString stringify substitution
                }
    in
    test description <|
        \_ ->
            Substitution.occurs variable value substitution
                |> Expect.equal expected


extendSuite : Test
extendSuite =
    describe "extend" <|
        List.map
            (testExtendSuite identity)
            [ { variable = u
              , value = a
              , substitution = []
              , expected =
                    -- `((,u . a))
                    Just
                        [ ( u, a )
                        ]
              }
            , { variable = u
              , value = vu
              , substitution = []
              , expected = Nothing
              }
            , { variable = x
              , value = vy
              , substitution =
                    -- `((,w . a) (,z . ,x) (,y . ,z))
                    [ ( w, a )
                    , ( z, vx )
                    , ( y, vz )
                    ]
              , expected = Nothing
              }
            , { variable = x
              , value =
                    -- `(a ,y)
                    Pair a (Pair vy Null)
              , substitution =
                    -- `((,z . ,w) (,y . (,x)))
                    [ ( z, vw )
                    , ( y, Pair vx Null )
                    ]
              , expected = Nothing
              }

            --
            -- Notice that extend doesn't prevent
            -- the situation in which two or more associations
            -- have the same first element.
            --
            , { variable = u
              , value = b
              , substitution =
                    -- `((,u . a))
                    [ ( u, a )
                    ]
              , expected =
                    -- `((,u . b) (,u . a))
                    Just
                        [ ( u, b )
                        , ( u, a )
                        ]
              }
            ]


testExtendSuite :
    (a -> String)
    ->
        { variable : Variable
        , value : Value a
        , substitution : Substitution a
        , expected : Maybe (Substitution a)
        }
    -> Test
testExtendSuite stringify { variable, value, substitution, expected } =
    let
        description =
            Debug.toString
                { variable = Variable.toString variable
                , value = Value.toString stringify value
                , substitution = Substitution.toString stringify substitution
                }
    in
    test description <|
        \_ ->
            Substitution.extend variable value substitution
                |> Expect.equal expected


unifySuite : Test
unifySuite =
    describe "unify" <|
        List.map
            (testUnify identity)
            [ { value1 = vu
              , value2 = a
              , substitution = []
              , expected =
                    -- `((,u . a))
                    Just [ ( u, a ) ]
              }
            , { value1 = a
              , value2 = vu
              , substitution = []
              , expected =
                    -- `((,u . a))
                    Just [ ( u, a ) ]
              }
            , { value1 = vu
              , value2 = vv
              , substitution = []
              , expected =
                    -- `((,u . ,v))
                    Just [ ( u, vv ) ]
              }
            , { value1 = vv
              , value2 = vu
              , substitution = []
              , expected =
                    -- `((,v . ,u))
                    Just [ ( v, vu ) ]
              }
            , { value1 = vu
              , value2 = vv
              , substitution =
                    -- `((,w . b) (,v . ,w))
                    [ ( w, b )
                    , ( v, vw )
                    ]
              , expected =
                    -- `((,u . b) (,w . b) (,v . ,w))
                    Just
                        [ ( u, b )
                        , ( w, b )
                        , ( v, vw )
                        ]
              }
            , { value1 = vv
              , value2 = vu
              , substitution =
                    -- `((,w . b) (,v . ,w))
                    [ ( w, b )
                    , ( v, vw )
                    ]
              , expected =
                    -- `((,u . b) (,w . b) (,v . ,w))
                    Just
                        [ ( u, b )
                        , ( w, b )
                        , ( v, vw )
                        ]
              }
            , { value1 = vu
              , value2 = a
              , substitution =
                    -- `((,u . a))
                    [ ( u, a )
                    ]
              , expected =
                    -- `((,u . a))
                    Just [ ( u, a ) ]
              }
            , { value1 = vu
              , value2 = b
              , substitution =
                    -- `((,u . a))
                    [ ( u, a )
                    ]
              , expected = Nothing
              }
            , { value1 = vu
              , value2 = vv
              , substitution =
                    -- `((,u . a) (,v . a))
                    [ ( u, a )
                    , ( v, a )
                    ]
              , expected =
                    -- `((,u . a) (,v . a))
                    Just
                        [ ( u, a )
                        , ( v, a )
                        ]
              }
            , { value1 =
                    -- `(,x (,y))
                    Pair
                        vx
                        (Pair (Pair vy Null) Null)
              , value2 =
                    -- `((a b) (c))
                    Pair
                        (Pair a (Pair b Null))
                        (Pair (Pair c Null) Null)
              , substitution = []
              , expected =
                    -- `((,y . c) (,x . (a b)))
                    Just
                        [ ( y, c )
                        , ( x, Pair a (Pair b Null) )
                        ]
              }
            ]


testUnify :
    (a -> String)
    ->
        { value1 : Value a
        , value2 : Value a
        , substitution : Substitution a
        , expected : Maybe (Substitution a)
        }
    -> Test
testUnify stringify { value1, value2, substitution, expected } =
    let
        description =
            Debug.toString
                { value1 = Value.toString stringify value1
                , value2 = Value.toString stringify value2
                , substitution = Substitution.toString stringify substitution
                }
    in
    test description <|
        \_ ->
            Substitution.unify value1 value2 substitution
                |> Expect.equal expected


walkAllSuite : Test
walkAllSuite =
    describe "walkAll" <|
        List.map
            (testWalkAll identity)
            [ { value = vx
              , substitution = []
              , expected = vx
              }
            , { value = vx
              , substitution = [ ( x, vy ) ]
              , expected = vy
              }

            --
            -- This is what differentiates walkAll from walk.
            -- See the corresponding test in walkSuite.
            --
            , { value = vw
              , substitution =
                    -- `((,x . b) (,z . ,y) (,w . (,x d ,z)))
                    [ ( x, b )
                    , ( z, vy )
                    , ( w, Pair vx (Pair d (Pair vz Null)) )
                    ]
              , expected = Pair b (Pair d (Pair vy Null))
              }
            ]


testWalkAll :
    (a -> String)
    ->
        { value : Value a
        , substitution : Substitution a
        , expected : Value a
        }
    -> Test
testWalkAll stringify { value, substitution, expected } =
    let
        description =
            Debug.toString
                { value = Value.toString stringify value
                , substitution = Substitution.toString stringify substitution
                }
    in
    test description <|
        \_ ->
            Substitution.walkAll value substitution
                |> Expect.equal expected


reifiedNameSubstitutionSuite : Test
reifiedNameSubstitutionSuite =
    describe "reifiedNameSubstitutionSuite" <|
        List.map
            (testReifiedNameSubstitution identity)
            [ { value = vx
              , expected = [ ( x, ReifiedVar 0 ) ]
              }
            , { value = vy
              , expected = [ ( y, ReifiedVar 0 ) ]
              }
            , { value = Pair vx vy
              , expected = [ ( y, ReifiedVar 1 ), ( x, ReifiedVar 0 ) ]
              }
            , { value = Pair vx (Pair vy (Pair vx vz))
              , expected = [ ( z, ReifiedVar 2 ), ( y, ReifiedVar 1 ), ( x, ReifiedVar 0 ) ]
              }
            ]


testReifiedNameSubstitution :
    (a -> String)
    ->
        { value : Value a
        , expected : Substitution a
        }
    -> Test
testReifiedNameSubstitution stringify { value, expected } =
    test (Debug.toString { value = Value.toString stringify value }) <|
        \_ ->
            Substitution.reifiedNameSubstitution value
                |> Expect.equal expected


reifySuite : Test
reifySuite =
    describe "reify" <|
        List.map
            (testReify identity)
            [ let
                corn =
                    Const "corn"

                ice =
                    Const "ice"
              in
              { value = vx
              , substitution =
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
              , expected =
                    -- (_0 (_1 _0) corn _2 ((ice) _2))
                    let
                        u0 =
                            ReifiedVar 0

                        u1 =
                            ReifiedVar 1

                        u2 =
                            ReifiedVar 2
                    in
                    Pair u0 (Pair (Pair u1 (Pair u0 Null)) (Pair corn (Pair u2 (Pair (Pair (Pair ice Null) (Pair u2 Null)) Null))))
              }
            ]


testReify :
    (a -> String)
    ->
        { value : Value a
        , substitution : Substitution a
        , expected : Value a
        }
    -> Test
testReify stringify { value, substitution, expected } =
    let
        description =
            Debug.toString
                { value = Value.toString stringify value
                , substitution = Substitution.toString stringify substitution
                }
    in
    test description <|
        \_ ->
            Substitution.reify value substitution
                |> Expect.equal expected
