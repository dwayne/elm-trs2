module Test.Logic.Goal exposing (suite)

import Expect
import Logic.Goal exposing (..)
import Logic.Substitution exposing (Substitution)
import Logic.Value exposing (Value(..))
import Logic.Variable exposing (Variable)
import Test exposing (Test, describe, test)
import Test.Fixtures exposing (..)


suite : Test
suite =
    describe "Logic.Goal"
        [ baseSuite
        , highLevelSuite
        ]


baseSuite : Test
baseSuite =
    describe "Base API" <|
        List.map
            testBase
            [ { description = "#s"
              , goal = succeed
              , length = Unbounded
              , expected = [ [] ]
              }
            , { description = "#u"
              , goal = fail
              , length = Unbounded
              , expected = []
              }
            , { description = "(≡ 'pea 'pod)"
              , goal = equals pea pod
              , length = Unbounded
              , expected = []
              }
            , { description = "(≡ 'pea u)"
              , goal = equals pea vu
              , length = Unbounded
              , expected = [ [ ( u, pea ) ] ]
              }
            , { description = "(≡ `(,x . b) `(a . ,y))"
              , goal = equals (Pair vx b) (Pair a vy)
              , length = Unbounded
              , expected = [ [ ( y, b ), ( x, a ) ] ]
              }
            , { description = "(disj2 (≡ 'olive x) (nevero))"
              , goal = disj2 (equals olive vx) never
              , length = AtMost 1
              , expected = [ [ ( x, olive ) ] ]
              }
            , { description = "(disj2 (nevero) (≡ 'olive x))"
              , goal = disj2 never (equals olive vx)
              , length = AtMost 1
              , expected = [ [ ( x, olive ) ] ]
              }
            , { description = "(alwayso)"
              , goal = always
              , length = AtMost 3
              , expected = [ [], [], [] ]
              }
            , { description =
                    """
                    (conj2
                        (disj2
                            (conj2 (≡ 'split x) (≡ 'pea y))
                            (conj2 (≡ 'red x) (≡ 'bean y)))
                        (≡ `(,x ,y soup) z))
                    """
              , goal =
                    conj2
                        (disj2
                            (conj2 (equals split vx) (equals pea vy))
                            (conj2 (equals red vx) (equals bean vy))
                        )
                        (equals (Pair vx (Pair vy (Pair soup Null))) vz)
              , length = Unbounded
              , expected =
                    [ [ ( z, Pair vx (Pair vy (Pair soup Null)) ), ( y, pea ), ( x, split ) ]
                    , [ ( z, Pair vx (Pair vy (Pair soup Null)) ), ( y, bean ), ( x, red ) ]
                    ]
              }
            , let
                plum =
                    Const "plum"
              in
              { description =
                    """
                    (call/fresh 'kiwi
                        (lambda (fruit)
                            (≡ 'plum fruit)))
                    """
              , goal =
                    callFresh
                        "kiwi"
                        (\fruit -> equals plum fruit)
              , length = AtMost 1
              , expected =
                    let
                        fruit =
                            Variable "kiwi" 1
                    in
                    [ [ ( fruit, plum ) ] ]
              }
            , { description =
                    """
                    (ifte #s
                        (≡ #f y)
                        (≡ #t y))
                    """
              , goal =
                    ifte
                        succeed
                        (equals false vy)
                        (equals true vy)
              , length = Unbounded
              , expected = [ [ ( y, false ) ] ]
              }
            , { description =
                    """
                    (ifte #u
                        (≡ #f y)
                        (≡ #t y))
                    """
              , goal =
                    ifte
                        fail
                        (equals false vy)
                        (equals true vy)
              , length = Unbounded
              , expected = [ [ ( y, true ) ] ]
              }
            , { description =
                    """
                    (ifte (≡ #t x)
                        (≡ #f y)
                        (≡ #t y))
                    """
              , goal =
                    ifte
                        (equals true vx)
                        (equals false vy)
                        (equals true vy)
              , length = Unbounded
              , expected = [ [ ( y, false ), ( x, true ) ] ]
              }
            , { description =
                    """
                    (ifte (disj2 (≡ #t x) (≡ #f x))
                        (≡ #f y)
                        (≡ #t y))
                    """
              , goal =
                    ifte
                        (disj2 (equals true vx) (equals false vx))
                        (equals false vy)
                        (equals true vy)
              , length = Unbounded
              , expected = [ [ ( y, false ), ( x, true ) ], [ ( y, false ), ( x, false ) ] ]
              }
            , { description =
                    """
                    (ifte (once (disj2 (≡ #t x) (≡ #f x)))
                        (≡ #f y)
                        (≡ #t y))
                    """
              , goal =
                    ifte
                        (once (disj2 (equals true vx) (equals false vx)))
                        (equals false vy)
                        (equals true vy)
              , length = Unbounded
              , expected = [ [ ( y, false ), ( x, true ) ] ]
              }
            ]


testBase :
    { description : String
    , goal : Goal a
    , length : Length
    , expected : List (Substitution a)
    }
    -> Test
testBase { description, goal, length, expected } =
    test description <|
        \_ ->
            runS length goal
                |> Expect.equal expected


highLevelSuite : Test
highLevelSuite =
    describe "High-level API" <|
        List.map
            (testHighLevel identity)
            [ { description =
                    """
                    (run* (x y)
                        (conde
                            ((teacupo x) (teacupo x))
                            ((≡ #f x) (teacupo y))))
                    """
              , values =
                    run2 Unbounded
                        (\x y ->
                            conde
                                [ [ teacupo x, teacupo x ]
                                , [ equals false x, teacupo y ]
                                ]
                        )
              , expected = "((tea _0) (cup _0) (#f tea) (#f cup))"
              }
            , { description =
                    """
                    (run* r
                        (fresh (x y)
                            (caro '(grape raisin pear) x)
                            (caro '((a) (b) (c)) y))
                            (≡ (cons x y) r)))
                    """
              , values =
                    run Unbounded
                        (\r ->
                            fresh2
                                (\x y ->
                                    conj
                                        [ caro (Pair grape (Pair raisin (Pair pear Null))) x
                                        , caro (Pair (Pair a Null) (Pair (Pair b Null) (Pair (Pair c Null) Null))) y
                                        , equals (Pair x y) r
                                        ]
                                )
                        )
              , expected = "((grape a))"
              }
            , { description =
                    """
                    (run* q
                        (conda
                            (#u #s)
                            (#s #u)))
                    """
              , values =
                    run Unbounded
                        (\q ->
                            conda
                                [ [ fail, succeed ]
                                , [ succeed, fail ]
                                ]
                        )
              , expected = "()"
              }
            , { description =
                    """
                    (run* q
                        (conda
                            (#u #s)
                            (#s #s)))
                    """
              , values =
                    run Unbounded
                        (\q ->
                            conda
                                [ [ fail, succeed ]
                                , [ succeed, succeed ]
                                ]
                        )
              , expected = "(_0)"
              }
            , { description =
                    """
                    (run* q
                        (conda
                            (#s #u)
                            (#s #s)))
                    """
              , values =
                    run Unbounded
                        (\q ->
                            conda
                                [ [ succeed, fail ]
                                , [ succeed, succeed ]
                                ]
                        )
              , expected = "()"
              }
            , { description =
                    """
                    (run* q
                        (conda
                            (#s #s)
                            (#s #u)))
                    """
              , values =
                    run Unbounded
                        (\q ->
                            conda
                                [ [ succeed, succeed ]
                                , [ succeed, fail ]
                                ]
                        )
              , expected = "(_0)"
              }
            , { description =
                    """
                    (run* q
                        (condu
                            ((alwayso) #s)
                            (#s #u)))
                    """
              , values =
                    run Unbounded
                        (\q ->
                            condu
                                [ [ always, succeed ]
                                , [ succeed, fail ]
                                ]
                        )
              , expected = "(_0)"
              }
            , { description = "(run 5 q (very-recursiveo))"
              , values = run (AtMost 5) (\q -> veryRecursiveo)
              , expected = "(_0 _0 _0 _0 _0)"
              }
            ]


testHighLevel :
    (a -> String)
    ->
        { description : String
        , values : List (Value a)
        , expected : String
        }
    -> Test
testHighLevel stringify { description, values, expected } =
    test description <|
        \_ ->
            toString stringify values
                |> Expect.equal expected



-- HELPERS


always =
    Logic.Goal.always


never =
    Logic.Goal.never


teacupo t =
    disj2 (equals tea t) (equals cup t)


caro p a =
    fresh
        (\d ->
            equals (Pair a d) p
        )


veryRecursiveo =
    conde
        [ [ never ]
        , [ lazy (\_ -> veryRecursiveo) ]
        , [ always ]
        , [ lazy (\_ -> veryRecursiveo) ]
        , [ never ]
        ]



-- VALUES


bean =
    Const "bean"


cup =
    Const "cup"


false =
    Const "#f"


grape =
    Const "grape"


olive =
    Const "olive"


pea =
    Const "pea"


pear =
    Const "pear"


pod =
    Const "pod"


raisin =
    Const "raisin"


red =
    Const "red"


soup =
    Const "soup"


split =
    Const "split"


tea =
    Const "tea"


true =
    Const "#t"
