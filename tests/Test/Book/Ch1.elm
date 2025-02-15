module Test.Book.Ch1 exposing (suite)

import Book.Ch1 exposing (..)
import Logic exposing (..)
import Test exposing (Test, describe)
import Test.Book.Fixtures exposing (testExample)


suite : Test
suite =
    describe "Book.Ch1" <|
        List.indexedMap
            testExample
            [ { input = run (\_ -> fail)
              , output = "()"
              }
            , { input = run (\_ -> equals pea pod)
              , output = "()"
              }
            , { input = run (\q -> equals q pea)
              , output = "(pea)"
              }
            , { input = run (\q -> equals pea q)
              , output = "(pea)"
              }
            , { input = run (equals pea)
              , output = "(pea)"
              }
            , { input = run (\_ -> succeed)
              , output = "(_0)"
              }
            , { input = run (\_ -> equals pea pea)
              , output = "(_0)"
              }
            , { input = run (\q -> equals q q)
              , output = "(_0)"
              }
            , { input =
                    run
                        (\_ ->
                            fresh
                                (\x ->
                                    equals pea x
                                )
                        )
              , output = "(_0)"
              }
            , { input =
                    run
                        (\q ->
                            fresh
                                (\x ->
                                    equals (cons x null) q
                                )
                        )
              , output = "((_0))"
              }
            , { input =
                    run
                        (\q ->
                            fresh
                                (\x ->
                                    equals x q
                                )
                        )
              , output = "(_0)"
              }
            , { input = run (\_ -> equals (list [ list [ list [ pea ] ], pod ]) (list [ list [ list [ pea ] ], pod ]))
              , output = "(_0)"
              }
            , { input = run (\q -> equals (list [ list [ list [ pea ] ], pod ]) (list [ list [ list [ pea ] ], q ]))
              , output = "(pod)"
              }
            , { input = run (\q -> equals (list [ list [ list [ q ] ], pod ]) (list [ list [ list [ pea ] ], pod ]))
              , output = "(pea)"
              }
            , { input =
                    run
                        (\q ->
                            fresh
                                (\x ->
                                    equals (list [ list [ list [ q ] ], pod ]) (list [ list [ list [ x ] ], pod ])
                                )
                        )
              , output = "(_0)"
              }
            , { input =
                    run
                        (\q ->
                            fresh
                                (\x ->
                                    equals (list [ list [ list [ q ] ], x ]) (list [ list [ list [ x ] ], pod ])
                                )
                        )
              , output = "(pod)"
              }
            , { input =
                    run
                        (\q ->
                            fresh
                                (\x ->
                                    equals (list [ x, x ]) q
                                )
                        )
              , output = "((_0 _0))"
              }
            , { input =
                    run
                        (\q ->
                            fresh
                                (\x ->
                                    fresh
                                        (\y ->
                                            equals (list [ q, y ]) (list [ list [ x, y ], x ])
                                        )
                                )
                        )
              , output = "((_0 _0))"
              }
            , { input =
                    run
                        (\q ->
                            fresh
                                (\x ->
                                    fresh
                                        (\y ->
                                            equals (list [ x, y ]) q
                                        )
                                )
                        )
              , output = "((_0 _1))"
              }
            , { input =
                    run
                        (\s ->
                            fresh
                                (\t ->
                                    fresh
                                        (\u ->
                                            equals (list [ t, u ]) s
                                        )
                                )
                        )
              , output = "((_0 _1))"
              }
            , { input =
                    run
                        (\q ->
                            fresh
                                (\x ->
                                    fresh
                                        (\y ->
                                            equals (list [ x, y, x ]) q
                                        )
                                )
                        )
              , output = "((_0 _1 _0))"
              }
            , { input =
                    run
                        (\_ ->
                            fresh
                                (\x ->
                                    equals (list [ x ]) x
                                )
                        )
              , output = "()"
              }
            , { input = run (\_ -> conj2 succeed succeed)
              , output = "(_0)"
              }
            , { input = run (\q -> conj2 succeed (equals corn q))
              , output = "(corn)"
              }
            , { input = run (\q -> conj2 fail (equals corn q))
              , output = "()"
              }
            , { input = run (\q -> conj2 (equals corn q) (equals meal q))
              , output = "()"
              }
            , { input = run (\q -> conj2 (equals corn q) (equals corn q))
              , output = "(corn)"
              }
            , { input = run (\_ -> disj2 fail fail)
              , output = "()"
              }
            , { input = run (\q -> disj2 (equals olive q) fail)
              , output = "(olive)"
              }
            , { input = run (\q -> disj2 fail (equals oil q))
              , output = "(oil)"
              }
            , { input = run (\q -> disj2 (equals olive q) (equals oil q))
              , output = "(olive oil)"
              }
            , { input =
                    run
                        (\q ->
                            fresh
                                (\x ->
                                    fresh
                                        (\y ->
                                            disj2
                                                (equals (list [ x, y ]) q)
                                                (equals (list [ y, x ]) q)
                                        )
                                )
                        )
              , output = "((_0 _1) (_0 _1))"
              }
            , { input =
                    run
                        (\x ->
                            disj2
                                (conj2 (equals olive x) fail)
                                (equals oil x)
                        )
              , output = "(oil)"
              }
            , { input =
                    run
                        (\x ->
                            disj2
                                (conj2 (equals olive x) succeed)
                                (equals oil x)
                        )
              , output = "(olive oil)"
              }
            , { input =
                    run
                        (\x ->
                            disj2
                                (equals oil x)
                                (conj2 (equals olive x) succeed)
                        )
              , output = "(oil olive)"
              }
            , { input =
                    run
                        (\x ->
                            disj2
                                (conj2 (equals virgin x) fail)
                                (disj2
                                    (equals olive x)
                                    (disj2
                                        succeed
                                        (equals oil x)
                                    )
                                )
                        )
              , output = "(olive _0 oil)"
              }
            , { input =
                    run
                        (\r ->
                            fresh
                                (\x ->
                                    fresh
                                        (\y ->
                                            conj2
                                                (equals split x)
                                                (conj2
                                                    (equals pea y)
                                                    (equals (list [ x, y ]) r)
                                                )
                                        )
                                )
                        )
              , output = "((split pea))"
              }
            , { input =
                    run
                        (\r ->
                            fresh
                                (\x ->
                                    fresh
                                        (\y ->
                                            conj2
                                                (conj2
                                                    (equals split x)
                                                    (equals pea y)
                                                )
                                                (equals (list [ x, y ]) r)
                                        )
                                )
                        )
              , output = "((split pea))"
              }
            , { input =
                    run
                        (\r ->
                            fresh2
                                (\x y ->
                                    conj2
                                        (conj2
                                            (equals split x)
                                            (equals pea y)
                                        )
                                        (equals (list [ x, y ]) r)
                                )
                        )
              , output = "((split pea))"
              }
            , { input =
                    run3
                        (\r x y ->
                            conj2
                                (conj2
                                    (equals split x)
                                    (equals pea y)
                                )
                                (equals (list [ x, y ]) r)
                        )
              , output = "(((split pea) split pea))"
              }
            , { input =
                    run2
                        (\x y ->
                            conj2
                                (equals split x)
                                (equals pea y)
                        )
              , output = "((split pea))"
              }
            , { input =
                    run2
                        (\x y ->
                            disj2
                                (conj2 (equals split x) (equals pea y))
                                (conj2 (equals red x) (equals bean y))
                        )
              , output = "((split pea) (red bean))"
              }
            , { input =
                    run
                        (\r ->
                            fresh2
                                (\x y ->
                                    conj2
                                        (disj2
                                            (conj2 (equals split x) (equals pea y))
                                            (conj2 (equals red x) (equals bean y))
                                        )
                                        (equals (list [ x, y, soup ]) r)
                                )
                        )
              , output = "((split pea soup) (red bean soup))"
              }
            , { input = run (\x -> teacupo x)
              , output = "(tea cup)"
              }
            , { input = run teacupo
              , output = "(tea cup)"
              }
            , { input =
                    run2
                        (\x y ->
                            disj2
                                (conj2 (teacupo x) (equals true y))
                                (conj2 (equals false x) (equals true y))
                        )
              , output = "((tea #t) (cup #t) (#f #t))"
              }
            , { input =
                    run2
                        (\x y ->
                            conj2
                                (teacupo x)
                                (teacupo y)
                        )
              , output = "((tea tea) (tea cup) (cup tea) (cup cup))"
              }
            , { input =
                    run2
                        (\x _ ->
                            conj2
                                (teacupo x)
                                (teacupo x)
                        )
              , output = "((tea _0) (cup _0))"
              }
            , { input =
                    run2
                        (\x y ->
                            disj2
                                (conj2 (teacupo x) (teacupo x))
                                (conj2 (equals false x) (teacupo y))
                        )
              , output = "((tea _0) (cup _0) (#f tea) (#f cup))"
              }
            , { input =
                    run2
                        (\x y ->
                            conde
                                [ [ equals split x, equals pea y ]
                                , [ equals red x, equals bean y ]
                                ]
                        )
              , output = "((split pea) (red bean))"
              }
            , { input =
                    run
                        (\x ->
                            conde
                                [ [ equals olive x, fail ]
                                , [ equals oil x ]
                                ]
                        )
              , output = "(oil)"
              }
            , { input =
                    run2
                        (\x y ->
                            conde
                                [ [ fresh
                                        (\z ->
                                            equals lentil z
                                        )
                                  ]
                                , [ equals x y ]
                                ]
                        )
              , output = "((_0 _1) (_0 _0))"
              }
            , { input =
                    run2
                        (\x y ->
                            conde
                                [ [ equals split x, equals pea y ]
                                , [ equals red x, equals bean y ]
                                , [ equals green x, equals lentil y ]
                                ]
                        )
              , output = "((split pea) (red bean) (green lentil))"
              }
            ]



-- VALUES


bean =
    string "bean"


corn =
    string "corn"


false =
    string "#f"


green =
    string "green"


lentil =
    string "lentil"


meal =
    string "meal"


oil =
    string "oil"


olive =
    string "olive"


pea =
    string "pea"


pod =
    string "pod"


red =
    string "red"


soup =
    string "soup"


split =
    string "split"


true =
    string "#t"


virgin =
    string "virgin"
