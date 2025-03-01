# The Reasoned Schemer (2nd Edition) in Elm

[The Reasoned Schemer (2nd Edition)](https://mitpress.mit.edu/9780262535519/the-reasoned-schemer/) is a book by [Daniel P. Friedman](https://en.wikipedia.org/wiki/Daniel_P._Friedman), [William E. Byrd](http://webyrd.net/), [Oleg Kiselyov](https://okmij.org/ftp/), and [Jason Hemann](https://scholar.google.com/citations?user=SePR8OkAAAAJ&hl=en) that shows the beauty and elegance of relational programming. Relational programming, as presented in the book, captures the essence of logic programming.

This package shows how to implement and embed their relational programming language, [miniKanren](https://minikanren.org/), using Elm.

## An example

William E. Byrd has decribed `appendo` as the "Hello, World!" of the relational programming. ([video](https://youtu.be/AdKXXN5-ApQ?t=1079))

So, here it is:

```elm
import Logic exposing (..)


appendo : Value a -> Value a -> Value a -> Goal a
appendo l t out =
    conde
        [ [ nullo l, equals t out ]
        , [ fresh3
                (\a d res ->
                    conj
                        [ conso a d l
                        , conso a res out
                        , lazy (\_ -> appendo d t res)
                        ]
                )
          ]
        ]


nullo : Value a -> Goal a
nullo x =
    equals null x


conso : Value a -> Value a -> Value a -> Goal a
conso a d p =
    conj
        [ caro p a
        , cdro p d
        ]


caro : Value a -> Value a -> Goal a
caro p a =
    fresh
        (\d ->
            equals (cons a d) p
        )


cdro : Value a -> Value a -> Goal a
cdro p d =
    fresh
        (\a ->
            equals (cons a d) p
        )
```

And, this is how it works:

```elm
let
    actual =
        toString <| run3AtMost 6 appendo

    expected =
        String.join " "
            [ "((() _0 _0)"
            , "((_0) _1 (_0 . _1))"
            , "((_0 _1) _2 (_0 _1 . _2))"
            , "((_0 _1 _2) _3 (_0 _1 _2 . _3))"
            , "((_0 _1 _2 _3) _4 (_0 _1 _2 _3 . _4))"
            , "((_0 _1 _2 _3 _4) _5 (_0 _1 _2 _3 _4 . _5)))"
            ]
in
actual == expected
-- True : Bool
```

For many more examples check out the source code for the `Book.*` modules as well as the extensive suite of tests.

## Public API

The public API is exposed via the `Logic` module.

**N.B.** _This section only presents a very high-level overview. Please see the documentation for more details._

### Value

The values (or terms) that can be associated with (logic) variables.

```elm
type Value a
```

#### Ways to construct values

Constants:

```elm
int
float
bool
char
string
custom
```

Pairs, lists, and dotted lists:

```elm
null       -- '()
cons       -- (cons 'a 'b)
list       -- '(a b c)
dottedList -- '(a b . c)
```

#### Ways to deconstruct values

To deconstruct constants you can use:

```elm
toConstant : Value a -> Constant a

type Constant a
    = Int Int
    | Float Float
    | Bool Bool
    | Char Char
    | String String
    | Custom a
```

To deconstruct pairs you can use:

```elm
car
cdr
uncons
```

### Goal

Goals (or relations) are functions which more or less take a substitution and, if it returns, produces a stream of substitutions.

```elm
type Goal a
```

#### Base goals

```elm
succeed -- #s
fail    -- #u

always
never
```

#### Relational operators

```elm
equals  -- ≡

disj2
disj

conj2
conj

conde
```

#### Define recursive goals

```elm
lazy
```

#### Introduce lexically-scoped logic variables

```elm
fresh
fresh2
fresh3
fresh4
fresh5
fresh6
fresh7
fresh8
```

#### Non-relational operators

```elm
once
conda
condu
```

### Runners

```elm
run
run2
run3
run4
run5
run6
run7
run8

runAtMost
run2AtMost
run3AtMost
run4AtMost
run5AtMost
run6AtMost
run7AtMost
run8AtMost
```

### Presenters

#### Default

```elm
toString : List (Value String) -> String
```

#### Custom

```elm
type Presenter a
    = Default (a -> String)
    | Alternative (Constant a -> String)

customToString : Presenter a -> List (Value a) -> String
```

## Implementation details

The `Logic.*` modules provide the low-level API upon which the `Logic` module is built. It is based on my understanding of "Chapter 10: Under the Hood" and "Connecting the Wires" from the book.

If you carefully read the source code of the modules from top to bottom in the following order:

- `Logic.Variable`
- `Logic.Value`
- `Logic.Stream`
- `Logic.Substitution`
- `Logic.Goal`

You should get a good sense of how it all works.

## Development

Enter the development environment using:

```bash
devbox shell
```

Aliases are provided for running common tasks:

```bash
f # Format Elm code
p # Preview docs
t # Run tests
```
