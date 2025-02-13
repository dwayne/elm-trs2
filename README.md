# The Reasoned Schemer (2nd Edition) in Elm

[The Reasoned Schemer](https://mitpressbookstore.mit.edu/book/9780262535519) is a book by [Daniel P. Friedman](https://en.wikipedia.org/wiki/Daniel_P._Friedman), [William E. Byrd](http://webyrd.net/), [Oleg Kiselyov](https://okmij.org/ftp/), and [Jason Hemann](https://scholar.google.com/citations?user=SePR8OkAAAAJ&hl=en).

The main purpose of the book is to show the beauty and elegance of relational programming. Relational programming, as presented in the book, captures the essence of logic programming. The book shows how to implement a relational programming language in Scheme, or in any other functional programming language.

This repository shows how to implement and embed the relational programming language, as presented in the book, using Elm. The implementation has been extensively tested to ensure its correctness.

## Examples

WIP

## Public API

The public API is exposed by the `Logic` module.

### Value

```elm
type Value a
```

#### Ways to construct values

**Constants**

```elm
int
float
bool
char
string
custom
```

**Pairs, lists, and dotted lists**

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

```elm
type Goal a
```

#### Relational operators

```elm
succeed -- #s
fail    -- #u

equals  -- ≡

disj2
disj

conj2
conj

conde

always
never

lazy

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
customToString : Presenter a -> List (Value a) -> String

type Presenter a
    = Default (a -> String)
    | Alternative (Constant a -> String)
```

## Implementation details

WIP

### About `lazy`

- Recall `defrel` and how you were lead to `lazy`

WIP

### Alternative API for `fresh`

Should the API for `fresh` be:

```elm
fresh : (Value a -> Goal a) -> Goal a
```

which it currently is, or:

```elm
fresh : (Value a -> List (Goal a)) -> Goal a
```

#### Before

```elm
fresh
    (\x ->
        conj
            [ a
            , b
            , c
            ]
    )
```

#### Change API

```elm
callFresh : String -> (Value a -> List (Goal a)) -> Goal a
callFresh name f =
    Goal
        (\state ->
            let
                var =
                    Var (Variable name state.nextId)
            in
            apply (conj (f var)) { state | nextId = state.nextId + 1 }
        )

fresh : (Value a -> List (Goal a)) -> Goal a
fresh =
    callFresh internalName
```

#### After

```elm
fresh
    (\x ->
        [ a
        , b
        , c
        ]
    )
```

Notice the missing `conj`.

#### Thoughts

- Advantages
  - Many examples use multiple goals in the body of `fresh`.
  - There would be one less indentation level.
- Disadvantages
  - I won't be able to write `fresh (\x -> g x)` as `fresh g` anymore.

### Alternative API for `run`

Similarly, we can change the API for `run` as follows:

```elm
run : Length -> (Value a -> List (Goal a)) -> List (Value a)
```

And, it leads to similar advantages and disadvantages as it did for `fresh`.
