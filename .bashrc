f () {
    format "$@"
}

t () {
    elm-test "$@"
}

export -f f t
