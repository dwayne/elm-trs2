module Logic.Variable exposing (Id, Variable, toString)


type alias Variable =
    { name : String -- N.B. The emtpy string is reserved for use internally.
    , id : Id
    }


type alias Id =
    Int


toString : Variable -> String
toString { name, id } =
    name ++ "." ++ String.fromInt id
