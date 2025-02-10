module Logic.Variable exposing (Id, Variable, toString)


type alias Variable =
    { name : String
    , id : Id
    }


type alias Id =
    Int


toString : Variable -> String
toString { name, id } =
    name ++ "." ++ String.fromInt id
