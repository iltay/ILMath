module Types =

    type Error =
        | OutOfBound
        | NotDefind
        | InvalidFunction
        | DuplicateFunctionName
        | InvalidParameter
        | DuplicateParameter
        | MissingParameter
        | MissingValue