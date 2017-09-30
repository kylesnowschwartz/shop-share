module RecordHelpers
    exposing
        ( replaceById
        )

import List.Extra exposing (replaceIf)


replaceById : { a | id : b } -> List { a | id : b } -> List { a | id : b }
replaceById replacement =
    replaceIf (\x -> x.id == replacement.id) replacement
