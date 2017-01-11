module CSVParser exposing (..)

import Combine as C exposing ((<$>), (<|>), (<*))
import Combine.Num as N
import Combine.Char as Ch
import Time.DateTime as T


parseTime : C.Parser s Float
parseTime =
    let
        toDateTime =
            Result.withDefault 0 << String.toFloat
    in
        toDateTime <$> C.while ((/=) ',') <* (C.string ",")


parseDateTime : C.Parser s T.DateTime
parseDateTime =
    let
        epoch =
            T.fromTimestamp 0

        toDateTime =
            Result.withDefault epoch << Result.map T.fromTimestamp << String.toFloat
    in
        toDateTime <$> C.while ((/=) ',') <* (C.string ",")


parseString : C.Parser s String
parseString =
    String.fromList <$> C.manyTill Ch.anyChar (C.string ",")
