module CSVParser exposing (..)

import Combine as C exposing ((<$>), (<|>), (<*))
import Combine.Num as N
import Combine.Char as Ch
import Time.DateTime as T


parseSubmissionTimestamp : C.Parser s T.DateTime
parseSubmissionTimestamp =
    parseDateTime


parseCustomerName : C.Parser s String
parseCustomerName =
    parseString


parseCustomerEmailAddress : C.Parser s String
parseCustomerEmailAddress =
    parseString


parseDescription : C.Parser s String
parseDescription =
    parseString


parseOpen : C.Parser s Bool
parseOpen =
    let
        match string =
            case string of
                "true" ->
                    True

                "false" ->
                    False

                _ ->
                    True

        parseBoolean =
            match <$> (C.string "true" <|> C.string "false")
    in
        parseBoolean <* (C.string ",")


parseClosedTimestamp : C.Parser s T.DateTime
parseClosedTimestamp =
    parseDateTime


parseEmployeeName : C.Parser s String
parseEmployeeName =
    String.fromList <$> C.manyTill Ch.anyChar (Ch.newline)


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
