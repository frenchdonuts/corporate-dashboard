module Data.Issue exposing (..)

import CSVParser exposing (parseDateTime, parseString)
import Time.DateTime as T
import Combine as C exposing ((<$>), (<|>), (<*), (<*>))
import Combine.Char as Ch
import Http
import RemoteData as R


type alias Issue =
    { submissionTimestamp : T.DateTime
    , customerName : String
    , customerEmailAddress : String
    , description : String
    , open : Bool
    , closedTimestamp : T.DateTime
    , employeeName : String
    }


getIssuesCmd : Cmd (R.RemoteData String (List Issue))
getIssuesCmd =
    let
        toIssues csv =
            C.parse parseIssues csv
                |> Result.map (\( _, _, issues ) -> issues)
                |> Result.mapError toString

        transform remoteData =
            R.mapError toString remoteData
                |> R.andThen (toIssues >> R.fromResult)
    in
        Http.getString "http://localhost:8080/static/issues.csv"
            |> Http.toTask
            |> R.asCmd
            |> Cmd.map transform


parseIssues : C.Parser s (List Issue)
parseIssues =
    C.manyTill parseIssue C.end


parseIssue : C.Parser s Issue
parseIssue =
    Issue
        <$> parseSubmissionTimestamp
        <*> parseCustomerName
        <*> parseCustomerEmailAddress
        <*> parseDescription
        <*> parseOpen
        <*> parseClosedTimestamp
        <*> parseEmployeeName


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
