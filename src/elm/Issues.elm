module Issues exposing (..)

import CSVParser exposing (..)
import Combine exposing ((<$>), (<*>))
import Time.DateTime as T
import Html as H exposing (Html, text)
import Material.Table as Table
import Http


type alias Model =
    List Issue


type alias Issue =
    { submissionTimestamp : T.DateTime
    , customerName : String
    , customerEmailAddress : String
    , description : String
    , open : Bool
    , closedTimestamp : T.DateTime
    , employeeName : String
    }


getIssuesCSVCmd : Cmd Msg
getIssuesCSVCmd =
    Http.send Issues <|
        Http.getString "http://localhost:8080/static/issues.csv"


parseIssues : Combine.Parser s (List Issue)
parseIssues =
    Combine.manyTill parseIssue Combine.end


parseIssue : Combine.Parser s Issue
parseIssue =
    Issue
        <$> parseSubmissionTimestamp
        <*> parseCustomerName
        <*> parseCustomerEmailAddress
        <*> parseDescription
        <*> parseOpen
        <*> parseClosedTimestamp
        <*> parseEmployeeName


type Msg
    = Issues (Result Http.Error String)
    | NoOp


init : ( Model, Cmd Msg )
init =
    [] ! [ getIssuesCSVCmd ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        Issues httpResult ->
            case httpResult of
                Ok csv ->
                    case Combine.parse parseIssues csv of
                        Ok ( _, _, issues ) ->
                            issues

                        Err err ->
                            Debug.log ("CSV parser error: " ++ (toString err)) []

                Err err ->
                    []

        NoOp ->
            model


view : Model -> Html Msg
view model =
    Table.table []
        [ Table.thead [] header
        , Table.tbody []
            (List.map issueRow model)
        ]


header : List (Html Msg)
header =
    let
        customerName =
            Table.th [] [ text "Customer Name" ]

        employeeName =
            Table.th [] [ text "Employee Name" ]

        description =
            Table.th [] [ text "Description" ]

        open =
            Table.th [] [ text "Open/Closed" ]

        customerEmailAddress =
            Table.th [] [ text "Customer Email" ]

        submissionTimestamp =
            Table.th [ Table.numeric ] [ text "Submission Timestamp" ]

        closedTimestamp =
            Table.th [ Table.numeric ] [ text "Closed Timestamp" ]
    in
        [ customerName
        , employeeName
        , description
        , open
        , customerEmailAddress
        , submissionTimestamp
        , closedTimestamp
        ]


issueRow : Issue -> Html Msg
issueRow issue =
    let
        { submissionTimestamp, customerName, customerEmailAddress, description, open, closedTimestamp, employeeName } =
            issue
    in
        Table.tr []
            [ Table.td [] [ text customerName ]
            , Table.td [] [ text employeeName ]
            , Table.td [] [ text description ]
            , Table.td [] [ text (toString open) ]
            , Table.td [] [ text (T.toISO8601 submissionTimestamp) ]
            , Table.td [] [ text (T.toISO8601 closedTimestamp) ]
            ]
