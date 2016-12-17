module Issues exposing (..)

import CSVParser exposing (..)
import Combine exposing ((<$>), (<*>))
import Time.DateTime as T
import Html as H exposing (Html)
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
    H.div [] [ H.text <| toString model ]


issueView : Issue -> Html Msg
issueView issue =
    let
        { submissionTimestamp, customerName, customerEmailAddress, description, open, closedTimestamp, employeeName } =
            issue
    in
        H.div [] []
