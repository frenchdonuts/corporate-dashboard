module Issues exposing (..)

import CSVParser exposing (..)
import Combine exposing ((<$>), (<*>))
import Time.DateTime as T
import Html as H exposing (Html, text)
import Material.Table as Table
import Material.Grid as Grid
import Material.Options as O
import Http


type alias Model =
    { issues : List Issue
    , sortKey : Key
    , sortOrder : SortOrder
    , filterKey : Key
    , filterString : String
    }


type alias Issue =
    { submissionTimestamp : T.DateTime
    , customerName : String
    , customerEmailAddress : String
    , description : String
    , open : Bool
    , closedTimestamp : T.DateTime
    , employeeName : String
    }


type SortOrder
    = ASC
    | DSC


cycle : SortOrder -> SortOrder
cycle order =
    case order of
        ASC ->
            DSC

        DSC ->
            ASC


type Key
    = SubmissionTimestamp
    | CustomerName
    | CustomerEmail
    | Description
    | Open
    | ClosedTimestamp
    | EmployeeName


sorter : Key -> SortOrder -> (Issue -> Issue -> Order)
sorter sortKey sortOrder =
    let
        comparisonFn compare =
            if sortOrder == ASC then
                compare
            else
                reverseComparison compare

        reverseComparison compare a b =
            case compare a b of
                LT ->
                    GT

                EQ ->
                    EQ

                GT ->
                    LT

        compareBool b1 b2 =
            case ( b1, b2 ) of
                ( True, False ) ->
                    GT

                ( False, True ) ->
                    LT

                _ ->
                    EQ
    in
        case sortKey of
            SubmissionTimestamp ->
                comparisonFn (\i1 i2 -> T.compare i1.submissionTimestamp i2.submissionTimestamp)

            CustomerName ->
                comparisonFn (\i1 i2 -> compare i1.customerName i2.customerName)

            CustomerEmail ->
                comparisonFn (\i1 i2 -> compare i1.customerEmailAddress i2.customerEmailAddress)

            Description ->
                comparisonFn (\i1 i2 -> compare i1.description i2.description)

            Open ->
                comparisonFn (\i1 i2 -> compareBool i1.open i2.open)

            ClosedTimestamp ->
                comparisonFn (\i1 i2 -> T.compare i1.closedTimestamp i2.closedTimestamp)

            EmployeeName ->
                comparisonFn (\i1 i2 -> compare i1.employeeName i2.employeeName)


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
    | Reorder Key
    | NoOp


init : ( Model, Cmd Msg )
init =
    { issues = []
    , sortKey = SubmissionTimestamp
    , sortOrder = DSC
    , filterKey = CustomerName
    , filterString = ""
    }
        ! [ getIssuesCSVCmd ]


update : Msg -> Model -> Model
update msg model =
    let
        { issues, sortKey, sortOrder, filterKey, filterString } =
            model
    in
        case msg of
            Issues httpResult ->
                case httpResult of
                    Ok csv ->
                        case Combine.parse parseIssues csv of
                            Ok ( _, _, issues ) ->
                                { model | issues = issues }

                            Err err ->
                                Debug.log ("CSV parser error: " ++ (toString err)) model

                    Err err ->
                        model

            Reorder newKey ->
                if newKey == sortKey then
                    { model | sortOrder = cycle sortOrder }
                else
                    { model | sortOrder = ASC }

            NoOp ->
                model


view : Model -> Html Msg
view model =
    let
        { issues, sortKey, sortOrder, filterKey, filterString } =
            model

        containsFilterString =
            String.contains filterString

        filter issue =
            case filterKey of
                SubmissionTimestamp ->
                    containsFilterString <| T.toISO8601 issue.submissionTimestamp

                CustomerName ->
                    containsFilterString issue.customerName

                CustomerEmail ->
                    containsFilterString issue.customerEmailAddress

                Description ->
                    containsFilterString issue.description

                Open ->
                    containsFilterString <| toString issue.open

                ClosedTimestamp ->
                    containsFilterString <| T.toISO8601 issue.closedTimestamp

                EmployeeName ->
                    containsFilterString issue.employeeName

        filteredAndSortedIssues =
            issues
                |> List.filter filter
                |> List.sortWith (sorter sortKey sortOrder)

        columns =
            [ CustomerName
            , EmployeeName
            , Description
            , Open
            , CustomerEmail
            , SubmissionTimestamp
            , ClosedTimestamp
            ]
    in
        Table.table
            [ O.css "display" "block"
            , O.css "overflow-x" "scroll"
            ]
            [ Table.thead [] (List.map (headerColumn model) columns)
            , Table.tbody []
                (List.map (issueRow columns) filteredAndSortedIssues)
            ]


headerColumn : Model -> Key -> Html Msg
headerColumn { issues, sortKey, sortOrder } key =
    let
        attrs =
            if sortKey == key then
                case sortOrder of
                    ASC ->
                        [ Table.ascending, Table.onClick (Reorder key) ]

                    DSC ->
                        [ Table.descending, Table.onClick (Reorder key) ]
            else
                [ Table.onClick (Reorder key) ]

        columnName key =
            case key of
                SubmissionTimestamp ->
                    "Submission Timestamp"

                CustomerName ->
                    "Customer Name"

                CustomerEmail ->
                    "Customer Email"

                Description ->
                    "Description"

                Open ->
                    "Open/Closed"

                ClosedTimestamp ->
                    "Closed Timestamp"

                EmployeeName ->
                    "Employee Name"
    in
        Table.th attrs [ text (columnName key) ]


dateTimeToString : T.DateTime -> String
dateTimeToString dateTime =
    let
        month =
            toString <| T.month dateTime

        day =
            toString <| T.day dateTime

        year =
            toString <| T.year dateTime
    in
        month ++ "/" ++ day ++ "/" ++ year


issueRow : List Key -> Issue -> Html Msg
issueRow columns issue =
    let
        { submissionTimestamp, customerName, customerEmailAddress, description, open, closedTimestamp, employeeName } =
            issue

        toTd string =
            Table.td [] [ text <| string ]

        columnData key =
            case key of
                SubmissionTimestamp ->
                    toTd (dateTimeToString submissionTimestamp)

                CustomerName ->
                    toTd customerName

                CustomerEmail ->
                    toTd employeeName

                Description ->
                    toTd description

                Open ->
                    toTd (toString open)

                ClosedTimestamp ->
                    toTd (dateTimeToString closedTimestamp)

                EmployeeName ->
                    toTd employeeName
    in
        Table.tr [] (List.map columnData columns)
