module Issues exposing (..)

import CSVParser exposing (..)
import Combine exposing ((<$>), (<*>))
import Time.DateTime as T
import Html as H exposing (Html, text)
import Material.Table as Table
import Material.Grid as Grid
import Http


type alias Model =
    ( List Issue, Key, SortOrder )


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
    ( [], SubmissionTimestamp, DSC ) ! [ getIssuesCSVCmd ]


update : Msg -> Model -> Model
update msg model =
    let
        ( issues, sortKey, sortOrder ) =
            model
    in
        case msg of
            Issues httpResult ->
                case httpResult of
                    Ok csv ->
                        case Combine.parse parseIssues csv of
                            Ok ( _, _, issues ) ->
                                ( issues, sortKey, sortOrder )

                            Err err ->
                                Debug.log ("CSV parser error: " ++ (toString err)) ( [], SubmissionTimestamp, sortOrder )

                    Err err ->
                        ( [], SubmissionTimestamp, sortOrder )

            Reorder newKey ->
                if newKey == sortKey then
                    ( issues, newKey, cycle sortOrder )
                else
                    ( issues, newKey, ASC )

            NoOp ->
                model


view : Model -> Html Msg
view model =
    let
        ( issues, sortKey, sortOrder ) =
            model

        sortedIssues =
            List.sortWith (sorter sortKey sortOrder) issues

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
        Grid.grid [ Grid.size Grid.Desktop 12 ]
            [ Grid.cell []
                [ Table.table []
                    [ Table.thead [] (List.map (headerColumn model) columns)
                    , Table.tbody []
                        (List.map (issueRow columns) sortedIssues)
                    ]
                ]
            ]


headerColumn : Model -> Key -> Html Msg
headerColumn ( issues, currentKey, order ) sortKey =
    let
        attrs =
            if currentKey == sortKey then
                case order of
                    ASC ->
                        [ Table.ascending, Table.onClick (Reorder sortKey) ]

                    DSC ->
                        [ Table.descending, Table.onClick (Reorder sortKey) ]
            else
                [ Table.onClick (Reorder sortKey) ]

        columnName sortKey =
            case sortKey of
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
        Table.th attrs [ text (columnName sortKey) ]


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

        ellipsize string =
            let
                truncatedString =
                    String.left 30 string
            in
                if String.length string > 30 then
                    truncatedString ++ "..."
                else
                    truncatedString

        toTd string =
            Table.td [] [ text <| ellipsize string ]

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
