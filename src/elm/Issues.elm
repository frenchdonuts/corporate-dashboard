module Issues exposing (..)

import CSVParser exposing (..)
import Combine exposing ((<$>), (<*>))
import Time.DateTime as T
import Html as H exposing (Html, text)
import Material.Table as Table
import Http


type alias Model =
    ( List Issue, SortKey, SortOrder )


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


type SortKey
    = SubmissionTimestamp
    | CustomerName
    | CustomerEmail
    | Description
    | Open
    | ClosedTimestamp
    | EmployeeName


sorter : SortKey -> SortOrder -> (Issue -> Issue -> Order)
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
    | Reorder SortKey
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

            Reorder newSortKey ->
                if newSortKey == sortKey then
                    ( issues, newSortKey, cycle sortOrder )
                else
                    ( issues, newSortKey, ASC )

            NoOp ->
                model


view : Model -> Html Msg
view model =
    let
        ( issues, sortKey, sortOrder ) =
            model

        sortedIssues =
            List.sortWith (sorter sortKey sortOrder) issues
    in
        Table.table []
            [ Table.thead [] (header model)
            , Table.tbody []
                (List.map issueRow sortedIssues)
            ]


header : Model -> List (Html Msg)
header model =
    let
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
        List.map (headerColumn model) columns


headerColumn : Model -> SortKey -> Html Msg
headerColumn ( issues, currentSortKey, order ) sortKey =
    let
        attrs =
            if currentSortKey == sortKey then
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
