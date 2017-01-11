module Issues exposing (..)

import Data.Issue exposing (Issue)
import Time.DateTime as T
import Html as H exposing (Html, div, text)
import Html.Attributes as A
import Material
import Material.Textfield as Text
import Material.Table as Table
import Material.Spinner as Spinner
import Material.Grid as Grid
import Material.Options as O
import Http
import RemoteData as R
import Misc exposing ((+|+))


type alias Model =
    { sortKey : Key
    , sortOrder : SortOrder
    , filterKey : Key
    , filterString : String
    , mdl : Material.Model
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
                comparisonFn (\i1 i2 -> compare i1.submissionTimestamp i2.submissionTimestamp)

            CustomerName ->
                comparisonFn (\i1 i2 -> compare i1.customerName i2.customerName)

            CustomerEmail ->
                comparisonFn (\i1 i2 -> compare i1.customerEmailAddress i2.customerEmailAddress)

            Description ->
                comparisonFn (\i1 i2 -> compare i1.description i2.description)

            Open ->
                comparisonFn (\i1 i2 -> compareBool i1.open i2.open)

            ClosedTimestamp ->
                comparisonFn (\i1 i2 -> compare i1.closedTimestamp i2.closedTimestamp)

            EmployeeName ->
                comparisonFn (\i1 i2 -> compare i1.employeeName i2.employeeName)


type Msg
    = SetFilterString String
    | Reorder Key
    | Mdl (Material.Msg Msg)
    | NoOp


init : Model
init =
    { sortKey = SubmissionTimestamp
    , sortOrder = DSC
    , filterKey = CustomerName
    , filterString = ""
    , mdl = Material.model
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { sortKey, sortOrder, filterKey, filterString } =
            model
    in
        case msg of
            SetFilterString str ->
                { model | filterString = str } ! []

            Reorder newKey ->
                if newKey == sortKey then
                    { model | sortOrder = cycle sortOrder } ! []
                else
                    { model | sortOrder = ASC, sortKey = newKey } ! []

            Mdl msg ->
                Material.update msg model

            NoOp ->
                model ! []


view : R.RemoteData String (List Issue) -> Model -> Html Msg
view remoteDataIssues model =
    let
        { sortKey, sortOrder, filterKey, filterString } =
            model

        loaderOn =
            R.isNotAsked remoteDataIssues || R.isLoading remoteDataIssues

        issues =
            R.withDefault [] remoteDataIssues

        containsFilterString str =
            String.contains (String.toLower filterString) (String.toLower str)

        filter issue =
            List.any containsFilterString
                [ issue.customerName
                , issue.customerEmailAddress
                , issue.description
                , issue.employeeName
                ]

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
        div
            [ A.style [ ( "position", "relative" ) ] ]
            ([ Grid.grid []
                [ Grid.cell
                    [ O.css "z-index" "3"
                    ]
                    (form model)
                ]
             ]
                |> (+|+) (not loaderOn)
                    [ Table.table
                        [ O.css "display" "block"
                        , O.css "overflow-x" "scroll"
                        ]
                        [ Table.thead
                            [ O.css "z-index" "3"
                            ]
                            (List.map (headerColumn model) columns)
                        , Table.tbody
                            []
                            (List.map (issueRow columns) filteredAndSortedIssues)
                        ]
                    ]
                |> (+|+) loaderOn
                    [ Spinner.spinner
                        [ Spinner.active loaderOn
                        , O.css "margin" "auto"
                        ]
                    ]
            )


form : Model -> List (Html Msg)
form model =
    let
        { filterString } =
            model
    in
        [ Text.render Mdl
            [ 0 ]
            model.mdl
            [ Text.value filterString
            , Text.onInput SetFilterString
            ]
        ]


headerColumn : Model -> Key -> Html Msg
headerColumn { sortKey, sortOrder } key =
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
                    toTd (dateTimeToString <| T.fromTimestamp submissionTimestamp)

                CustomerName ->
                    toTd customerName

                CustomerEmail ->
                    toTd customerEmailAddress

                Description ->
                    toTd description

                Open ->
                    toTd (toString open)

                ClosedTimestamp ->
                    if closedTimestamp == -1 then
                        toTd "N/A"
                    else
                        toTd (dateTimeToString <| T.fromTimestamp closedTimestamp)

                EmployeeName ->
                    toTd employeeName
    in
        Table.tr [] (List.map columnData columns)
