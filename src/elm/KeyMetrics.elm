module KeyMetrics exposing (..)

import Html as H exposing (Html)
import Chart exposing (lChart, vBar, hBar, title, updateStyles, toHtml)
import Visualization.Scale as Scale exposing (ContinuousScale, ContinuousTimeScale)
import Visualization.Axis as Axis
import Visualization.List as List
import Visualization.Shape as Shape
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Http
import Json.Decode as J
import RemoteData as R


type alias Model =
    { payingCustomersOverTimeData : R.RemoteData Http.Error (List ( Float, String ))
    , issuesOverTimeData : R.RemoteData Http.Error (List ( Float, String ))
    }


type Msg
    = NoOp
    | DataFetched (R.RemoteData Http.Error ( List Float, List Float ))


init : ( Model, Cmd Msg )
init =
    { payingCustomersOverTimeData = R.NotAsked
    , issuesOverTimeData = R.NotAsked
    }
        ! [ fetchIssuesOverTimeData ]


fetchIssuesOverTimeData : Cmd Msg
fetchIssuesOverTimeData =
    let
        decoder =
            J.map2 (,)
                (J.field "issuesOverTimeData" (J.list J.float))
                (J.field "payingCustomersOverTimeData" (J.list J.float))
    in
        Http.get "http://localhost:8080/static/key_metrics.json" decoder
            |> Http.send (R.fromResult >> DataFetched)


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        DataFetched remoteData ->
            let
                toChartData =
                    List.indexedMap (\i f -> ( f, toMonth i ))

                payingCustomersOverTime =
                    R.map (Tuple.first >> toChartData) remoteData

                issuesOverTime =
                    R.map (Tuple.second >> toChartData) remoteData
            in
                { model
                    | payingCustomersOverTimeData = payingCustomersOverTime
                    , issuesOverTimeData = issuesOverTime
                }


toMonth : Int -> String
toMonth i =
    if (i + 1) % 12 == 0 then
        "Dec"
    else if (i + 1) % 11 == 0 then
        "Nov"
    else if (i + 1) % 10 == 0 then
        "Oct"
    else if (i + 1) % 9 == 0 then
        "Sep"
    else if (i + 1) % 8 == 0 then
        "Aug"
    else if (i + 1) % 7 == 0 then
        "Jul"
    else if (i + 1) % 6 == 0 then
        "Jun"
    else if (i + 1) % 5 == 0 then
        "May"
    else if (i + 1) % 4 == 0 then
        "Apr"
    else if (i + 1) % 3 == 0 then
        "Mar"
    else if (i + 1) % 2 == 0 then
        "Feb"
    else
        "Jan"


view : Model -> Html Msg
view model =
    let
        { payingCustomersOverTimeData, issuesOverTimeData } =
            model

        defaultHtml =
            H.div [] []

        payingCustomersOverTimeDataChart =
            lChart
                >> Chart.title "Customers Over Time"

        issuesOverTimeChart =
            hBar
                >> Chart.title "Issues Over Time"

        chartHtml chart data =
            R.map (chart >> toHtml) data
                |> R.withDefault defaultHtml
    in
        H.div
            []
            [ chartHtml payingCustomersOverTimeDataChart payingCustomersOverTimeData
            , chartHtml issuesOverTimeChart issuesOverTimeData
            ]


issuesOverTimeBarChart : Model -> Svg Msg
issuesOverTimeBarChart model =
    let
        { issuesOverTimeData } =
            model
    in
        svg
            []
            []
