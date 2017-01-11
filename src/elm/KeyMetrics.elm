module KeyMetrics exposing (..)

import Html as H exposing (Html)
import Chart exposing (lChart, vBar, hBar, title, updateStyles, toHtml)
import Visualization.Scale as Scale exposing (ContinuousScale, ContinuousTimeScale)
import Visualization.Axis as Axis
import Visualization.List as List
import Visualization.Shape as Shape
import Visualization.Path as Path
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Http
import Json.Decode as J
import RemoteData as R
import RemoteData.Infix exposing (..)
import Task
import Date exposing (Date)
import Misc


type alias Model =
    { payingCustomersOverTimeData : R.RemoteData Http.Error (List ( Float, String ))
    , issuesOver12Months : R.RemoteData Http.Error (List Float)
    , currentDate : Maybe Date.Date
    }


type Msg
    = NoOp
    | DataFetched (R.RemoteData Http.Error ( List Float, List Float ))
    | CurrentTimeFetched Date.Date


init : ( Model, Cmd Msg )
init =
    { payingCustomersOverTimeData = R.NotAsked
    , issuesOver12Months = R.NotAsked
    , currentDate = Nothing
    }
        ! [ fetchissuesOver12Months
          , Task.perform CurrentTimeFetched Date.now
          ]


fetchissuesOver12Months : Cmd Msg
fetchissuesOver12Months =
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

                toChartData_ =
                    List.indexedMap (\i f -> ( Misc.toMonth i, f ))

                payingCustomersOverTime =
                    R.map (Tuple.first >> toChartData) remoteData

                issuesOverTime =
                    R.map Tuple.second remoteData
            in
                { model
                    | payingCustomersOverTimeData = payingCustomersOverTime
                    , issuesOver12Months = issuesOverTime
                }

        CurrentTimeFetched date ->
            { model | currentDate = Just date }


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
        { payingCustomersOverTimeData, issuesOver12Months } =
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
            [ issuesOverTimeBarChart model
              --, chartHtml payingCustomersOverTimeDataChart payingCustomersOverTimeData
              --, chartHtml issuesOverTimeChart issuesOver12Months
            ]


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


issuesOverTimeBarChart : Model -> Html Msg
issuesOverTimeBarChart model =
    let
        { issuesOver12Months, currentDate } =
            model

        dataTransform currentDate listLength i issuesThisMonth =
            ((Date.toTime currentDate) - (11 - toFloat i) * Misc.msInMonth)
                |> Date.fromTime
                |> flip (,) issuesThisMonth

        data =
            (\issues currentDate -> List.indexedMap (dataTransform currentDate (List.length issues)) issues)
                <$> (issuesOver12Months |> R.mapError toString)
                <*> (currentDate |> Result.fromMaybe "" >> R.fromResult)
                |> R.withDefault []

        xScale : ContinuousTimeScale
        xScale =
            Scale.time
                ( List.head data |> Maybe.map Tuple.first |> Maybe.withDefault (Date.fromTime 1448928000000), List.reverse data |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault (Date.fromTime 1456790400000) )
                ( 0, w - 2 * padding )

        yScale : ContinuousScale
        yScale =
            Scale.linear ( 0, 5 ) ( h - 2 * padding, 0 )

        opts : Axis.Options a
        opts =
            Axis.defaultOptions

        xAxis : Svg msg
        xAxis =
            Axis.axis { opts | orientation = Axis.Bottom, tickCount = List.length data } xScale

        yAxis : Svg msg
        yAxis =
            Axis.axis { opts | orientation = Axis.Left, tickCount = 5 } yScale

        barGenerator : ( Date, Float ) -> Path.Path -> Path.Path
        barGenerator ( x, y ) =
            let
                rw =
                    30

                -- Left
                rx =
                    (Scale.convert xScale x) - (rw / 2)

                -- Top
                ry =
                    Scale.convert yScale y

                rh =
                    Tuple.first (Scale.rangeExtent yScale) - ry
            in
                Path.rect rx ry rw rh

        bars : String
        bars =
            List.foldr barGenerator Path.begin data
                |> Path.toAttrString
    in
        svg
            [ width (toString w ++ "px"), height (toString h ++ "px") ]
            [ g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (h - padding) ++ ")") ]
                [ xAxis ]
            , g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString padding ++ ")") ]
                [ yAxis ]
            , g [ transform ("translate(" ++ toString padding ++ ", " ++ toString padding ++ ")"), class "series" ]
                [ Svg.path
                    [ d bars, stroke "none", strokeWidth "3px", fill "rgba(255, 0, 0, 0.54)" ]
                    []
                ]
            ]


data =
    [ ( Date.fromTime 1448928000000, 2 )
    , ( Date.fromTime 1451606400000, 2 )
    , ( Date.fromTime 1454284800000, 1 )
    , ( Date.fromTime 1456790400000, 1 )
    ]
