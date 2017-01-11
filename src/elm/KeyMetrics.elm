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
                (J.field "payingCustomersOverTimeData" (J.list J.float))
                (J.field "issuesOverTimeData" (J.list J.float))
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
    if i == 0 then
        ""
    else if i % 12 == 0 then
        "Dec"
    else if i % 12 == 1 then
        "Jan"
    else if i % 12 == 2 then
        "Feb"
    else if i % 12 == 3 then
        "Mar"
    else if i % 12 == 4 then
        "Apr"
    else if i % 12 == 5 then
        "May"
    else if i % 12 == 6 then
        "Jun"
    else if i % 12 == 7 then
        "Jul"
    else if i % 12 == 8 then
        "Aug"
    else if i % 12 == 9 then
        "Sep"
    else if i % 12 == 10 then
        "Oct"
    else
        "Nov"


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
            [ Svg.Attributes.style "text-align: center" ]
            [ issuesOverTimeBarChart model
              --, chartHtml payingCustomersOverTimeDataChart payingCustomersOverTimeData
              --, chartHtml issuesOverTimeChart issuesOver12Months
            ]


w : Float
w =
    500


h : Float
h =
    0.75 * w


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

        -- Append dummy data, 0, to get our axis looking right
        data =
            (\issues currentDate -> List.indexedMap (,) (0 :: issues))
                <$> (issuesOver12Months |> R.mapError toString)
                <*> (currentDate |> Result.fromMaybe "" >> R.fromResult)
                |> R.withDefault []

        xScale : ContinuousScale
        xScale =
            Scale.linear
                ( 0, (toFloat << List.length) data )
                ( 0, w - 2 * padding )

        maxY =
            List.map Tuple.second data
                |> List.maximum
                |> Maybe.withDefault 10
                |> (*) 1.1

        yScale : ContinuousScale
        yScale =
            Scale.linear ( 0, maxY ) ( h - 2 * padding, 0 )

        opts : Axis.Options a
        opts =
            Axis.defaultOptions

        xAxis : Svg msg
        xAxis =
            Axis.axis
                { opts
                    | orientation = Axis.Bottom
                    , tickCount = List.length data
                    , tickFormat = Just (truncate >> toMonth)
                }
                xScale

        yAxis : Svg msg
        yAxis =
            Axis.axis { opts | orientation = Axis.Left } yScale

        barGenerator : ( Int, Float ) -> Path.Path -> Path.Path
        barGenerator ( x, y ) =
            let
                rw =
                    30

                -- Left
                rx =
                    (Scale.convert xScale (toFloat x)) - (rw / 2)

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
            [ width (toString w ++ "px"), height (toString h ++ "px"), Svg.Attributes.style "padding: 8px" ]
            [ g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (h - padding) ++ ")") ]
                [ xAxis ]
            , g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString padding ++ ")") ]
                [ yAxis ]
            , g [ transform ("translate(" ++ toString padding ++ ", " ++ toString padding ++ ")") ]
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
