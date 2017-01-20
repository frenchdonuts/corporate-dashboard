module KeyMetrics exposing (..)

import Html as H exposing (Html)
import Chart exposing (lChart, vBar, hBar, title, updateStyles, toHtml)
import Visualization.Scale as Scale exposing (ContinuousScale, ContinuousTimeScale)
import Visualization.Axis as Axis
import Visualization.List as List
import Visualization.Shape as Shape
import Visualization.Path as Path
import Svg exposing (..)
import Svg.Attributes as SA exposing (..)
import Http
import Json.Decode as J
import RemoteData as R
import RemoteData.Infix exposing (..)
import Task
import Date exposing (Date)
import Misc
import Window
import Material.Grid as Grid
import Material.Typography as Typo
import Material.Options as O
import Data.Issue exposing (Issue)


type alias Model =
    { payingCustomersOver12Months : R.RemoteData Http.Error (List Float)
    , issuesOver12Months : R.RemoteData Http.Error (List Float)
    , currentDate : Maybe Date.Date
    , windowSize : Window.Size
    }


type Msg
    = NoOp
    | DataFetched (R.RemoteData Http.Error ( List Float, List Float ))
    | CurrentTimeFetched Date.Date
    | OnWindowResize Window.Size


init : ( Model, Cmd Msg )
init =
    { payingCustomersOver12Months = R.NotAsked
    , issuesOver12Months = R.NotAsked
    , currentDate = Nothing
    , windowSize = { width = 0, height = 0 }
    }
        ! [ fetchData
          , Task.perform CurrentTimeFetched Date.now
          , Task.perform OnWindowResize Window.size
          ]


subscriptions : Sub Msg
subscriptions =
    Window.resizes OnWindowResize


fetchData : Cmd Msg
fetchData =
    let
        decoder =
            J.map2 (,)
                (J.field "payingCustomersOverTimeData" (J.list J.float))
                (J.field "issuesOverTimeData" (J.list J.float))
    in
        Http.get "/static/key_metrics.json" decoder
            |> Http.send (R.fromResult >> DataFetched)


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        DataFetched remoteData ->
            let
                payingCustomersOverTime =
                    R.map Tuple.first remoteData

                issuesOverTime =
                    R.map Tuple.second remoteData
            in
                { model
                    | payingCustomersOver12Months = payingCustomersOverTime
                    , issuesOver12Months = issuesOverTime
                }

        CurrentTimeFetched date ->
            { model | currentDate = Just date }

        OnWindowResize size ->
            { model | windowSize = size }


view : R.RemoteData String (List Issue) -> Model -> Html Msg
view remoteDataIssues model =
    let
        { payingCustomersOver12Months, issuesOver12Months } =
            model

        currentIssuesCount =
            R.map List.length remoteDataIssues
                |> R.withDefault 0
    in
        Grid.grid
            []
            [ Grid.cell
                [ Grid.size Grid.Desktop 8
                , Grid.size Grid.Tablet 8
                , Typo.display1
                , O.css "margin-top" "32px"
                , O.css "margin-bottom" "32px"
                ]
                [ H.text ("Number of current issues: " ++ toString currentIssuesCount) ]
            , Grid.cell
                [ Grid.size Grid.Desktop 8
                , Grid.size Grid.Tablet 8
                , Typo.display1
                , O.css "margin-bottom" "-32px"
                ]
                [ H.text "Issues Over Time" ]
            , Grid.cell
                [ Grid.size Grid.Desktop 8
                , Grid.offset Grid.Desktop 2
                , Grid.size Grid.Tablet 8
                ]
                [ issuesBarChart model ]
            , Grid.cell
                [ Grid.size Grid.Desktop 8
                , Grid.size Grid.Tablet 8
                , Typo.display1
                , O.css "margin-bottom" "-32px"
                ]
                [ H.text "Paying Customers Over Time" ]
            , Grid.cell
                [ Grid.size Grid.Desktop 8
                , Grid.offset Grid.Desktop 2
                , Grid.size Grid.Tablet 8
                ]
                [ payingCustomersLineGraph model ]
            ]


issuesBarChart : Model -> Html Msg
issuesBarChart model =
    let
        { issuesOver12Months, currentDate, windowSize } =
            model

        ( w, h ) =
            chartDimensions (toFloat windowSize.width)

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
                ( 0, w - 2 * chartPadding )

        maxY =
            List.map Tuple.second data
                |> List.maximum
                |> Maybe.withDefault 10
                |> (*) 1.1

        yScale : ContinuousScale
        yScale =
            Scale.linear ( 0, maxY ) ( h - 2 * chartPadding, 0 )

        opts : Axis.Options a
        opts =
            Axis.defaultOptions

        xAxis : Svg msg
        xAxis =
            Axis.axis
                { opts
                    | orientation = Axis.Bottom
                    , tickCount = List.length data
                    , tickFormat = Just (truncate >> tickString)
                }
                xScale

        tickString i =
            if i == 0 then
                ""
            else
                toMonth i

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
            [ width (toString w ++ "px"), height (toString h ++ "px") ]
            [ g [ transform ("translate(" ++ toString (chartPadding - 1) ++ ", " ++ toString (h - chartPadding) ++ ")") ]
                [ xAxis ]
            , g [ transform ("translate(" ++ toString (chartPadding - 1) ++ ", " ++ toString chartPadding ++ ")") ]
                [ yAxis ]
            , g [ transform ("translate(" ++ toString chartPadding ++ ", " ++ toString chartPadding ++ ")") ]
                [ Svg.path
                    [ d bars, stroke "none", strokeWidth "3px", fill "rgba(255, 0, 0, 0.54)" ]
                    []
                ]
            ]


payingCustomersLineGraph : Model -> Html Msg
payingCustomersLineGraph model =
    let
        { payingCustomersOver12Months, currentDate, windowSize } =
            model

        ( w, h ) =
            chartDimensions (toFloat windowSize.width)

        dataTransform i x =
            ( toFloat i, x )

        data =
            (\payingCustomers currentDate -> List.indexedMap dataTransform payingCustomers)
                <$> (payingCustomersOver12Months |> R.mapError toString)
                <*> (currentDate |> Result.fromMaybe "" >> R.fromResult)
                |> R.withDefault []

        xScale : ContinuousScale
        xScale =
            Scale.linear
                ( 0, (toFloat << List.length) data )
                ( 0, w - 2 * chartPadding )

        maxY =
            List.map Tuple.second data
                |> List.maximum
                |> Maybe.withDefault 10
                |> (*) 1.1

        yScale : ContinuousScale
        yScale =
            Scale.linear ( 0, maxY ) ( h - 2 * chartPadding, 0 )

        opts : Axis.Options a
        opts =
            Axis.defaultOptions

        xAxis : Svg msg
        xAxis =
            Axis.axis
                { opts
                    | orientation = Axis.Bottom
                    , tickCount = List.length data
                    , tickFormat = Just (truncate >> (+) 1 >> toMonth)
                }
                xScale

        tickString i =
            if i == 0 then
                ""
            else
                toMonth i

        yAxis : Svg msg
        yAxis =
            Axis.axis { opts | orientation = Axis.Left } yScale

        areaGenerator : ( Float, Float ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
        areaGenerator ( x, y ) =
            Just ( ( Scale.convert xScale x, Tuple.first (Scale.rangeExtent yScale) ), ( Scale.convert xScale x, Scale.convert yScale y ) )

        lineGenerator : ( Float, Float ) -> Maybe ( Float, Float )
        lineGenerator ( x, y ) =
            Just ( Scale.convert xScale x, Scale.convert yScale y )

        area : String
        area =
            List.map areaGenerator data
                |> Shape.area Shape.monotoneInXCurve

        line : String
        line =
            List.map lineGenerator data
                |> Shape.line Shape.monotoneInXCurve
    in
        svg [ width (toString w ++ "px"), height (toString h ++ "px") ]
            [ g [ transform ("translate(" ++ toString (chartPadding - 1) ++ ", " ++ toString (h - chartPadding) ++ ")") ]
                [ xAxis ]
            , g [ transform ("translate(" ++ toString (chartPadding - 1) ++ ", " ++ toString chartPadding ++ ")") ]
                [ yAxis ]
            , g [ transform ("translate(" ++ toString chartPadding ++ ", " ++ toString chartPadding ++ ")"), class "series" ]
                [ Svg.path [ d area, stroke "none", strokeWidth "3px", fill "rgba(255, 0, 0, 0.54)" ] []
                , Svg.path [ d line, stroke "red", strokeWidth "3px", fill "none" ] []
                ]
            ]


maxChartWidth : Float
maxChartWidth =
    600


chartPadding : Float
chartPadding =
    38


chartDimensions : Float -> ( Float, Float )
chartDimensions windowWidth =
    let
        w =
            Basics.min windowWidth maxChartWidth

        h =
            0.75 * w
    in
        ( w, h )


toMonth : Int -> String
toMonth i =
    if i % 12 == 0 then
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
