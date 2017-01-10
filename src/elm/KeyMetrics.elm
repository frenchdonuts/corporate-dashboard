module KeyMetrics exposing (..)

import Html as H exposing (Html)
import Chart exposing (lChart, vBar, title, toHtml)
import Http
import Json.Decode as J
import RemoteData as R


type alias Model =
    { payingCustomersOverTimeData : List ( Float, String )
    , issuesOverTimeData : R.RemoteData Http.Error (List ( Float, String ))
    }


type Msg
    = NoOp
    | IssuesOverTimeDataFetched (R.RemoteData Http.Error (List Float))


init : ( Model, Cmd Msg )
init =
    { payingCustomersOverTimeData = []
    , issuesOverTimeData = R.NotAsked
    }
        ! [ fetchIssuesOverTimeData ]


fetchIssuesOverTimeData : Cmd Msg
fetchIssuesOverTimeData =
    let
        decoder =
            J.field "issuesOverTimeData" (J.list J.float)
    in
        Http.get "http://localhost:8080/static/key_metrics.json" decoder
            |> Http.send (R.fromResult >> IssuesOverTimeDataFetched)


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        IssuesOverTimeDataFetched remoteData ->
            { model | issuesOverTimeData = R.map (List.indexedMap (\i f -> ( f, toMonth i ))) remoteData }


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

        issuesOverTimeChart =
            R.map (vBar >> title "Issues over time" >> toHtml) issuesOverTimeData
                |> R.withDefault defaultHtml
    in
        H.div
            []
            [ issuesOverTimeChart ]
