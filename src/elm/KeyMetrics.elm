module KeyMetrics exposing (..)

import Html as H exposing (Html)


type alias Model =
    { openIssuesCount : Int
    , payingCustomersOverTimeData : List ( Float, String )
    , issuesOverTimeData : List ( Float, String )
    }


type Msg
    = NoOp


init : Model
init =
    { openIssuesCount = 0
    , payingCustomersOverTimeData = []
    , issuesOverTimeData = []
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Msg
view model =
    let
        { openIssuesCount, payingCustomersOverTimeData, issuesOverTimeData } =
            model
    in
        H.div [] []
