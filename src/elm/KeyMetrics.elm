module KeyMetrics exposing (..)


type alias Model =
    { openIssuesCount : Int
    , payingCustomersOverTimeData : List ( Float, String )
    , issuesOverTimeData : List ( Float, String )
    }


init : Model
init =
    { openIssuesCount = 0
    , payingCustomersOverTimeData = []
    , issuesOverTimeData = []
    }
