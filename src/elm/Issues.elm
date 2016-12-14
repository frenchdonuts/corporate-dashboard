module Issues exposing (..)

import Time.DateTime as T


type alias Model =
    List Issue


type alias Issue =
    { submissionTimestamp : T.DateTime
    , customerName : String
    , customerEmailAddress : String
    , description : String
    , open : Bool
    , closedTimestamp : T.DateTime
    , employeeName : String
    }


init : Model
init =
    []
