module Issues exposing (..)

import Time.DateTime as T
import Html as H exposing (Html)


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


type Msg
    = NoOp


init : Model
init =
    []


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Msg
view model =
    H.div [] []


issueView : Issue -> Html Msg
issueView issue =
    let
        { submissionTimestamp, customerName, customerEmailAddress, description, open, closedTimestamp, employeeName } =
            issue
    in
        H.div [] []
