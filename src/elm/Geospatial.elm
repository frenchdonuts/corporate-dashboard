module Geospatial exposing (..)

import Dict as D
import Html as H exposing (Html)


type alias Model =
    D.Dict String Location


type alias Location =
    { latitude : Float
    , longitude : Float
    , employeeCount : Int
    }


type Msg
    = NoOp


init : Model
init =
    D.empty


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Msg
view model =
    H.div [] []
