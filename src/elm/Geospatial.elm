module Geospatial exposing (..)

import Dict as D


type alias Model =
    D.Dict String Location


type alias Location =
    { latitude : Float
    , longitude : Float
    , employeeCount : Int
    }


init : Model
init =
    D.empty
