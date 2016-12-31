module Geospatial exposing (..)

import Dict as D
import Html as H exposing (Html, node)
import Material.Grid as G
import Http
import Json.Decode as J


type alias Model =
    D.Dict String Location


type alias Location =
    { latitude : Float
    , longitude : Float
    , employeeCount : Int
    }


type Msg
    = NoOp
    | GeospatialData (Result Http.Error (D.Dict String Location))


init : ( Model, Cmd Msg )
init =
    D.empty ! [ Http.send GeospatialData fetchGeospatialData ]


fetchGeospatialData : Http.Request (D.Dict String Location)
fetchGeospatialData =
    Http.get "http://localhost:8080/static/geospatial.json" geospatialDecoder


geospatialDecoder : J.Decoder (D.Dict String Location)
geospatialDecoder =
    let
        locationDecoder =
            J.map3 Location
                (J.field "latitude" J.float)
                (J.field "longitude" J.float)
                (J.field "employeeCount" J.int)
    in
        J.dict locationDecoder


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        GeospatialData fetchResult ->
            Result.withDefault model (Debug.log "fetchResult" fetchResult)


view : Model -> Html Msg
view model =
    H.div []
        [ H.text (toString model) ]
