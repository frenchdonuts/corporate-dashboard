module Main exposing (..)

import Geospatial as G
import KeyMetrics as K
import Issues as I
import Html as H
import Material
import Material.Layout as Layout
import Navigation
import UrlParser exposing (Parser, top, s, oneOf, map, parsePath)


-- TOP-LEVEL (where all the routing takes place)


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Page
    = Home
    | Geospatial
    | KeyMetrics
    | Issues


type alias Model =
    { history : List (Maybe Page)
    , mdl : Material.Model
    , geospatial : G.Model
    , keyMetrics : K.Model
    , issues : I.Model
    }


type Msg
    = UrlChange Navigation.Location
    | Mdl (Material.Msg Msg)


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    { history = [ parsePath route location ]
    , mdl = Material.model
    , geospatial = G.init
    , keyMetrics = K.init
    , issues = I.init
    }
        ! [ Layout.sub0 Mdl ]


route : Parser (Page -> a) a
route =
    oneOf
        [ map Home top
        , map Geospatial (s "geospatial")
        , map KeyMetrics (s "keymetrics")
        , map Issues (s "issues")
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Layout.subs Mdl model.mdl
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange navLocation ->
            { model | history = parsePath route navLocation :: model.history }
                ! []

        Mdl msg ->
            Material.update msg model


view : Model -> H.Html Msg
view model =
    let
        { history, mdl, geospatial, keyMetrics, issues } =
            model

        currentPage =
            case history of
                maybePage :: history ->
                    Maybe.withDefault Home maybePage

                [] ->
                    Home
    in
        H.div [] []
