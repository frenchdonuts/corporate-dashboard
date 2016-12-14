module Main exposing (..)

import Home as H
import Geospatial as G
import KeyMetrics as K
import Issues as I
import Html as H
import Material
import Material.Layout as L
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
    , home : H.Model
    , geospatial : G.Model
    , keyMetrics : K.Model
    , issues : I.Model
    }


type Msg
    = NewUrl String
    | UrlChange Navigation.Location
    | Mdl (Material.Msg Msg)


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    { history = [ parsePath route location ]
    , mdl = Material.model
    , home = H.init
    , geospatial = G.init
    , keyMetrics = K.init
    , issues = I.init
    }
        ! [ L.sub0 Mdl ]


route : Parser (Page -> a) a
route =
    oneOf
        [ map Home top
        , map Geospatial (s "geospatial")
        , map KeyMetrics (s "keymetrics")
        , map Issues (s "issues")
        ]


pageToString : Page -> String
pageToString p =
    case p of
        Home ->
            ""

        Geospatial ->
            "geospatial"

        KeyMetrics ->
            "keymetrics"

        Issues ->
            "issues"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ L.subs Mdl model.mdl
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUrl url ->
            ( model, Navigation.newUrl url )

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
