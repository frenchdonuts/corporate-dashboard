module Main exposing (..)

import Home as H
import Geospatial as G
import KeyMetrics as K
import Issues as I
import Data.Issue as DI
import Html as H exposing (Html)
import Material
import Material.Layout as L
import Material.Options as O
import Navigation
import UrlParser exposing (Parser, top, s, oneOf, map, parsePath)
import RemoteData as R


-- TOP-LEVEL (where all the routing takes place)


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { history : List (Maybe Page)
    , mdl : Material.Model
    , home : H.Model
    , geospatial : G.Model
    , keyMetrics : K.Model
    , issuesPage : I.Model
    , issues : R.RemoteData String (List DI.Issue)
    }


type Page
    = Home
    | Geospatial
    | KeyMetrics
    | Issues


type Msg
    = NewUrl String
    | UrlChange Navigation.Location
    | Mdl (Material.Msg Msg)
    | HomeMsg H.Msg
    | GeospatialMsg G.Msg
    | KeyMetricsMsg K.Msg
    | IssuesMsg I.Msg
    | IssuesDataFetched (R.RemoteData String (List DI.Issue))


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        ( geospatialInitModel, geospatialInitCmd ) =
            G.init
    in
        { history =
            [ Just KeyMetrics ]
            --parsePath route location ]
        , mdl = Material.model
        , home = H.init
        , geospatial = geospatialInitModel
        , keyMetrics = K.init
        , issuesPage = I.init
        , issues = R.NotAsked
        }
            ! [ L.sub0 Mdl
              , Cmd.map GeospatialMsg geospatialInitCmd
              ]


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
        HomeMsg msg ->
            { model | home = H.update msg model.home } ! []

        GeospatialMsg msg ->
            { model | geospatial = G.update msg model.geospatial } ! []

        KeyMetricsMsg msg ->
            { model | keyMetrics = K.update msg model.keyMetrics }
                ! []

        IssuesMsg msg ->
            let
                ( issuesPage_, cmd ) =
                    I.update msg model.issuesPage
            in
                { model | issuesPage = issuesPage_ }
                    ! [ Cmd.map IssuesMsg cmd
                      ]

        IssuesDataFetched remoteData ->
            { model | issues = remoteData } ! []

        NewUrl url ->
            ( model, Navigation.newUrl url )

        UrlChange navLocation ->
            let
                newPage =
                    parsePath route navLocation

                fetchIssuesCmd =
                    if (newPage == Just KeyMetrics || newPage == Just Issues) && (model.issues |> R.isNotAsked) then
                        Cmd.map IssuesDataFetched DI.getIssuesCmd
                    else
                        Cmd.none
            in
                { model | history = newPage :: model.history }
                    ! [ fetchIssuesCmd ]

        Mdl msg ->
            Material.update msg model


view : Model -> Html Msg
view model =
    let
        { history, mdl, home, geospatial, keyMetrics, issuesPage, issues } =
            model

        currentPage =
            case history of
                maybePage :: history ->
                    Maybe.withDefault Home maybePage

                [] ->
                    Home

        mainView =
            case currentPage of
                Home ->
                    H.map HomeMsg (H.view home)

                Geospatial ->
                    H.map GeospatialMsg (G.view geospatial)

                KeyMetrics ->
                    H.map KeyMetricsMsg (K.view keyMetrics)

                Issues ->
                    H.map IssuesMsg (I.view issues issuesPage)
    in
        L.render Mdl
            mdl
            [ L.fixedDrawer ]
            { header = header
            , drawer = drawer
            , tabs = ( [], [] )
            , main = [ mainView ]
            }


header : List (Html Msg)
header =
    [ L.row
        [ O.css "transition" "height 333ms ease-in-out 0s"
        ]
        [ L.title [] [ H.text "Massive Corp. Inc." ]
        , L.spacer
        , L.navigation []
            [ L.link
                [ L.href "https://github.com/frenchdonuts/corporate-dashboard" ]
                [ H.span [] [ H.text "github" ] ]
            ]
        ]
    ]


drawer : List (Html Msg)
drawer =
    [ L.title [] [ H.text "Dashboards" ]
    , L.navigation
        []
        [ L.link
            [ L.onClick (NewUrl "/geospatial") ]
            [ H.text "Geospatial" ]
        , L.link
            [ L.onClick (NewUrl "/keymetrics") ]
            [ H.text "Key Metrics" ]
        , L.link
            [ L.onClick (NewUrl "/issues") ]
            [ H.text "Issues" ]
        ]
    ]
