port module Main exposing (..)

import Geospatial as G
import KeyMetrics as K
import Issues as I
import Data.Issue as DI exposing (Issue)
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
    , geospatial : G.Model
    , keyMetrics : K.Model
    , issuesPage : I.Model
    , issues : R.RemoteData String (List Issue)
    }


type Page
    = Geospatial
    | KeyMetrics
    | Issues


type Msg
    = NewUrl String
    | UrlChange Navigation.Location
    | Mdl (Material.Msg Msg)
    | GeospatialMsg G.Msg
    | KeyMetricsMsg K.Msg
    | IssuesMsg I.Msg
    | IssuesDataFetched (R.RemoteData String (List Issue))
    | NewIssue Issue


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        ( geospatialInitModel, geospatialInitCmd ) =
            G.init

        ( keyMetricsInitModel, keyMetricsInitCmd ) =
            K.init

        homePage =
            parsePath route location

        fetchIssuesCmd =
            if homePage == Just KeyMetrics || homePage == Just Issues then
                Cmd.map IssuesDataFetched DI.getIssuesCmd
            else
                Cmd.none
    in
        { history =
            [ homePage ]
        , mdl = Material.model
        , geospatial = geospatialInitModel
        , keyMetrics = keyMetricsInitModel
        , issuesPage = I.init
        , issues = R.NotAsked
        }
            ! [ L.sub0 Mdl
              , Cmd.map GeospatialMsg geospatialInitCmd
              , Cmd.map KeyMetricsMsg keyMetricsInitCmd
              , fetchIssuesCmd
              ]


route : Parser (Page -> a) a
route =
    oneOf
        [ map Geospatial (s "geospatial")
        , map KeyMetrics (s "keymetrics")
        , map Issues (s "issues")
        ]


pageToString : Page -> String
pageToString p =
    case p of
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
        , Sub.map GeospatialMsg G.subscriptions
        , Sub.map KeyMetricsMsg K.subscriptions
        , issues NewIssue
        ]


port issues : (Issue -> msg) -> Sub msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GeospatialMsg msg ->
            let
                ( geospatialModel, geospatialCmd ) =
                    G.update msg model.geospatial
            in
                { model | geospatial = geospatialModel } ! [ Cmd.map GeospatialMsg geospatialCmd ]

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

        NewIssue issue ->
            { model | issues = R.map (\issues -> issue :: issues) model.issues } ! []

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
        { history, mdl, geospatial, keyMetrics, issuesPage, issues } =
            model

        currentPage =
            case history of
                maybePage :: history ->
                    Maybe.withDefault Geospatial maybePage

                [] ->
                    Geospatial

        mainView =
            case currentPage of
                Geospatial ->
                    H.map GeospatialMsg (G.view geospatial)

                KeyMetrics ->
                    H.map KeyMetricsMsg (K.view issues keyMetrics)

                Issues ->
                    H.map IssuesMsg (I.view issues issuesPage)
    in
        L.render Mdl
            mdl
            [ L.fixedHeader ]
            { header = header currentPage
            , drawer = drawer
            , tabs = ( [], [] )
            , main = [ mainView ]
            }


header : Page -> List (Html Msg)
header currentPage =
    let
        pageToString page =
            case page of
                Geospatial ->
                    "Geospatial Data"

                KeyMetrics ->
                    "Key Metrics"

                Issues ->
                    "Issues"
    in
        [ L.row
            [ O.css "transition" "height 333ms ease-in-out 0s"
            ]
            [ L.title [] [ H.text <| pageToString currentPage ]
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
