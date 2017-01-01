module Geospatial exposing (..)

import Dict as D
import Html as H exposing (Html, node)
import Html.Events as E
import Material.Grid as G
import Material.Typography as T
import Material.Options as O
import Material.Color as C
import Http
import Json.Decode as J


type alias Model =
    { hoveredRowIndex : Maybe Int
    , geospatialData : D.Dict String Location
    }


type alias Location =
    { latitude : Float
    , longitude : Float
    , employeeCount : Int
    }


type Msg
    = NoOp
    | GeospatialData (Result Http.Error (D.Dict String Location))
    | OnRowHover (Maybe Int)


init : ( Model, Cmd Msg )
init =
    Model Nothing D.empty ! [ Http.send GeospatialData fetchGeospatialData ]


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
            { model | geospatialData = Result.withDefault model.geospatialData fetchResult }

        OnRowHover maybeIndex ->
            { model | hoveredRowIndex = maybeIndex }


type Column
    = City
    | EmployeeCount


columnToString : Column -> String
columnToString column =
    case column of
        City ->
            "City"

        EmployeeCount ->
            "Employee Count"


view : Model -> Html Msg
view model =
    let
        columns =
            [ City
            , EmployeeCount
            ]

        spacer =
            G.cell
                [ G.size G.Desktop 12, G.size G.Tablet 8, G.size G.Phone 4 ]
                []

        margin =
            [ spacer, spacer, spacer ]
    in
        G.grid []
            (margin ++ header columns model ++ body columns model)


(+|+) : Bool -> List a -> List a -> List a
(+|+) append xs2 xs1 =
    if append then
        xs1 ++ xs2
    else
        xs1


cell : Maybe Int -> Bool -> Bool -> Bool -> String -> G.Cell Msg
cell i isFirstColumn isHighlighted isHeader text =
    let
        typography =
            if isHeader then
                T.body2
            else
                T.body1

        baseAttrs =
            [ G.size G.Desktop 4
            , G.size G.Tablet 4
            , G.size G.Phone 2
            , typography
            , T.justify
            , T.center
            , O.css "cursor" "pointer"
            , O.attribute <| E.onMouseEnter <| OnRowHover i
            , O.attribute <| E.onMouseLeave <| OnRowHover Nothing
            , O.when (C.background (C.color C.Grey C.S100)) isHighlighted
            ]

        firstColumnAttrs =
            [ G.offset G.Desktop 1
            , O.css "margin-top" "0"
            , O.css "margin-right" "0"
            , O.css "margin-bottom" "0"
            , O.css "padding-left" "8px"
            , O.css "padding-top" "8px"
            , O.css "padding-right" "8px"
            , O.css "padding-bottom" "8px"
            ]

        notFirstColumnAttrs =
            [ O.css "margin" "0"
            , O.css "padding" "8px"
            ]

        attrs =
            baseAttrs
                |> (+|+) isFirstColumn firstColumnAttrs
                |> (+|+) (not isFirstColumn) notFirstColumnAttrs
    in
        G.cell
            attrs
            [ H.text text ]


header : List Column -> Model -> List (G.Cell Msg)
header columns _ =
    let
        cell_ column =
            cell Nothing (isFirstColumn columns column) False True (columnToString column)
    in
        List.map cell_ columns


isFirstColumn : List Column -> Column -> Bool
isFirstColumn columns column =
    case columns of
        c :: cs ->
            c == column

        [] ->
            False


body : List Column -> Model -> List (G.Cell Msg)
body columns model =
    let
        { geospatialData } =
            model

        entries =
            D.toList geospatialData
    in
        indexedConcatMap (row columns model) entries


row : List Column -> Model -> Int -> ( String, Location ) -> List (G.Cell Msg)
row columns model i entry =
    let
        { hoveredRowIndex } =
            model

        isHighlighted =
            Maybe.map ((==) i) hoveredRowIndex |> Maybe.withDefault False

        cell_ column =
            cell (Just i) (isFirstColumn columns column) isHighlighted False (columnDataToString entry column)
    in
        List.map cell_ columns


columnDataToString : ( String, Location ) -> Column -> String
columnDataToString entry column =
    let
        ( city, { employeeCount } ) =
            entry
    in
        case column of
            City ->
                city

            EmployeeCount ->
                toString employeeCount


indexedConcatMap : (Int -> a -> List b) -> List a -> List b
indexedConcatMap f xs =
    List.concat (List.indexedMap f xs)
