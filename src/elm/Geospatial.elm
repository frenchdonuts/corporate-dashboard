module Geospatial exposing (..)

import Dict as D
import Html as H exposing (Html)
import Html.Attributes as A exposing (attribute)
import Html.Events as E
import Material.Grid as G
import Material.Typography as T
import Material.Options as O
import Material.Color as C
import Material.Icon as I
import Http
import Json.Decode as J


type alias Model =
    { hoveredRowIndex : Maybe Int
    , geospatialData : D.Dict String Location
    , selectedCity : Maybe String
    , selectedColumn : Column
    , isAscending : Bool
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
    | OnClickHeader Column
    | OnClickRow String


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


init : ( Model, Cmd Msg )
init =
    Model Nothing D.empty Nothing City True ! [ Http.send GeospatialData fetchGeospatialData ]


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
    let
        { hoveredRowIndex, geospatialData, selectedColumn, isAscending, selectedCity } =
            model
    in
        case msg of
            NoOp ->
                model

            GeospatialData fetchResult ->
                { model | geospatialData = Result.withDefault model.geospatialData fetchResult }

            OnRowHover maybeIndex ->
                { model | hoveredRowIndex = maybeIndex }

            OnClickHeader column ->
                let
                    sortOrderIsAscending =
                        if selectedColumn == column then
                            (not isAscending)
                        else
                            True
                in
                    { model
                        | isAscending = sortOrderIsAscending
                        , selectedColumn = column
                    }

            OnClickRow clickedCity ->
                let
                    selectedCity_ =
                        case selectedCity of
                            Nothing ->
                                Just clickedCity

                            Just city ->
                                if city == clickedCity then
                                    Nothing
                                else
                                    Just clickedCity
                in
                    { model | selectedCity = selectedCity_ }


view : Model -> Html Msg
view model =
    let
        columns =
            [ City
            , EmployeeCount
            ]

        map =
            G.cell
                [ G.size G.Desktop 12
                , G.size G.Tablet 8
                , G.size G.Phone 4
                , O.css "height" "400px"
                ]
                [ googleMap model ]
    in
        G.grid
            [ O.css "margin-top" "48px" ]
            ([ map ] ++ header columns model ++ body columns model)


googleMap : Model -> Html Msg
googleMap model =
    let
        { geospatialData, selectedCity } =
            model

        zoom =
            case selectedCity of
                Just _ ->
                    "4"

                Nothing ->
                    ""

        mapMarkers =
            selectedCity
                |> Maybe.andThen (\city -> Maybe.map ((,) city) (D.get city geospatialData))
                |> Maybe.map (flip (::) [])
                |> Maybe.withDefault (D.toList geospatialData)
                |> List.map toMarker

        toMarker ( city, location ) =
            googleMapMarker city location
    in
        H.node "google-map"
            [ attribute "fit-to-markers" ""
            , attribute "disable-default-ui" ""
            , attribute "disable-zoom" ""
            , attribute "zoom" zoom
            ]
            mapMarkers


googleMapMarker : String -> Location -> Html Msg
googleMapMarker city location =
    let
        { latitude, longitude, employeeCount } =
            location

        infoPane =
            H.span []
                [ H.text <| (toString employeeCount) ++ " employees"
                ]
    in
        H.node "google-map-marker"
            [ attribute "latitude" (toString latitude)
            , attribute "longitude" (toString longitude)
            , attribute "title" city
            ]
            [ infoPane ]


header : List Column -> Model -> List (G.Cell Msg)
header columns model =
    let
        { selectedColumn, isAscending } =
            model

        cell_ column =
            headerCell
                (isFirstColumn columns column)
                (selectedColumn == column)
                isAscending
                (OnClickHeader column)
                (columnToString column)
    in
        List.map cell_ columns


headerCell : Bool -> Bool -> Bool -> Msg -> String -> G.Cell Msg
headerCell isFirstColumn isSelected isAscending onClickMsg text =
    let
        baseAttrs =
            [ G.size G.Desktop 4
            , G.size G.Tablet 4
            , G.size G.Phone 2
            , T.body2
            , T.justify
            , T.center
            , O.css "cursor" "pointer"
            , O.attribute <| E.onClick onClickMsg
            ]

        firstColumnAttrs =
            [ G.offset G.Desktop 2
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

        sortIcon =
            if isAscending then
                I.i "arrow_downward"
            else
                I.i "arrow_upward"

        children =
            [ H.text text ]
                |> (+|+) isSelected [ sortIcon ]
    in
        G.cell
            attrs
            children


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
        { geospatialData, selectedColumn, isAscending } =
            model

        invert c =
            if isAscending then
                c
            else
                case c of
                    GT ->
                        LT

                    EQ ->
                        EQ

                    LT ->
                        GT

        sorter column p1 p2 =
            case column of
                City ->
                    invert <| compare (Tuple.first p1) (Tuple.first p2)

                EmployeeCount ->
                    invert <| compare (.employeeCount <| Tuple.second p1) (.employeeCount <| Tuple.second p2)

        entries =
            List.sortWith (sorter selectedColumn) (D.toList geospatialData)
    in
        indexedConcatMap (row columns model) entries


row : List Column -> Model -> Int -> ( String, Location ) -> List (G.Cell Msg)
row columns model i entry =
    let
        { hoveredRowIndex, selectedCity } =
            model

        ( city, _ ) =
            entry

        isHighlighted =
            Maybe.map ((==) i) hoveredRowIndex
                |> Maybe.withDefault False
                |> (||) (Maybe.map ((==) city) selectedCity |> Maybe.withDefault False)

        cell_ column =
            rowCell (Just i) (isFirstColumn columns column) isHighlighted (OnClickRow city) (columnDataToString entry column)
    in
        List.map cell_ columns


rowCell : Maybe Int -> Bool -> Bool -> Msg -> String -> G.Cell Msg
rowCell i isFirstColumn isHighlighted onClickMsg text =
    let
        baseAttrs =
            [ G.size G.Desktop 4
            , G.size G.Tablet 4
            , G.size G.Phone 2
            , T.body1
            , T.justify
            , T.center
            , O.css "cursor" "pointer"
            , O.attribute <| E.onClick onClickMsg
            , O.attribute <| E.onMouseEnter <| OnRowHover i
            , O.attribute <| E.onMouseLeave <| OnRowHover Nothing
            , O.when (C.background (C.color C.Grey C.S100)) isHighlighted
            ]

        firstColumnAttrs =
            [ G.offset G.Desktop 2
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


(+|+) : Bool -> List a -> List a -> List a
(+|+) append xs2 xs1 =
    if append then
        xs1 ++ xs2
    else
        xs1
