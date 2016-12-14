module Home exposing (..)

import Html as H exposing (Html)


type alias Model =
    { username : String
    , password : String
    }


type Msg
    = SetUsername String
    | SetPassword String


init : Model
init =
    { username = ""
    , password = ""
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetUsername name ->
            { model | username = name }

        SetPassword password ->
            { model | password = password }


view : Model -> H.Html Msg
view model =
    let
        { username, password } =
            model
    in
        H.div [] []
