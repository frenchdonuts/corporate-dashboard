module Home exposing (..)

import Html as H


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


home : Model -> H.Html Msg
home model =
    let
        { username, password } =
            model
    in
        H.div [] []
