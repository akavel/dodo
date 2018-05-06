module Main exposing (..)

import Html exposing (Html, text, div, h1, img, button, input)
import Html.Attributes exposing (src, placeholder)
import Html.Events exposing (onInput)


---- MODEL ----


type alias Model =
    { text : String
    }


init : ( Model, Cmd Msg )
init =
    ( { text = "" }, Cmd.none )



---- UPDATE ----


type Msg =
    Change String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let newModel = case msg of
        Change newText ->
            { model | text = newText }
    in ( newModel, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Your text", onInput Change ] []
        , div [] [ text (String.reverse model.text) ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
