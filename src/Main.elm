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


type alias DodoModel =
    List Checklist
type alias Checklist =
    { name : String
    , tasks : List Task
    }
type alias Task =
    { text : String
    , done : Bool
    }

---- UPDATE ----


type Msg =
    Change String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let newModel = case msg of
        Change newText ->
            { model | text = newText }
    in ( newModel, Cmd.none )

{-
We need to be able to:
- add new items to the current list
- remove item from current list
- disable/reenable item from current list (toggle)
- edit an item on the list
- TODO: clear (hide) all disabled items
- TODO: move items around the list
- TODO: show names of all lists
- TODO: change the current list to a different one
- TODO: rename current list
- TODO: delete a list
- TODO: add new list
- TODO: copy (duplicate) a list with a new name
-}
type DodoMsg
    = LoadFromStorage
    | AppendTask String
    | ToggleTask Int
    | DeleteTask Int


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
