module Main exposing (..)

import Html exposing (Html, div, text, p)
-- debois/elm-mdl - Material Design Lite (MDL)
import Material
import Material.Scheme
import Material.Options as Options
import Material.Textfield as Textfield
import Material.Typography as Typo
import Material.Helpers exposing (pure)


---- MODEL ----


type alias Model =
    { text : String
    , mdl : Material.Model  -- MDL boilerplate
    }


model : Model
model =
    { text = ""
    , mdl = Material.model  -- MDL boilerplate
    }


init : ( Model, Cmd Msg )
init = ( model, Cmd.none )


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


type Msg
    = Change String
    | Mdl (Material.Msg Msg)  -- MDL boilerplate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newText ->
            pure { model | text = newText }
        Mdl msg_ ->
            Material.update Mdl msg_ model

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

type alias Mdl = Material.Model

view : Model -> Html Msg
view model =
    div []
        [ Textfield.render
            Mdl [0] model.mdl  -- MDL boilerplate
            [ Textfield.label "Your text"
            , Textfield.floatingLabel
            , Options.onInput Change
            ] []
        , Options.styled p
            [ Typo.headline ]
            [ text (String.reverse model.text) ]
        -- [ input [ placeholder "Your text", onInput Change ] []
        -- , div [] [ text (String.reverse model.text) ]
        ]
    |> Material.Scheme.top



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
