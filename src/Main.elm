module Main exposing (..)

import Html exposing (Html, div, text, p, header, footer, main_)
import Html.Attributes exposing (class)
-- import Array exposing (Array)
-- debois/elm-mdl - Material Design Lite (MDL)
import Material
import Material.Scheme
import Material.Options as Options
import Material.Color as Color
import Material.Textfield as Textfield
import Material.Typography as Typo
import Material.List as Lists
import Material.Helpers exposing (pure)


---- MODEL ----


type alias Model =
    { text : String
    -- , checklists : List Checklist
    , checklist : Checklist
    , mdl : Material.Model  -- MDL boilerplate
    }
type alias Checklist =
    { name : String
    , tasks : List Task
    }
type alias Task =
    { text : String
    , done : Bool
    }


model : Model
model =
    { text = ""
    , checklist = Checklist "New List 0"
        [ Task "Foo" False
        , Task "Bar" True
        ]
    , mdl = Material.model  -- MDL boilerplate
    }


init : ( Model, Cmd Msg )
init = ( model, Cmd.none )


---- UPDATE ----


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
type Msg
    = Change String
    -- = LoadFromStorage
    -- | AppendTask String
    -- | ToggleTask Int
    -- | DeleteTask Int
    | Mdl (Material.Msg Msg)  -- MDL boilerplate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newText ->
            pure { model | text = newText }
        Mdl msg_ ->
            Material.update Mdl msg_ model



---- VIEW ----

type alias Mdl = Material.Model

viewTask idx submodel =
    let
        color =
            if submodel.done
            then Color.color Color.Grey Color.S300
            else Color.black
    in Lists.li
        [ ]
        -- [ Options.css "border-bottom" "1px solid #000 !important"
        -- ]
        -- TODO(akavel): somehow add dividers, maybe in similar way as in:
        -- https://github.com/google/material-design-lite/pull/1785/files
        [ Lists.content
            -- [ Color.text (Color.color Color.Grey Color.S300) ]
            -- [ Color.text Color.black ]
            [ Color.text color
            -- , Options.css "border-bottom" "1px solid rgba(#000, 0.12) !important"
            -- , Options.css "border-bottom" "1px solid #000 !important"
            ]
            -- []
            [ text (submodel.text) ]
        ]

viewMain : Model -> Html Msg
viewMain model =
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
        , Lists.ul []
            (List.indexedMap viewTask model.checklist.tasks)
        ]

view : Model -> Html Msg
view model =
    div [ class "site mdl-layout mdl-js-layout" ]
        [ header [ class "mdl-layout__header mdl-layout__header--waterfall"]
            [ div [ class "mdl-layout__header-row" ]
                [ text "hello header" ]
            ]
        , main_ [ class "mdl-layout__content" ]
            [ text "hello content" ]
            -- [ viewMain model ]
        , footer [ class "mdl-mini-footer" ]
            [ text "hello footer" ]
        , div [ class "mdl-layout__obfuscator" ] []
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
