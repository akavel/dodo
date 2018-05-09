module Main exposing (..)

import Html exposing (Html, div, text, p, header, footer, main_)
import Html.Attributes exposing (class)
import Html.Events
-- import Array exposing (Array)
-- debois/elm-mdl - Material Design Lite (MDL)
import Material
import Material.Dialog as Dialog
import Material.Scheme
import Material.Options as Options
import Material.Color as Color
import Material.Textfield as Textfield
import Material.Button as Button
import Material.Icon as Icon
import Material.Typography as Typo
import Material.List as Lists
import Material.Helpers exposing (pure)
import Focus exposing (..)
import String


---- MODEL ----


type alias Model =
    { text : String
    -- , checklists : List Checklist
    , checklist : Checklist
    , newTask : String
    , mdl : Material.Model  -- MDL boilerplate
    }
checklist : Focus { r | checklist : a } a
checklist = Focus.create .checklist (\f r -> { r | checklist = f r.checklist })

type alias Checklist =
    { name : String
    , tasks : List Task
    }
tasks : Focus { r | tasks : a } a
tasks = Focus.create .tasks (\f r -> { r | tasks = f r.tasks })

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
    , newTask = ""
    , mdl = Material.model  -- MDL boilerplate
    }


init : ( Model, Cmd Msg )
init = ( model, Cmd.none )


---- UPDATE ----


{-
We need to be able to:
- (done) add new items to the current list
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
    -- = LoadFromStorage
    = EditNewTask String
    | AppendTask
    | EditTask Int
    -- | ToggleTask Int
    -- | DeleteTask Int
    | Mdl (Material.Msg Msg)  -- MDL boilerplate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditNewTask newText ->
            pure { model | newTask = newText }
        AppendTask ->
            { model | newTask = "" }
            |> Focus.update (checklist => tasks) (\tasks -> tasks ++ [ Task model.newTask False ])
            |> pure
        EditTask idx ->
            -- TODO
            pure model
        Mdl msg_ ->
            Material.update Mdl msg_ model



---- VIEW ----

type alias Mdl = Material.Model

viewTask : Int -> Task -> Html Msg
viewTask idx submodel =
    let
        color =
            if submodel.done
            then Color.color Color.Grey Color.S300
            else Color.black
    in Lists.li []
        -- [ Options.css "border-bottom" "1px solid #000 !important" ]
        -- TODO(akavel): somehow add dividers, maybe in similar way as in:
        -- https://github.com/google/material-design-lite/pull/1785/files
        [ Lists.content
            [ Color.text color
            , Options.attribute <| Html.Events.onClick (EditTask idx)
            , Dialog.openOn "click"
            -- , Options.attribute <| Html.Events.onClick <| EditTask <| idx
            -- , Options.css "border-bottom" "1px solid rgba(#000, 0.12) !important"
            -- , Options.css "border-bottom" "1px solid #000 !important"
            ]
            [ text (submodel.text) ]
        ]

viewMain : Model -> Html Msg
viewMain model =
    div []
        [ Textfield.render
            Mdl [0] model.mdl  -- MDL boilerplate
            [ Textfield.label "Your text"
            , Textfield.floatingLabel
            -- , Options.onInput Change
            ] []
        , Options.styled p
            [ Typo.headline ]
            [ text (String.reverse model.text) ]
        , Lists.ul []
            (List.indexedMap viewTask model.checklist.tasks)
        ]

view : Model -> Html Msg
view model =
    div [ class "app-layout" ]
        [ header [ class "app-header" ]
            [ text "hello header" ]
        , main_ [ class "app-content" ]
            -- [ text "hello content"
            [ Lists.ul []
                (List.indexedMap viewTask model.checklist.tasks)
            ]
        , footer [ class "app-footer" ]
            [ Textfield.render
                Mdl [0] model.mdl  -- MDL boilerplate
                [ Textfield.label "New task"
                , Textfield.floatingLabel
                , Textfield.text_
                , Textfield.value model.newTask
                , Options.onInput EditNewTask
                ]
                []
            , Button.render
                Mdl [1] model.mdl  -- MDL boilerplate
                [ Button.fab
                , Button.colored
                , Button.disabled |> Options.when (String.trim model.newTask == "")
                , Options.onClick AppendTask
                ]
                [ Icon.i "add" ]
            ]
        , Dialog.view []
            [ Dialog.title []
                [ text "Hello dialog" ]
            ]
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
