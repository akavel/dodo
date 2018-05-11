module Main exposing (..)

import Html exposing (Html, div, text, p, header, footer, main_)
import Html.Attributes exposing (class, classList, style)
import Html.Events
import String
-- debois/elm-mdl — Material Design Lite (MDL)
import Material
import Material.Scheme
import Material.Elevation as Elevation
import Material.Options as Options exposing (cs)
import Material.Color as Color
import Material.Textfield as Textfield
import Material.Button as Button
import Material.Icon as Icon
import Material.List as Lists
import Material.Menu as Menu
-- evancz/focus — helpers for modifying nested fields in Model
import Focus exposing (..)


---- MODEL ----


type alias Model =
    { text : String
    -- TODO(akavel): , checklists : List Checklist
    , checklist : Checklist
    , newTask : String
    , mdl : Material.Model  -- MDL boilerplate
    -- TODO(akavel): put below stuff in single Maybe
    , editTask : Bool
    , editTaskIdx : Int
    , editTaskText : String
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
    , editTask = False
    , editTaskIdx = -1
    , editTaskText = ""
    }


---- UPDATE ----


{-
We need to be able to:
- (done) add new items to the current list
- remove item from current list
- disable/reenable item from current list (toggle)
- edit an item on the list
- save items on disk and load them on app start
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
    | SaveEdit
    -- | ToggleTask Int
    -- | DeleteTask Int
    | Mdl (Material.Msg Msg)  -- MDL boilerplate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditNewTask newText ->
            { model | newTask = newText } ! [Cmd.none]
        AppendTask ->
            { model | newTask = "" }
            |> Focus.update (checklist => tasks) (\tasks -> tasks ++ [ Task model.newTask False ])
            |> flip (!) [Cmd.none]
        EditTask idx ->
            { model
                | editTask = True
                , editTaskIdx = idx
                , editTaskText =
                    model.checklist.tasks
                    |> List.drop idx
                    |> List.head
                    |> Maybe.map .text
                    |> Maybe.withDefault ""
                } ! [Cmd.none]
        SaveEdit ->
            -- TODO
            { model |
                editTask = False
                , editTaskIdx = -1
                } ! [Cmd.none]
        Mdl msg_ ->
            Material.update Mdl msg_ model



---- VIEW ----

type alias Mdl = Material.Model

view : Model -> Html Msg
view model =
    div [ class "app-layout" ]
        [ header
            [ classList
                [ ("app-header", True)
                , ("grayed-out", model.editTask)
                ]
            ]
            [ text "hello header" ]
        , main_
            [ classList
                [ ("app-main", True)
                , ("grayed-out", model.editTask)
                ]
            ]
            [ div
                [ classList
                    [ ("edit-actions", True)
                    , ("hidden-out", not model.editTask)
                    ]
                ]
                [ Menu.render
                    Mdl [80, 0] model.mdl
                    [ Menu.topLeft
                    , Menu.icon "more_horiz"
                    , Options.css "id" "edit-menu"
                    ]
                    [ Menu.item
                        []
                        [ text "some item 1" ]
                    , Menu.item
                        []
                        [ text "some item 2" ]
                    ]
                -- [ Button.render
                --     Mdl [80, 0] model.mdl  -- MDL boilerplate
                --     [ Button.fab
                --     , Button.plain
                --     -- , Options.onClick ...
                --     -- , Options.css "margin-right" "auto"
                --     ]
                --     -- [ Icon.i "close" ]
                --     [ Icon.i "more_horiz" ]
                -- TODO(akavel): can we remove below div and do the stretching purely via CSS?
                , div [ style [("flex", "1")] ] []
                , Button.render
                    Mdl [80, 1] model.mdl  -- MDL boilerplate
                    [ Button.fab
                    , Button.colored
                    , Button.accent
                    , Elevation.e4
                    -- , Options.onClick ...
                    ]
                    [ Icon.i "sentiment_very_satisfied" ]
                ]
            , Lists.ul [ cs "app-checklist" ]
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
        -- , Dialog.render
        --     { styles = []
        --     , title = ""
        --     , content =
        --         -- [ Menu.render
        --         --     Mdl [100, 5] model.mdl  -- MDL boilerplate
        --         --     [ Menu.bottomRight ]
        --         --     [ Menu.item []
        --         --         [ Icon.i "delete_forever", text "Delete" ]
        --         --     ]
        --         [ Textfield.render
        --             Mdl [100, 0] model.mdl  -- MDL boilerplate
        --             [ Textfield.value model.dialogTaskText
        --             -- , Options.css "float" "left"
        --             -- , Options.onInput DialogEditTask
        --             ]
        --             []
        --         -- , Button.render
        --         --     Mdl [100, 1] model.mdl  -- MDL boilerplate
        --         --     [ Button.fab
        --         --     -- [ Button.primary
        --         --     -- , Button.colored
        --         --     -- , Button.minifab
        --         --     -- [ Button.icon
        --         --     -- , Options.css "float" "left"
        --         --     ]
        --         --     [ Icon.i "delete_forever" ]
        --         , Button.render
        --             Mdl [100, 2] model.mdl  -- MDL boilerplate
        --             [ Button.colored
        --             , Button.accent
        --             , Button.fab
        --             , Elevation.e4
        --             -- , Options.css "float" "right"
        --             ]
        --             [ Icon.i "sentiment_very_satisfied" ]
        --         ]
        --     , actionBar =
        --         [ Button.render
        --             Mdl [100, 90] model.mdl  -- MDL boilerplate
        --             [ Button.colored
        --             , Button.raised
        --             , Button.accent
        --             , Options.onClick SaveEdit
        --             ]
        --             [ text "OK" ]
        --         , Button.render
        --             Mdl [100, 91] model.mdl  -- MDL boilerplate
        --             [ Button.colored
        --             , Button.raised
        --             , Button.primary
        --             , Options.onClick SaveEdit
        --             ]
        --             [ text "Cancel" ]
        --         , Toggles.switch
        --             Mdl [100, 92] model.mdl  -- MDL boilerplate
        --             [ Toggles.value False ]
        --             -- [ text "Delete" ]
        --             [ Icon.i "delete_forever" ]
        --         -- [ Html.button
        --         --     [ Html.Events.onClick SaveEdit
        --         --     , class "mdl-button mdl-button--raised mdl-button--accent"
        --         --     ]
        --         --     [ text "Close" ]
        --         ]
        --     }
        --     model.dialogVisible
        ]
    |> Material.Scheme.top

viewTask : Int -> Task -> Html Msg
viewTask idx submodel =
    Lists.li
        -- TODO(akavel): verify if divider is styled OK w.r.t. Material Design
        -- see: https://github.com/google/material-design-lite/pull/1785/files
        [ Options.css "border-bottom" "1px solid rgba(0,0,0, 0.12)" ]
        [ Lists.content
            [ Options.when submodel.done <| Color.text (Color.color Color.Grey Color.S300)
            , Options.attribute <| Html.Events.onClick (EditTask idx)
            ]
            [ text (submodel.text) ]
        ]


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = (model, Cmd.none)
        , update = update
        -- , subscriptions = always Sub.none
        , subscriptions = (\model -> Menu.subs Mdl model.mdl)
        }
