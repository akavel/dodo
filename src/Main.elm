port module Main exposing (..)

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


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = (model, loadStorage ())
        , update = update
        -- , subscriptions = always Sub.none
        , subscriptions = subscriptions
        }


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


---- PORTS ----

type alias StorageV0 =
    { checklist : Checklist
    -- TODO(akavel): add newTask, editTask*
    }

port saveStorage : StorageV0 -> Cmd msg
port loadStorage : () -> Cmd msg
port storageContents : (Maybe StorageV0 -> msg) -> Sub msg


---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Menu.subs Mdl model.mdl
        , storageContents LoadedStorageV0
        ]


---- UPDATE ----


{-
We need to be able to:
- (done) add new items to the current list
- remove item from current list
- disable/reenable item from current list (toggle)
- edit an item on the list
- (done) save items on disk and load them on app start
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
    = LoadedStorageV0 (Maybe StorageV0)
    | EditNewTask String
    | AppendTask
    | EditTask Int
    | EditTaskText String
    | CancelEdit
    | SaveEdit
    | DeleteTask
    -- | ToggleTask Int
    | Mdl (Material.Msg Msg)  -- MDL boilerplate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadedStorageV0 Nothing ->
            { model | checklist = Checklist "New List 0" [] } ! [Cmd.none]
        LoadedStorageV0 (Just storageV0) ->
            { model | checklist = storageV0.checklist } ! [Cmd.none]
        EditNewTask newText ->
            { model | newTask = newText } ! [Cmd.none]
        AppendTask ->
            let
                newModel =
                    { model | newTask = "" }
                    |> Focus.update (checklist => tasks) (\tasks -> tasks ++ [ Task model.newTask False ])
                storageV0 =
                    StorageV0 newModel.checklist
            in (newModel, saveStorage storageV0)
        EditTask idx ->
            { model
                | editTask = True
                , editTaskIdx = idx
                , editTaskText =
                    model.checklist.tasks
                    |> nth idx
                    |> Maybe.map .text
                    |> Maybe.withDefault ""
                } ! [Cmd.none]
        EditTaskText newText ->
            { model | editTaskText = newText } ! [Cmd.none]
        CancelEdit ->
            { model
                | editTask = False
                , editTaskIdx = -1
            } ! [Cmd.none]
        SaveEdit ->
            let
                newTasks =
                    model.checklist.tasks
                    |> List.indexedMap (\idx task ->
                        if idx == model.editTaskIdx
                            then { task | text = model.editTaskText }
                            else task)
                newModel =
                    { model
                        | editTask = False
                        , editTaskIdx = -1 }
                    |> Focus.update (checklist => tasks) (\tasks -> newTasks)
                storageV0 =
                    StorageV0 newModel.checklist
            in (newModel, saveStorage storageV0)
        DeleteTask ->
            let
                remainingTasks =
                    model.checklist.tasks
                    |> List.indexedMap (,)
                    |> List.filterMap (\(idx, task) ->
                        if idx == model.editTaskIdx
                            then Nothing
                            else Just task)
                newModel =
                    { model
                        | editTask = False
                        , editTaskIdx = -1 }
                    |> Focus.update (checklist => tasks) (\tasks -> remainingTasks)
                storageV0 =
                    StorageV0 newModel.checklist
            in (newModel, saveStorage storageV0)
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
            [ text model.checklist.name ]
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
                (viewEditActions model)
            , Lists.ul [ cs "app-checklist" ]
                (List.indexedMap viewTask model.checklist.tasks)
            ]
        , footer [ class "app-footer" ]
            (viewFooter model)
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
        [ Options.css "border-bottom" "1px solid rgba(0,0,0, 0.12)"
        , Options.attribute <| Html.Events.onClick (EditTask idx)
        -- FIXME(akavel): why below doesn't work?
        , Options.when (model.editTask && idx == model.editTaskIdx)
            <| Color.background Color.primary
        ]
        [ Lists.content
            [ Options.when submodel.done <| Color.text (Color.color Color.Grey Color.S300)
            ]
            [ text (submodel.text) ]
        ]

viewEditActions model =
    let
        oldText =
            model.checklist.tasks
            |> nth model.editTaskIdx
            |> Maybe.map .text
        isNotEdited =
            oldText == Just model.editTaskText
        -- TODO(akavel): can we remove below div and do the stretching purely via CSS?
        stretcher =
            div [ style [("flex", "1")] ] []
    in
        if isNotEdited
        then
            [ Button.render
                Mdl [80, 0] model.mdl  -- MDL boilerplate
                [ Button.fab
                , Button.flat
                , Options.onClick CancelEdit
                ]
                [ Icon.i "close" ]
            , stretcher
            -- TODO(akavel): make the menu icon bigger & proper fab (or at least minifab)
            , Menu.render
                Mdl [80, 1] model.mdl  -- MDL boilerplate
                [ Menu.topLeft
                , Menu.icon "delete"
                , Options.css "id" "edit-menu"
                ]
                [ Menu.item
                    [ Menu.onSelect DeleteTask ]
                    [ Icon.i "delete_forever", text "Delete forever?" ]
                ]
            , stretcher
            , Button.render
                Mdl [80, 9] model.mdl  -- MDL boilerplate
                [ Button.fab
                , Button.colored
                , Button.accent
                , Elevation.e4
                -- , Options.onClick ...
                ]
                [ Icon.i "sentiment_very_satisfied" ]
            ]
        else
            [ Button.render
                Mdl [81, 0] model.mdl  -- MDL boilerplate
                [ Button.fab
                , Button.flat
                , Options.onClick CancelEdit
                ]
                [ Icon.i "close" ]
            , stretcher
            , Button.render
                Mdl [81, 9] model.mdl  -- MDL boilerplate
                [ Button.fab
                , Button.colored
                , Button.accent
                , Elevation.e4
                , Options.onClick SaveEdit
                ]
                [ Icon.i "check" ]
            ]

viewFooter model =
    if model.editTask
    then
        [ Textfield.render
            Mdl [0] model.mdl  -- MDL boilerplate
            [ Textfield.label "Edit task"
            , Textfield.floatingLabel
            , Textfield.text_
            , Textfield.value model.editTaskText
            , Options.onInput EditTaskText
            ]
            []
        ]
    else
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

---- UTILS ----

nth : Int -> List a -> Maybe a
nth n list =
    List.head <| List.drop n list

