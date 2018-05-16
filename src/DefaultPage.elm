module DefaultPage exposing (..)

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
import Material.Tabs as Tabs
-- evancz/focus — helpers for modifying nested fields in Model
import Focus exposing (..)
-- (internal modules)
import StorageV0


---- MODEL ----


type alias Model =
    -- TODO(akavel): checklists : List Checklist
    { checklist : StorageV0.Checklist
    , newTask : String
    , mdl : Material.Model  -- MDL boilerplate
    -- TODO(akavel): put below stuff in single Maybe
    , editTask : Bool
    , editTaskIdx : Int
    , editTaskText : String
    , selectedTab : Int
    }

checklist : Focus { r | checklist : a } a
checklist = Focus.create .checklist (\f r -> { r | checklist = f r.checklist })

tasks : Focus { r | tasks : a } a
tasks = Focus.create .tasks (\f r -> { r | tasks = f r.tasks })

model : Model
model =
    { checklist = StorageV0.Checklist "New List 0"
        [ StorageV0.Task "Foo" False
        , StorageV0.Task "Bar" True
        ]
    , newTask = ""
    , mdl = Material.model  -- MDL boilerplate
    , editTask = False
    , editTaskIdx = -1
    , editTaskText = ""
    , selectedTab = 1
    }


---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Menu.subs Mdl model.mdl
        , StorageV0.loaded LoadedStorageV0
        ]


---- UPDATE ----


type Msg
    = LoadedStorageV0 (Maybe StorageV0.Model)
    | EditNewTask String
    | AppendTask
    | EditTask Int
    | EditTaskText String
    | CancelEdit
    | ToggleTask
    | SaveEdit
    | DeleteTask
    | Mdl (Material.Msg Msg)  -- MDL boilerplate


-- Plea is a request to parent view to execute the specified message
type Plea
    = Please (Cmd Msg)
    | PleaseSwipeLeft
    | PleaseSwipeRight

update : Msg -> Model -> ( Model, Plea )
update msg model =
    case msg of
        LoadedStorageV0 Nothing ->
            ( { model | checklist = StorageV0.Checklist "New List 0" [] }, Please Cmd.none )
        LoadedStorageV0 (Just storageV0) ->
            ( { model | checklist = storageV0.checklist }, Please Cmd.none )
        EditNewTask newText ->
            ( { model | newTask = newText }, Please Cmd.none )
        AppendTask ->
            let
                newModel =
                    { model | newTask = "" }
                    |> Focus.update (checklist => tasks) (\tasks -> tasks ++ [ StorageV0.Task model.newTask False ])
                storageV0 =
                    StorageV0.Model newModel.checklist
            in ( newModel, Please (StorageV0.save storageV0) )
        EditTask idx ->
            ( { model
                | editTask = True
                , editTaskIdx = idx
                , editTaskText =
                    model.checklist.tasks
                    |> nth idx
                    |> Maybe.map .text
                    |> Maybe.withDefault ""
                }, Please Cmd.none )
        EditTaskText newText ->
            ( { model | editTaskText = newText }, Please Cmd.none )
        CancelEdit ->
            ( model |> stopEdit, Please Cmd.none )
        ToggleTask ->
            let
                newModel =
                    model |> stopEdit |> transformTasksWith (\idx task ->
                        if idx == model.editTaskIdx
                            then Just { task | done = not task.done }
                            else Just task)
                storageV0 =
                    StorageV0.Model newModel.checklist
            in ( newModel, Please (StorageV0.save storageV0) )
        SaveEdit ->
            let
                newModel =
                    model |> stopEdit |> transformTasksWith (\idx task ->
                        if idx == model.editTaskIdx
                            then Just { task | text = model.editTaskText }
                            else Just task)
                storageV0 =
                    StorageV0.Model newModel.checklist
            in ( newModel, Please (StorageV0.save storageV0) )
        DeleteTask ->
            let
                newModel =
                    model |> stopEdit |> transformTasksWith (\idx task ->
                        if idx == model.editTaskIdx
                            then Nothing
                            else Just task)
                storageV0 =
                    StorageV0.Model newModel.checklist
            in ( newModel, Please (StorageV0.save storageV0) )
        Mdl msg_ ->
            Material.update Mdl msg_ model
            |> Tuple.mapSecond Please



---- VIEW ----


type alias Mdl = Material.Model

view : Model -> Html Msg
view model =
    div [ class "app-layout" ]
        [ header
            [ classList
                [ ("app-header", True)
                -- TODO(akavel): clicking grayed-out area should cause CancelEdit
                , ("grayed-out", model.editTask)
                ]
            ]
            [ text model.checklist.name ]
        , main_
            [ classList
                [ ("app-main", True)
                -- TODO(akavel): clicking grayed-out area should cause CancelEdit
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
        ]
    |> Material.Scheme.top

viewTask : Int -> StorageV0.Task -> Html Msg
viewTask idx submodel =
    let
        gray =
            Color.color Color.Grey Color.S300
        content2 =
            if submodel.done
            then
                Lists.content2 []
                    [ Icon.view "sentiment_very_satisfied" [ Color.text gray ] ]
            else
                text ""
    in
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
                [ Options.when submodel.done <| Color.text gray
                , Options.css "text-align" "left"
                ]
                [ text (submodel.text) ]
            , content2
            ]

viewEditActions model =
    let
        originalTask =
            model.checklist.tasks
            |> nth model.editTaskIdx
        originalText =
            originalTask
            |> Maybe.map .text
        isNotEdited =
            originalText == Just model.editTaskText
        isDone =
            originalTask
            |> Maybe.map .done
            |> Maybe.withDefault False
        doneIcon =
            if isDone == True
                then "sentiment_satisfied"       -- clicking will go back to only "satisfied"
                else "sentiment_very_satisfied"  -- clicking will mark done == "very satisfied"
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
                    [ Icon.i "delete_forever", text "Delete task?" ]
                ]
            , stretcher
            , Button.render
                Mdl [80, 9] model.mdl  -- MDL boilerplate
                [ Button.fab
                , Button.colored
                , Button.accent |> Options.when (not isDone)
                , Button.primary |> Options.when isDone
                , Elevation.e4
                , Options.onClick ToggleTask
                ]
                [ Icon.i doneIcon ]
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
                , Button.disabled |> Options.when (String.trim model.editTaskText == "")
                , Options.onClick SaveEdit
                ]
                [ Icon.i "check" ]
            ]

viewFooter model =
    if model.editTask
    then
        [ Textfield.render
            Mdl [1] model.mdl  -- MDL boilerplate
            [ Textfield.label "Edit task"
            , Textfield.floatingLabel
            , Textfield.text_
            , Textfield.value model.editTaskText
            , Options.attribute <| Html.Attributes.attribute "autocapitalize" "sentences"
            , Options.onInput EditTaskText
            ]
            []
        ]
    else
        [ Textfield.render
            Mdl [2] model.mdl  -- MDL boilerplate
            [ Textfield.label "New task"
            , Textfield.floatingLabel
            , Textfield.text_
            , Textfield.value model.newTask
            , Options.attribute <| Html.Attributes.attribute "autocapitalize" "sentences"
            , Options.onInput EditNewTask
            ]
            []
        , Button.render
            Mdl [3] model.mdl  -- MDL boilerplate
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

stopEdit : Model -> Model
stopEdit model =
    { model
        | editTask = False
        , editTaskIdx = -1 }

transformTasksWith : (Int -> StorageV0.Task -> Maybe StorageV0.Task) -> Model -> Model
transformTasksWith taskModifier model =
    let
        listModifier tasks =
            tasks
            |> List.indexedMap taskModifier
            |> List.filterMap identity
    in
        { model
            | editTask = False
            , editTaskIdx = -1 }
        |> Focus.update (checklist => tasks) listModifier

