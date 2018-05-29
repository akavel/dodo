module DefaultPage exposing (..)

import Html exposing (Html)
import Html.Attributes
import String
-- mdgriffith/stylish-elephants — easier building of HTML+CSS layouts
import Color exposing (..)
import Element exposing (..)
import Element.Events as Event
import Element.Input as Input
-- evancz/focus — helpers for modifying nested fields in Model
import Focus exposing (..)
-- (internal modules)
import StorageV1


---- MODEL ----


type alias Model =
    { checklist : StorageV1.Checklist
    , newTask : String
    -- , newTaskXXX : Int   -- HACK required by https://github.com/mdgriffith/style-elements/issues/91
    -- , mdl : Material.Model  -- MDL boilerplate
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
    { checklist = StorageV1.Checklist "New List 0"
        [ StorageV1.Task "Foo" False
        , StorageV1.Task "Bar" True
        ]
    -- TODO(akavel): newTask & editTask are not needed if we treat 'editTaskIdx
    -- == len(checklist)' as editing of new task
    , newTask = ""
    -- , newTaskXXX = 0
    -- , mdl = Material.model  -- MDL boilerplate
    , editTask = False
    , editTaskIdx = -1
    , editTaskText = ""
    , selectedTab = 1
    }


---- UPDATE ----


type Msg
    = SwipeLeft
    | SwipeRight
    | EditNewTask String
    | AppendTask
    | EditTask Int
    | EditTaskText String
    | CancelEdit
    | ToggleTask
    | SaveEdit
    | DeleteTask
    -- | Mdl (Material.Msg Msg)  -- MDL boilerplate


-- Plea is a request to parent view to execute the specified message
type Plea
    = Please (Cmd Msg)
    | PleaseSave
    | PleaseSwipeLeft
    | PleaseSwipeRight

update : Msg -> Model -> ( Model, Plea )
update msg model =
    case msg of
        SwipeLeft ->
            ( model, PleaseSwipeLeft )
        SwipeRight ->
            ( model, PleaseSwipeRight )
        EditNewTask newText ->
            ( { model | newTask = newText }, Please Cmd.none )
        AppendTask ->
            let
                newModel =
                    { model
                        | newTask = ""
                        -- , newTaskXXX = model.newTaskXXX + 1
                        }
                    |> Focus.update (checklist => tasks) (\tasks -> tasks ++ [ StorageV1.Task model.newTask False ])
            in ( newModel, PleaseSave )
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
            in ( newModel, PleaseSave )
        SaveEdit ->
            let
                newModel =
                    model |> stopEdit |> transformTasksWith (\idx task ->
                        if idx == model.editTaskIdx
                            then Just { task | text = model.editTaskText }
                            else Just task)
            in ( newModel, PleaseSave )
        DeleteTask ->
            let
                newModel =
                    model |> stopEdit |> transformTasksWith (\idx task ->
                        if idx == model.editTaskIdx
                            then Nothing
                            else Just task)
            in ( newModel, PleaseSave )
        -- Mdl msg_ ->
        --     Material.update Mdl msg_ model
        --     |> Tuple.mapSecond Please


---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Menu.subs Mdl model.mdl
    Sub.none


---- STYLES ----


type Styles
    = Screen
    | Navbar
    | Group      -- a group (list) of items
    | Item Bool  -- True for pending, False for done
    | Footer
    | PLAIN


type Variations
    = Disabled


stylesheet =
    styleSheet
        [ style (Item True)
            [ variation Disabled styleDisabled
            ]
        , style (Item False)
            [ Color.text gray
            , variation Disabled styleDisabled
            ]
        ]


styleDisabled =
    -- [ Color.background lightGray ]
    -- [ Filter.blur 0.5 ]
    [ Filter.opacity 25.0 ]
    -- [ Filter.blur 10.5, Filter.invert 10.5 ]
    -- [ Style.prop "filter" "blur(5px) invert(25%)" ]

---- VIEW ----


view : Model -> Html Msg
view model =
    viewport stylesheet <|
        column Screen
            [ height fill
            , width fill
            ]
            [ el Navbar [] (text model.checklist.name)
            , column Group
                [ height fill
                , width fill
                , spacing 20
                , yScrollbar
                ]
                (List.indexedMap (viewTask model) model.checklist.tasks)
            , viewFooter model
            ]

viewTask : Model -> Int -> StorageV1.Task -> Element Msg
viewTask model idx submodel =
    paragraph (Item <| not submodel.done)
        [ Event.onClick (EditTask idx) |> attrWhen (not model.editTask)
        , vary Disabled model.editTask
        ]
        [ text submodel.text ]


viewFooter : Model -> Element Msg
viewFooter model =
    if model.editTask
    then
        Input.text PLAIN []
            { onChange = EditTaskText
            , value = model.editTaskText
            , label = Input.labelAbove <| text "Edit task"
            , options = []
            }
        |> above (viewEditActions model)
    else
        row PLAIN []
            [ Input.text PLAIN []
                { onChange = EditNewTask
                , value = model.newTask
                , label = Input.labelAbove <| text "New task"
                , options = []
                    -- [ Input.textKey <| toString model.newTaskXXX ]
                }
            , Input.button PLAIN
                [ Event.onClick AppendTask
                , classList
                    [ ("mdl-button", True)
                    , ("mdl-js-button", True)
                    , ("mdl-button--fab", True)
                    , ("mdl-button--colored", True)
                    , ("mdl-js-ripple-effect", True)
                    ]
                , attribute "disabled" "disabled"
                    |> attrWhen (String.trim model.newTask == "")
                ]
                -- ( italic "add" )
                ( icon "add" )
            ]


viewEditActions : Model -> List (Element Msg)
viewEditActions model =
    [ row PLAIN
        -- [ spread
        [ width fill
        ]
        [ Input.button PLAIN
            [ Event.onClick CancelEdit
            , alignLeft
            ]
            ( icon "close" )
        , Input.button PLAIN
            [ Event.onClick ToggleTask
            , alignRight
            ]
            ( icon "done" )
        ]
    ]


{--
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
            [ Button.render
                Mdl [10, 0] model.mdl  -- MDL boilerplate
                [ Options.onClick SwipeLeft ]
                [ Icon.i "chevron_left" ]
            , Options.styled p
                [ Typo.title ]
                [ text model.checklist.name ]
            , Button.render
                Mdl [10, 1] model.mdl  -- MDL boilerplate
                [ Options.onClick SwipeRight ]
                [ Icon.i "chevron_right" ]
            ]
            -- [ text model.checklist.name ]
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

viewTask : Int -> StorageV1.Task -> Html Msg
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
--}


---- UTILS ----


nth : Int -> List a -> Maybe a
nth n list =
    List.head <| List.drop n list


attrWhen : Bool -> Attribute msg -> Attribute msg
attrWhen condition attr =
    if condition
    then attr
    -- else inlineStyle []
    -- TODO(akavel): try to find a better NOP attribute
    else scale 1.0


attribute : String -> String -> Attribute msg
attribute name value =
    Html.Attributes.attribute name value |> htmlAttribute


classList : List (String, Bool) -> Attribute msg
classList list =
    Html.Attributes.classList list |> htmlAttribute


icon : String -> Element Msg
icon name =
    -- TODO(akavel): check if `el (italic ...)` would work
    html <| Html.i
        [ Html.Attributes.class "material-icons" ]
        [ Html.text name ]


stopEdit : Model -> Model
stopEdit model =
    { model
        | editTask = False
        , editTaskIdx = -1 }

transformTasksWith : (Int -> StorageV1.Task -> Maybe StorageV1.Task) -> Model -> Model
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


