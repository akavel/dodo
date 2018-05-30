module DefaultPage exposing (..)

import Html exposing (Html)
import Html.Attributes
import String
-- mdgriffith/stylish-elephants — easier building of HTML+CSS layouts
import Color exposing (..)
import Element exposing (..)
import Element.Events as Event
import Element.Input as Input
import Element.Font as Font
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


---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout
        -- NOTE(akavel): `height shrink` is a trick from mdgriffith himself; no idea how/why this works \O_o/
        [ height shrink
        , width fill
        ]
    <|
        column
            [ height fill
            , width fill
            ]
            [ el
                [ classList
                    [ ( "grayed-out", model.editTask ) ]
                ]
                (text model.checklist.name)
            , column
                [ height fill
                , width fill
                , scrollbarY
                , spacing 20
                -- , classList
                --     [ ( "grayed-out", model.editTask ) ]
                ]
                (List.indexedMap (viewTask model) model.checklist.tasks)
            , viewFooter model
            ]

viewTask : Model -> Int -> StorageV1.Task -> Element Msg
viewTask model idx submodel =
    paragraph
        ( [ Event.onClick (EditTask idx) |> attrWhen (not model.editTask)
        , width fill
        , classList
            [ ( "grayed-out", model.editTask ) ]
        ] ++ (styleItem submodel model.editTask) )
        [ text submodel.text ]


viewFooter : Model -> Element Msg
viewFooter model =
    if model.editTask
    then
        Input.text
            [ above (viewEditActions model) ]
            { onChange = Just EditTaskText
            , text = model.editTaskText
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "Edit task"
            }
    else
        row []
            [ Input.text []
                { onChange = Just EditNewTask
                , text = model.newTask
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text "New task"
                }
            , Input.button
                [ mdl ["button", "js-button", "button--fab", "button--colored", "js-ripple-effect"]
                , disabledWhen (String.trim model.newTask == "")
                ]
                -- FIXME(akavel): why below `if` doesn't work to make the element disabled?
                { onPress = if (String.trim model.newTask == "")
                    then Nothing
                    else Just AppendTask
                , label = icon "add"
                }
            ]


viewEditActions : Model -> Element Msg
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
    in
        if isNotEdited
        then
            row
                [ width fill
                ]
                [ Input.button
                    [ alignLeft
                    , mdl ["button", "js-button", "button--fab"]
                    ]
                    { onPress = Just CancelEdit
                    , label = icon "close"
                    }
                , Input.button
                    [ alignRight
                    , mdl ["button", "js-button", "shadow--4dp",
                        "button--fab", "button--colored"]
                    -- NOTE(akavel): without height, stylish-elefants makes button disappear
                    , height (px 56)
                    , classList
                        [ ("mdl-button--accent", not isDone)
                        , ("mdl-button--primary", isDone)
                        ]
                    ]
                    { onPress = Just ToggleTask
                    , label = icon doneIcon
                    }
                ]
        else
            row
                [ width fill
                ]
                [ Input.button
                    [ alignLeft
                    , mdl ["button", "js-button", "button--fab"]
                    ]
                    { onPress = Just CancelEdit
                    , label = icon "close"
                    }
                , Input.button
                    [ alignRight
                    , mdl ["button", "js-button", "shadow--4dp",
                        "button--fab", "button--colored", "button--accent"]
                    -- NOTE(akavel): without height, stylish-elefants makes button disappear
                    , height (px 56)
                    , disabledWhen (String.trim model.editTaskText == "")
                    ]
                    { onPress = Just SaveEdit
                    , label = icon "check"
                    }
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
        -- TODO(akavel): can we remove below div and do the stretching purely via CSS?
        stretcher =
            div [ style [("flex", "1")] ] []
    in
        if isNotEdited
        then
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
            [ Icon.i "add" ]
        ]
--}


---- STYLES ----


styleItem : StorageV1.Task -> Bool -> List (Attribute Msg)
styleItem task enabled =
    ( if task.done
        then [ Font.color gray ]
        else []
    ) ++
    ( if enabled
        then []
        else [ alpha 25.0 ]
    )

-- styleDisabled =
--     -- [ Color.background lightGray ]
--     -- [ Filter.blur 0.5 ]
--     [ Filter.opacity 25.0 ]
--     -- [ Filter.blur 10.5, Filter.invert 10.5 ]
--     -- [ Style.prop "filter" "blur(5px) invert(25%)" ]


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


disabledWhen : Bool -> Attribute msg
disabledWhen condition =
    attribute "disabled" "disabled"
    |> attrWhen condition

mdl : List String -> Attribute msg
mdl classSuffixes =
    classSuffixes
    |> List.map (\class -> ("mdl-" ++ class, True))
    |> Html.Attributes.classList
    |> htmlAttribute


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


