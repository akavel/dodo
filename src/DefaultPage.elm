module DefaultPage exposing (..)

import Html exposing (Html)
import Html.Attributes
import String
-- mdgriffith/stylish-elephants — easier building of HTML+CSS layouts
import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
-- evancz/focus — helpers for modifying nested fields in Model
import Focus exposing (..)
-- (internal modules)
import StorageV1


---- MODEL ----


type alias Model =
    { checklist : StorageV1.Checklist
    , newTask : String
    -- TODO(akavel): put below stuff in single Maybe
    , editTask : Bool
    , editTaskIdx : Int
    , editTaskText : String
    , selectedTab : Int
    , verifyDeleteTask : Bool
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
    , editTask = False
    , editTaskIdx = -1
    , editTaskText = ""
    , selectedTab = 1
    , verifyDeleteTask = False
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
    | VerifyDeleteTask
    | DeleteTask Bool


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
        VerifyDeleteTask ->
            ( { model | verifyDeleteTask = True }, Please Cmd.none )
        DeleteTask False ->
            ( model |> stopEdit, Please Cmd.none )
        DeleteTask True ->
            let
                newModel =
                    model |> stopEdit |> transformTasksWith (\idx task ->
                        if idx == model.editTaskIdx
                            then Nothing
                            else Just task)
            in ( newModel, PleaseSave )


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
            , attrWhen (model.editTask)
                <| inFront
                <| column
                    [ height fill
                    , width fill
                    ]
                    [ el
                        [ Background.color Color.gray
                        , alpha 0.75
                        , height fill
                        , width fill
                        ]
                        ( text "" )
                    , viewFooter model
                    ]
            ]
            [ viewHeader model
            , column
                [ height fill
                , width fill
                , scrollbarY
                , spacing 20
                , padding 20
                ]
                (List.indexedMap (viewTask model) model.checklist.tasks)
            , viewFooter model
            ]


viewHeader : Model -> Element Msg
viewHeader model =
    row
        [ width fill ]
        [ Input.button
            [ alignLeft ]
            { onPress = Just SwipeLeft
            , label = icon "chevron_left"
            }
        , el [ centerX ] (text model.checklist.name)
        , Input.button
            [ alignRight ]
            { onPress = Just SwipeRight
            , label = icon "chevron_right"
            }
        ]


viewTask : Model -> Int -> StorageV1.Task -> Element Msg
viewTask model idx submodel =
    paragraph
        [ Event.onClick (EditTask idx)
        , width fill
        , Font.alignLeft
        , attrWhen submodel.done
            <| Font.color gray
        -- , Border.widthEach
        --     { bottom = 1, left = 0, right = 0, top = 0 }
        -- , Border.color <| Color.rgba 0 0 0 0.12
        ]
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
            -- , placeholder = Just <| Input.placeholder [] <| text "Edit task"
            -- , label = Input.labelLeft [] <| text "Edit"
            -- , label = Input.labelAbove [] <| text "Edit task"
            , label = Input.labelLeft [] <| text ""
            -- , label = Input.labelAbove [] <| text ""
            }
    else
        row []
            [ Input.text []
                { onChange = Just EditNewTask
                , text = model.newTask
                , placeholder = Nothing
                -- , placeholder = Just <| Input.placeholder [] <| text "New task"
                , label = Input.labelLeft [] <| text "New"
                -- , label = Input.labelAbove [] <| text "New task"
                -- , label = Input.labelAbove [] <| text ""
                }
            , Input.button
                [ mdl ["button", "js-button", "button--fab", "button--colored", "js-ripple-effect"]
                -- NOTE(akavel): without height, stylish-elefants makes button disappear
                , height (px 56)
                , disabledWhen (String.trim model.newTask == "")
                , alignRight
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
                    , Background.color <| Color.rgb 200 200 200
                    ]
                    { onPress = Just CancelEdit
                    , label = icon "close"
                    }
                , Input.button
                    [ centerX
                    -- , mdl ["button", "js-button", "button--mini-fab"]
                    -- TODO(akavel): allow quitting the menu by pressing
                    -- outside of it, or just display a regular yes/no
                    -- modal dialog window instead.
                    , attrWhen model.verifyDeleteTask
                        <| above
                        <| column []
                            -- [ Background.color Color.white
                            -- , padding 10
                            -- , width shrink
                            -- , height shrink
                            -- ]
                            [ Input.button
                                [ Background.color Color.white
                                , padding 10
                                , Border.rounded 3
                                , mdl ["shadow--2dp"]
                                ]
                                { onPress = Just (DeleteTask True)
                                -- , label = text "Delete task?"
                                , label = row []
                                    [ icon "delete_forever"
                                    , text "Delete task?"
                                    ]
                                }
                            ]
                    ]
                    { onPress = Just VerifyDeleteTask
                    , label = icon "delete"
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
                    , Background.color <| Color.rgb 200 200 200
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
        , editTaskIdx = -1
        , verifyDeleteTask = False
        }

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


