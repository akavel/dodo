module DefaultPage exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import String
import Json.Decode
import Debug
-- mdgriffith/stylish-elephants — easier building of HTML+CSS layouts
import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
-- evancz/focus — helpers for modifying nested fields in Model
import Focus exposing (..)
-- (internal modules)
import StorageV1


---- MODEL ----


type alias Model =
    { checklist : StorageV1.Checklist
    , editedTaskText : String
    , editedTaskIdx : Int
    , askingTaskDeletion : Bool
    , showingChecklistMenu : Bool
    , editedChecklistName : Maybe String
    , verifyingChecklistDeletion : Bool
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
    , editedTaskText = ""
    , editedTaskIdx = newTaskIdx
    , askingTaskDeletion = False
    , showingChecklistMenu = False
    , editedChecklistName = Nothing
    , verifyingChecklistDeletion = False
    }


newTaskIdx = -1


---- UPDATE ----


type Msg
    = SwipeLeft
    | SwipeRight
    | EditTask Int
    | EditTaskText String
    | CancelEdit
    | ToggleTask
    | SaveEdit
    | AskDeleteTask Bool
    | DeleteTask
    | ShowChecklistMenu Bool
    | EditChecklistName (Maybe String)
    | SaveChecklistName
    | AddChecklist
    | VerifyDeleteChecklist
    | DeleteChecklist Bool


-- Plea is a request to parent view to execute the specified message
type Plea
    = Please (Cmd Msg)
    | PleaseSave
    | PleaseSwipeLeft
    | PleaseSwipeRight
    | PleaseAddChecklist
    | PleaseDeleteChecklist


update : Msg -> Model -> ( Model, Plea )
update msg model =
    case msg of
        SwipeLeft ->
            ( model, PleaseSwipeLeft )
        SwipeRight ->
            ( model, PleaseSwipeRight )
        EditTask idx ->
            ( { model
                | editedTaskIdx = idx
                , editedTaskText =
                    model.checklist.tasks
                    |> nth idx
                    |> Maybe.map .text
                    |> Maybe.withDefault ""
                }, Please Cmd.none )
        EditTaskText newText ->
            ( { model | editedTaskText = newText }, Please Cmd.none )
        CancelEdit ->
            ( model |> stopEdit, Please Cmd.none )
        ToggleTask ->
            let
                newModel =
                    model |> stopEdit |> modifyNthTask model.editedTaskIdx
                        (\task -> Just { task | done = not task.done })
            in ( newModel, PleaseSave )
        SaveEdit ->
            let
                newText =
                    model.editedTaskText |> String.trim |> capitalizeLeft
                newModel =
                    if model.editedTaskIdx == newTaskIdx
                    then
                        model
                        |> stopEdit
                        |> Focus.update (checklist => tasks) (\tasks -> tasks ++ [ StorageV1.Task newText False ])
                    else
                        model |> stopEdit |> modifyNthTask model.editedTaskIdx
                            (\task -> Just { task | text = newText })
            in ( newModel, PleaseSave )
        AskDeleteTask ask ->
            ( { model | askingTaskDeletion = Debug.log "askingTaskDeletion:" ask }, Please Cmd.none )
        DeleteTask ->
            let
                newModel =
                    model |> stopEdit |> modifyNthTask model.editedTaskIdx
                        (\task -> Nothing)
            in ( newModel, PleaseSave )
        ShowChecklistMenu show ->
            ( { model | showingChecklistMenu = show }, Please Cmd.none )
        EditChecklistName (Just newName) ->
            ( { model
                | editedChecklistName = Just newName
                , showingChecklistMenu = False
                }, Please Cmd.none )
        EditChecklistName Nothing ->
            ( { model
                | editedChecklistName = Nothing
                }, Please Cmd.none )
        SaveChecklistName ->
            let
                newName =
                    model.editedChecklistName
                    |> Maybe.withDefault model.checklist.name
                    |> String.trim
                    |> capitalizeLeft
                checklist =
                    model.checklist
                newChecklist =
                    { checklist | name = newName }
            in ( { model
                | checklist = newChecklist
                , editedChecklistName = Nothing
                }, PleaseSave )
        AddChecklist ->
            ( model, PleaseAddChecklist )
        VerifyDeleteChecklist ->
            ( { model
                | verifyingChecklistDeletion = True
                , showingChecklistMenu = False
                }, Please Cmd.none )
        DeleteChecklist confirm ->
            ( { model | verifyingChecklistDeletion = False }
            , if confirm then PleaseDeleteChecklist else Please Cmd.none
            )


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
            -- Show modal dialog for editing a task, if needed
            , attrWhen (model.editedTaskIdx /= newTaskIdx)
                <| inFront
                <| scrim
                    { alpha = grayedOutAlpha
                    , onClick = Just (AskDeleteTask False)
                    , top = fill
                    , right = (px 0)
                    , bottom = (px 0)
                    , left = (px 0)
                    }
                    <| viewFooter model
            -- Show Checklist-related menu as a modal dialog, if needed
            , attrWhen model.showingChecklistMenu
                <| inFront
                <| viewChecklistMenuLayer model.checklist.name
            -- Show modal dialog for editing Checklist name, if needed
            , attrWhen (model.editedChecklistName /= Nothing)
                <| inFront
                <| viewChecklistNameEditor
                <| Maybe.withDefault ""
                <| model.editedChecklistName
            -- Show modal dialog for deleting Checklist, if needed
            , attrWhen model.verifyingChecklistDeletion
                <| inFront
                <| scrim
                    { alpha = grayedOutAlpha
                    , onClick = Just (DeleteChecklist False)
                    , top = fill
                    , right = fill
                    , bottom = fill
                    , left = fill
                    }
                <| column
                    [ height fill
                    , width fill
                    -- , Background.color Color.white
                    , Border.rounded 6
                    , padding 16
                    , spacing 16
                    , mdl ["shadow--8dp"]
                    ]
                    [ el
                        [ Region.heading 4
                        , width fill
                        , Font.bold
                        -- , mdl ["dialog__title"]
                        ]
                        ( text "Delete checklist?" )
                    , row
                        [ width fill
                        -- , mdl ["dialog__actions"]
                        , spacing 16
                        ]
                        [ Input.button
                            [ mdl ["button", "js-button", "button--primary"]
                            , width fill
                            ]
                            { onPress = Just (DeleteChecklist True)
                            , label = text "Delete"
                            }
                        , Input.button
                            [ mdl ["button", "js-button", "button--primary"]
                            , width fill
                            ]
                            { onPress = Just (DeleteChecklist False)
                            , label = text "Cancel"
                            }
                        ]
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
                (List.indexedMap viewTask model.checklist.tasks)
            , if (model.editedTaskIdx == newTaskIdx)
                then viewFooter model
                -- Dummy element to fill the same height as footer would have,
                -- so that task items don't move when we start editing a task.
                -- Alternatively, could maybe use fake: viewFooter { model | editedTaskIdx = newTaskIdx }
                else el
                    [ height (px 56)
                    , width fill
                    ]
                    none
            ]


viewHeader : Model -> Element Msg
viewHeader model =
    row
        [ width fill
        , mdl ["shadow--2dp", "color--primary", "color-text--primary-contrast"]
        , height (px 40)
        ]
        [ Input.button
            [ alignLeft ]
            { onPress = Just SwipeLeft
            , label = icon "chevron_left"
            }
        -- TODO(akavel): if text overflows, somehow make it use smaller font +
        -- multiple rows; if even longer, trim right with "..."
        , el
            [ centerX
            -- [ width fill
            , Event.onClick (ShowChecklistMenu True)
            -- , attrWhen (model.showingChecklistMenu)
            --     <| below
            --     <| viewChecklistMenu model
            ]
            ( text model.checklist.name )
        , Input.button
            [ alignRight ]
            { onPress = Just SwipeRight
            , label = icon "chevron_right"
            }
        ]


viewTask : Int -> StorageV1.Task -> Element Msg
viewTask idx submodel =
    row
        [ Event.onClick (EditTask idx)
        , width fill
        -- FIXME(akavel): why below doesn't work?
        , attrWhen (idx == model.editedTaskIdx)
            -- TODO(akavel): use MDL's primary/accent color
            <| Background.color Color.lightBlue
        -- TODO(akavel): add hairline between tasks somehow; maybe via padding here?
        -- Ideally, make it follow Material Design guidelines (rgba(0,0,0,.12)?)
        ]
        [ paragraph
            [ width fill
            , Font.alignLeft
            , attrWhen submodel.done
                <| Font.color
                <| Color.greyscale 0.2
            ]
            [ text submodel.text ]
        , el
            [ alignRight
            , attrWhen submodel.done
                <| Font.color
                <| Color.greyscale 0.2
            -- If not done, use invisible (transparent) color, but still
            -- display the element, to keep the layout from moving when toggled.
            , attrWhen (not submodel.done)
                <| Font.color
                <| Color.rgba 0 0 0 0
            ]
            ( icon "sentiment_very_satisfied" )
        ]


viewFooter : Model -> Element Msg
viewFooter model =
    if model.editedTaskIdx /= newTaskIdx
    then
        column []
            [ viewEditActions model
            , Input.text []
                { onChange = Just EditTaskText
                , text = model.editedTaskText
                , placeholder = Nothing
                , label = Input.labelLeft [] <| none
                }
            ]
    else
        row [ width fill ]
            [ Input.text
                [ width fill ]
                { onChange = Just EditTaskText
                , text = model.editedTaskText
                , placeholder = Just <| Input.placeholder
                    [ Font.color Color.gray
                    , Font.alignLeft
                    , Font.italic
                    ]
                    <| icon "create"
                , label = Input.labelLeft [] <| none
                }
            , Input.button
                [ mdl ["button", "js-button", "button--fab", "button--colored", "js-ripple-effect"]
                -- NOTE(akavel): without height, stylish-elefants makes button disappear
                , height (px 56)
                , disabledWhen (String.trim model.editedTaskText == "")
                , alignRight
                ]
                -- FIXME(akavel): why below `if` doesn't work to make the element disabled?
                { onPress = if (String.trim model.editedTaskText == "")
                    then Nothing
                    else Just SaveEdit
                , label = icon "add"
                }
            ]


viewEditActions : Model -> Element Msg
viewEditActions model =
    let
        originalTask =
            model.checklist.tasks
            |> nth model.editedTaskIdx
        originalText =
            originalTask
            |> Maybe.map .text
        isNotEdited =
            originalText == Just model.editedTaskText
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
                , Background.color
                    <| withAlpha grayedOutAlpha
                    <| Color.gray
                , Event.onClick (AskDeleteTask False)
                ]
                [ Input.button
                    [ alignLeft
                    , mdl ["button", "js-button", "button--fab"]
                    , Background.color <| Color.rgb 200 200 200
                    ]
                    { onPress = Just CancelEdit
                    , label = icon "close"
                    }
                , el
                    [ centerX
                    , attrWhen model.askingTaskDeletion
                        <| above
                        <| column []
                            [ Input.button
                                [ Background.color Color.white
                                , padding 8
                                , Border.rounded 3
                                , mdl ["shadow--2dp"]
                                ]
                                { onPress = Just DeleteTask
                                , label = row []
                                    [ icon "delete_forever"
                                    , text "Delete task?"
                                    ]
                                }
                            ]
                    , onClickNoBubble (AskDeleteTask True)
                    , pointer
                    ]
                    (icon "delete")
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
                , Background.color
                    <| withAlpha grayedOutAlpha
                    <| Color.gray
                , Event.onClick (AskDeleteTask False)
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
                    , disabledWhen (String.trim model.editedTaskText == "")
                    ]
                    { onPress = Just SaveEdit
                    , label = icon "check"
                    }
                ]


viewChecklistMenuLayer : String -> Element Msg
viewChecklistMenuLayer checklistName =
    scrim
        { alpha = 0.01
        , onClick = Just (ShowChecklistMenu False)
        , top = (px 32)
        , right = fill
        , bottom = fill
        , left = fill
        }
        <| column
            [ Background.color Color.white
            , Font.color Color.black
            , padding 8
            , spacing 8
            , Border.rounded 3
            , mdl ["shadow--2dp"]
            -- TODO(akavel): below don't work; how to make width wrap children when parent element has centerX ?
            -- , width fill
            -- , height fill
            ]
            [ row
                [ Event.onClick <| AddChecklist
                -- TODO(akavel): why spacing doesn't do anything here? :/ fix this somehow
                -- , spacing 8
                ]
                [ icon "add" -- FIXME(akavel): appropriate icon
                , text "New list..."
                ]
            , row
                [ Event.onClick <| EditChecklistName <| Just checklistName
                ]
                [ icon "edit"
                , text "Rename list..."
                ]
            -- TODO(akavel): add menu separator here
            , row
                [ Event.onClick <| VerifyDeleteChecklist
                ]
                [ icon "delete"
                , text "Delete list..."
                ]
            ]


viewChecklistNameEditor : String -> Element Msg
viewChecklistNameEditor name =
    scrim
        { alpha = grayedOutAlpha
        , onClick = Nothing
        , top = (px 0)
        , right = (px 0)
        , bottom = fill
        , left = (px 0)
        }
        <| Input.text
            [ width fill
            , mdl ["shadow--2dp", "color--primary", "color-text--primary-contrast"]
            , height (px 40)
            , below
                <| row
                    [ width fill ]
                    [ Input.button
                        [ alignLeft
                        , mdl ["button", "js-button", "button--fab"]
                        , Background.color <| Color.rgb 200 200 200
                        ]
                        { onPress = Just (EditChecklistName Nothing)
                        , label = icon "close"
                        }
                    , Input.button
                        [ alignRight
                        , mdl ["button", "js-button", "shadow--4dp",
                            "button--fab", "button--colored", "button--primary"]
                        -- NOTE(akavel): without height, stylish-elefants makes button disappear
                        , height (px 56)
                        , disabledWhen (String.trim name == "")
                        ]
                        { onPress = if (String.trim name == "")
                            then Nothing
                            else Just SaveChecklistName
                        , label = icon "done"
                        }
                    ]
            ]
            { onChange = Just (\s -> EditChecklistName (Just s))
            , text = name
            , placeholder = Nothing
            , label = Input.labelLeft [] <| none
            }


---- STYLES ----


grayedOutAlpha = 0.75


---- UTILS ----


nth : Int -> List a -> Maybe a
nth n list =
    List.head <| List.drop n list


capitalizeLeft : String -> String
capitalizeLeft s =
    (s |> String.left 1 |> String.toUpper)
    ++
    (s |> String.dropLeft 1)


withAlpha : Float -> Color -> Color
withAlpha alpha color =
    let
        c = Color.toRgb color
    in
        Color.rgba c.red c.green c.blue alpha


attrWhen : Bool -> Attribute msg -> Attribute msg
attrWhen condition attr =
    if condition
    then attr
    -- else inlineStyle []
    -- TODO(akavel): try to find a better NOP attribute
    else scale 1.0


elemWhen : Bool -> Element msg -> Element msg
elemWhen condition elem =
    if condition
    then elem
    else none


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


-- NOTE: code from mdgriffith (https://github.com/mdgriffith/stylish-elephants/issues/90#issuecomment-395070073)
onClickNoBubble : msg -> Attribute msg
onClickNoBubble msg =
    htmlAttribute
    <| Html.Events.onWithOptions "click"
        { stopPropagation = True
        , preventDefault = False
        }
    <| Json.Decode.succeed msg


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
        | editedTaskIdx = newTaskIdx
        , editedTaskText = ""
        , askingTaskDeletion = False
        }


modifyNthTask : Int -> (StorageV1.Task -> Maybe StorageV1.Task) -> Model -> Model
modifyNthTask n modifier model =
    let
        newModel =
            model |> Focus.update (checklist => tasks) listModifier
        listModifier tasks =
            tasks
            |> List.indexedMap idxModifier
            |> List.filterMap identity
        idxModifier idx task =
            if idx == n
            then (task |> modifier)
            else Just task
    in newModel


-- scrim builds a partially transparent surrounding area around an element,
-- which intercepts user clicks
-- TODO(akavel): with fully 0 alpha, onClick doesn't seem to work :(
type alias Scrim msg =
    { alpha : Float
    , onClick : Maybe msg
    , top : Length
    , right : Length
    , bottom : Length
    , left : Length
    }
scrim : Scrim msg -> Element msg -> Element msg
scrim config contents =
    let
        stretcher w h =
            el
                [ Background.color Color.gray
                , alpha config.alpha
                , width w
                , height h
                , config.onClick
                    |> Maybe.map (\msg -> Event.onClick msg)
                    |> Maybe.withDefault (scale 1)
                ]
                none
    in
        column [ height fill, width fill ]
            [ stretcher fill config.top
            , row [ width fill ]
                [ stretcher config.left fill
                , contents
                , stretcher config.right fill
                ]
            , stretcher fill config.bottom
            ]

