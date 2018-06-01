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
    , editedTaskText : String
    , editedTaskIdx : Int
    , verifyingTaskDeletion : Bool
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
    , verifyingTaskDeletion = False
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
                capitalizeLeft s =
                    (s |> String.left 1 |> String.toUpper)
                    ++
                    (s |> String.dropLeft 1)
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
        VerifyDeleteTask ->
            ( { model | verifyingTaskDeletion = True }, Please Cmd.none )
        DeleteTask False ->
            ( model |> stopEdit, Please Cmd.none )
        DeleteTask True ->
            let
                newModel =
                    model |> stopEdit |> modifyNthTask model.editedTaskIdx
                        (\task -> Nothing)
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
            -- Show modal dialog for editing a task, if needed
            , attrWhen (model.editedTaskIdx /= newTaskIdx)
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
                        none
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
                (List.indexedMap viewTask model.checklist.tasks)
            , viewFooter model
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
        , el [ centerX ] (text model.checklist.name)
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
        Input.text
            [ above (viewEditActions model) ]
            { onChange = Just EditTaskText
            , text = model.editedTaskText
            , placeholder = Nothing
            -- , placeholder = Just <| Input.placeholder [] <| text "Edit task"
            -- , label = Input.labelLeft [] <| text "Edit"
            -- , label = Input.labelAbove [] <| text "Edit task"
            , label = Input.labelLeft [] <| none
            -- , label = Input.labelAbove [] <| none
            }
    else
        row [ width fill ]
            [ Input.text
                [ width fill ]
                { onChange = Just EditTaskText
                , text = model.editedTaskText
                -- , placeholder = Nothing
                , placeholder = Just <| Input.placeholder
                    [ Font.color Color.gray
                    , Font.alignLeft
                    , Font.italic
                    ]
                    -- <| text "..."
                    <| icon "create"
                , label = Input.labelLeft [] <| none
                -- , label = Input.labelLeft [] <| text "New"
                -- , label = Input.labelAbove [] <| text "New task"
                -- , label = Input.labelAbove [] <| none
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
                    , attrWhen model.verifyingTaskDeletion
                        <| above
                        <| column []
                            -- [ Background.color Color.white
                            -- , padding 10
                            -- , width shrink
                            -- , height shrink
                            -- ]
                            [ Input.button
                                [ Background.color Color.white
                                , padding 8
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
                    , disabledWhen (String.trim model.editedTaskText == "")
                    ]
                    { onPress = Just SaveEdit
                    , label = icon "check"
                    }
                ]


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

