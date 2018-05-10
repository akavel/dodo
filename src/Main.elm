module Main exposing (..)

import Html exposing (Html, div, text, p, header, footer, main_)
import Html.Attributes exposing (class, style)
import Html.Events
-- import Array exposing (Array)
-- debois/elm-mdl - Material Design Lite (MDL)
import Material
import Material.Scheme
-- import Material.Menu as Menu
import Material.Toggles as Toggles
import Material.Elevation as Elevation
import Material.Options as Options
import Material.Color as Color
import Material.Textfield as Textfield
import Material.Button as Button
import Material.Icon as Icon
import Material.Typography as Typo
import Material.List as Lists
import Material.Helpers exposing (pure)
import Dialog
import Flex
import Focus exposing (..)
import String


---- MODEL ----


type alias Model =
    { text : String
    -- , checklists : List Checklist
    , checklist : Checklist
    , newTask : String
    , mdl : Material.Model  -- MDL boilerplate
    , dialogVisible : Dialog.Visible
    , dialogTaskIdx : Int
    , dialogTaskText : String
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
    , dialogVisible = Dialog.hidden
    , dialogTaskIdx = -1
    , dialogTaskText = ""
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
    | SaveEdit
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
            -- TODO handle idx
            pure { model
                | dialogVisible = True
                , dialogTaskIdx = idx
                , dialogTaskText =
                    Maybe.withDefault "" <| Maybe.map .text <| List.head <| List.drop idx model.checklist.tasks
                }
        SaveEdit ->
            -- TODO
            pure { model |
                dialogVisible = False
                , dialogTaskIdx = -1
                }
        Mdl msg_ ->
            Material.update Mdl msg_ model



---- VIEW ----

type alias Mdl = Material.Model

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
    appLayout
        [ header []
            [ text "hello header" ]
        , tasksLayout
            [ Lists.ul []
                (List.indexedMap viewTask model.checklist.tasks)
            ]
        , footer []
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
        , Dialog.render
            { styles = []
            , title = ""
            , content =
                -- [ Menu.render
                --     Mdl [100, 5] model.mdl  -- MDL boilerplate
                --     [ Menu.bottomRight ]
                --     [ Menu.item []
                --         [ Icon.i "delete_forever", text "Delete" ]
                --     ]
                [ Textfield.render
                    Mdl [100, 0] model.mdl  -- MDL boilerplate
                    [ Textfield.value model.dialogTaskText
                    -- , Options.css "float" "left"
                    -- , Options.onInput DialogEditTask
                    ]
                    []
                -- , Button.render
                --     Mdl [100, 1] model.mdl  -- MDL boilerplate
                --     [ Button.fab
                --     -- [ Button.primary
                --     -- , Button.colored
                --     -- , Button.minifab
                --     -- [ Button.icon
                --     -- , Options.css "float" "left"
                --     ]
                --     [ Icon.i "delete_forever" ]
                , Button.render
                    Mdl [100, 2] model.mdl  -- MDL boilerplate
                    [ Button.colored
                    , Button.accent
                    , Button.fab
                    , Elevation.e4
                    -- , Options.css "float" "right"
                    ]
                    [ Icon.i "sentiment_very_satisfied" ]
                ]
            , actionBar =
                [ Button.render
                    Mdl [100, 90] model.mdl  -- MDL boilerplate
                    [ Button.colored
                    , Button.raised
                    , Button.accent
                    , Options.onClick SaveEdit
                    ]
                    [ text "OK" ]
                , Button.render
                    Mdl [100, 91] model.mdl  -- MDL boilerplate
                    [ Button.colored
                    , Button.raised
                    , Button.primary
                    , Options.onClick SaveEdit
                    ]
                    [ text "Cancel" ]
                , Toggles.switch
                    Mdl [100, 92] model.mdl  -- MDL boilerplate
                    [ Toggles.value False ]
                    -- [ text "Delete" ]
                    [ Icon.i "delete_forever" ]
                -- [ Html.button
                --     [ Html.Events.onClick SaveEdit
                --     , class "mdl-button mdl-button--raised mdl-button--accent"
                --     ]
                --     [ text "Close" ]
                ]
            }
            model.dialogVisible
        ]
    |> Material.Scheme.top


-- TODO(akavel): make below construction prettier (more regular, less weird operators)
appLayout = div [ style
    <| ("height", "100vh")
    :: Flex.display
    ++ Flex.direction Flex.Vertical
    ]
tasksLayout = main_ [ style
    [ ("height", "100vh")
    , ("overflow-y", "scroll")
    , ("border-bottom", "1px solid rgba(0,0,0,0.12)")  -- FIXME(akavel): use Material Design guidelines
    ] ]


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
