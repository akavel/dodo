module ListsPage exposing (..)

import Html exposing (Html, div, text, p, header, footer, main_)
import Html.Attributes exposing (class, classList, style)
import Html.Events
-- debois/elm-mdl — Material Design Lite (MDL)
import Material
import Material.Scheme
import Material.Options as Options exposing (cs)
import Material.List as Lists
-- (internal modules)
import Slit
import StorageV1


---- MODEL ----


type alias Model =
    { lists : StorageV1.Model
    , mdl : Material.Model  -- MDL boilerplate
    }


model : Model
model =
    { lists = Slit.fromElement <| StorageV1.Checklist "New List 0" []
    , mdl = Material.model  -- MDL boilerplate
    }


---- UPDATE ----


type Msg
    = Select Int
    | Mdl (Material.Msg Msg)  -- MDL boilerplate


-- Plea is a request to parent view to execute the specified message
type Plea
    = Please (Cmd Msg)
    | PleaseSwipeRight
    | PleaseSave


update : Msg -> Model -> ( Model, Plea )
update msg model =
    case msg of

        Select idx ->
            let
                newmodel =
                    { model
                        | lists = model.lists |> Slit.scrollTo idx
                    }
            in
                ( newmodel, PleaseSwipeRight )

        Mdl msg_ ->
            Material.update Mdl msg_ model
            |> Tuple.mapSecond Please


---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


---- VIEW ----


type alias Mdl = Material.Model

view : Model -> Html Msg
view model =
    div [ class "app-layout" ]
        [ text "hello lists"
        , Lists.ul []
            (List.indexedMap viewItem <| Slit.toList model.lists)
        ]
    |> Material.Scheme.top


viewItem : Int -> StorageV1.Checklist -> Html Msg
viewItem idx submodel =
    Lists.li
        -- TODO(akavel): verify if divider is styled OK w.r.t. Material Design
        -- see: https://github.com/google/material-design-lite/pull/1785/files
        [ Options.css "border-bottom" "1px solid rgba(0,0,0, 0.12)"
        , Options.attribute <| Html.Events.onClick (Select idx)
        -- FIXME(akavel): why below doesn't work?
        -- , Options.when (model.editTask && idx == model.editTaskIdx)
        --     <| Color.background Color.primary
        ]
        [ Lists.content
            [ Options.css "text-align" "left"
            ]
            [ text (submodel.name) ]
        ]

