module ListsPage exposing (..)

import Html exposing (Html, div, text, p, header, footer, main_)
import Html.Attributes exposing (class, classList, style)
-- debois/elm-mdl — Material Design Lite (MDL)
import Material
import Material.Scheme
import Material.Options as Options exposing (cs)
import Material.Button as Button
import Material.Icon as Icon
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
    = SwipeRight
    | Mdl (Material.Msg Msg)  -- MDL boilerplate


-- Plea is a request to parent view to execute the specified message
type Plea
    = Please (Cmd Msg)
    | PleaseSwipeRight
    | PleaseSave


update : Msg -> Model -> ( Model, Plea )
update msg model =
    case msg of

        SwipeRight ->
            ( model, PleaseSwipeRight )

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
        , Button.render
            Mdl [10, 1] model.mdl  -- MDL boilerplate
            [ Options.onClick SwipeRight ]
            [ Icon.i "chevron_right" ]
        ]
    |> Material.Scheme.top

