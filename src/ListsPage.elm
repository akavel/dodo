module ListsPage exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class, classList, style)
import Html.Events
-- mdgriffith/stylish-elephants — easier building of HTML+CSS layouts
import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
-- import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
-- (internal modules)
import Slit
import StorageV1


---- MODEL ----


type alias Model =
    { lists : StorageV1.Model
    }


model : Model
model =
    { lists = Slit.fromElement <| StorageV1.Checklist "New List 0" []
    }


---- UPDATE ----


type Msg
    = Select Int


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


---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
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
            [ el [ width fill ] ( text "helo lists" )
            , column
                [ height fill
                , width fill
                , spacing 20
                , padding 20
                ]
                ( List.indexedMap viewItem <| Slit.toList model.lists )
            ]


viewItem : Int -> StorageV1.Checklist -> Element Msg
viewItem idx submodel =
    paragraph
        -- TODO(akavel): add divider styled OK w.r.t. Material Design
        -- see: https://github.com/google/material-design-lite/pull/1785/files
        -- Options.css "border-bottom" "1px solid rgba(0,0,0, 0.12)"
        [ Event.onClick (Select idx)
        , width fill
        , Font.alignLeft
        ]
        [ text submodel.name ]

