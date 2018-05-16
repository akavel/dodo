port module Main exposing (..)

import Html exposing (Html)
-- (internal modules)
import Slit exposing (Slit)
import StorageV0
import StorageV1
import DefaultPage


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = (model, StorageV0.load ())
        , update = update
        -- , subscriptions = always Sub.none
        , subscriptions = subscriptions
        }


---- MODEL ----


type alias Model =
    { currentPage : CurrentPage
    -- TODO(akavel): promote data from subpage into main storage, and reverse
    , storage : StorageV1.Model
    }

type CurrentPage
    = OnDefaultPage DefaultPage.Model

model : Model
model =
    { currentPage = OnDefaultPage DefaultPage.model
    , storage =
        Slit.base <| StorageV1.Checklist "New List 0"
            [ StorageV1.Task "Foo" False
            , StorageV1.Task "Bar" True
            ]
    }


---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.currentPage of
        OnDefaultPage submodel ->
            DefaultPage.subscriptions submodel
            |> Sub.map DefaultPageMsg


---- UPDATE ----


{-
We need to be able to:
- (done) add new items to the current list
- (done) remove item from current list
- (done) disable/reenable item from current list (toggle)
- (done) edit an item on the list
- (done) save items on disk and load them on app start
- TODO: show names of all lists
- TODO: change the current list to a different one
- TODO: add new list
- TODO: delete a list
- TODO: rename current list
- TODO: copy (duplicate) a list with a new name
- TODO: clear (hide) all disabled items
- TODO: move items around the list
-}
type Msg
    = DefaultPageMsg DefaultPage.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (msg, model.currentPage) of
        (DefaultPageMsg submsg, OnDefaultPage submodel) ->
            let
                freshSubmodel =
                    { submodel | checklist = Slit.peek model.storage }
                (pagemodel, pagemsg) =
                    DefaultPage.update submsg freshSubmodel
                newmodel =
                    { model
                        | currentPage = OnDefaultPage pagemodel
                        , storage = model.storage |> Slit.poke pagemodel.checklist
                    }
                newmsg =
                    case pagemsg of
                        DefaultPage.Please cmd ->
                            Cmd.map DefaultPageMsg cmd
                        DefaultPage.PleaseSwipeLeft ->
                            Cmd.none
                        DefaultPage.PleaseSwipeRight ->
                            Cmd.none
            in
                (newmodel, newmsg)
            -- |> Tuple.mapFirst (\submodel -> { model | currentPage = OnDefaultPage submodel })
            -- |> Tuple.mapSecond (Cmd.map DefaultPageMsg)


---- VIEW ----


view : Model -> Html Msg
view model =
    case model.currentPage of
        OnDefaultPage submodel ->
            DefaultPage.view submodel
            |> Html.map DefaultPageMsg

