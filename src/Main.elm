port module Main exposing (..)

import Html exposing (Html)
-- (internal modules)
import Slit exposing (Slit)
import StorageV0
import StorageV1
import DefaultPage
import ListsPage


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = (model, load ())
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
    | OnListsPage ListsPage.Model

model : Model
model =
    { currentPage = OnDefaultPage emptyDefaultPage
    , storage = StorageV1.empty
    }

-- TODO(akavel): why { DefaultPage.model | ... } is not allowed?
emptyDefaultPage = DefaultPage.model
emptyListsPage = ListsPage.model


---- UPDATE ----


{-
We need to be able to:
- *done!* add new items to the current list
- *done!* remove item from current list
- *done!* disable/reenable item from current list (toggle)
- *done!* edit an item on the list
- *done!* save items on disk and load them on app start
- *done!* show names of all lists
- TODO: add new list
- TODO: change the current list to a different one
- TODO: delete a list
- TODO: rename current list
- TODO: copy (duplicate) a list with a new name
- TODO: clear (hide) all disabled items
- TODO: move items around the list
- TODO: calendar with done items
-}
type Msg
    = Loaded (Maybe Storage)
    | DefaultPageMsg DefaultPage.Msg
    | ListsPageMsg ListsPage.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (msg, model.currentPage) of
        (Loaded Nothing, _) ->
            update (Loaded (Just emptyStorage)) model
        (Loaded (Just json), _) ->
            let
                -- Try to decode storage.v1;...
                newstorage =
                    json.v1
                    -- TODO(akavel): somehow handle errors in storage decoding
                    |> Maybe.andThen StorageV1.fromJS
                    -- ...if failed, try to upgrade from v0;...
                    |> Maybe.withDefault fromV0
                fromV0 =
                    json.checklist
                    |> Maybe.map StorageV0.Model
                    |> Maybe.map StorageV1.fromV0
                    -- ...if upgrade failed, fall back to default empty contents.
                    |> Maybe.withDefault StorageV1.empty
                -- Inject the loaded data to DefaultPage's model...
                pagemodel =
                    { emptyDefaultPage
                        | checklist = Slit.peek newstorage
                    }
                -- ...and to the top-level model. Also, open the default page.
                newmodel =
                    { currentPage = OnDefaultPage pagemodel
                    , storage = newstorage
                    }
            in
                -- Finally, save the data in upgraded format.
                ( newmodel, saveV1 newmodel.storage )

        -- Handle DefaultPage actions
        (DefaultPageMsg submsg, OnDefaultPage submodel) ->
            let
                (pagemodel, pagemsg) =
                    DefaultPage.update submsg submodel
                newstorage =
                    model.storage |> Slit.poke pagemodel.checklist
                newpage =
                    case pagemsg of
                        DefaultPage.PleaseSwipeLeft ->
                            OnListsPage { emptyListsPage | lists = newstorage }
                        _ ->
                            OnDefaultPage pagemodel
                newmodel =
                    { model
                        | currentPage = newpage
                        -- Make sure any changes made on the page are reflected
                        -- in the high level data.
                        , storage = newstorage
                    }
                    |> if (pagemsg == DefaultPage.PleaseAddChecklist)
                        then addChecklist
                        else identity
                newmsg =
                    case pagemsg of
                        DefaultPage.Please cmd ->
                            Cmd.map DefaultPageMsg cmd
                        DefaultPage.PleaseSave ->
                            save
                                { checklist = Nothing
                                , v1 = Just (StorageV1.toJS newmodel.storage)
                                }
                        DefaultPage.PleaseSwipeLeft ->
                            Cmd.none
                        DefaultPage.PleaseSwipeRight ->
                            Cmd.none
                        DefaultPage.PleaseAddChecklist ->
                            save
                                { checklist = Nothing
                                , v1 = Just (StorageV1.toJS newmodel.storage)
                                }
            in
                (newmodel, newmsg)

        -- TODO(akavel): how to properly handle combination like below?
        (DefaultPageMsg submsg, _) ->
            let
                submodel =
                    { emptyDefaultPage
                        | checklist = Slit.peek model.storage }
            in
                update msg { model | currentPage = OnDefaultPage submodel }

        (ListsPageMsg submsg, OnListsPage submodel) ->
            let
                (pagemodel, pagemsg) =
                    ListsPage.update submsg submodel
                newstorage =
                    pagemodel.lists
                newpage =
                    case pagemsg of
                        ListsPage.PleaseSwipeRight ->
                            OnDefaultPage
                                { emptyDefaultPage | checklist = Slit.peek newstorage }
                        _ ->
                            OnListsPage pagemodel
                newmodel =
                    { model
                        | currentPage = newpage
                        -- Make sure any changes made on the page are reflected
                        -- in the high level data.
                        , storage = newstorage
                    }
                newmsg =
                    case pagemsg of
                        ListsPage.Please cmd ->
                            Cmd.map ListsPageMsg cmd
                        ListsPage.PleaseSave ->
                            save
                                { checklist = Nothing
                                , v1 = Just (StorageV1.toJS newmodel.storage)
                                }
                        ListsPage.PleaseSwipeRight ->
                            save
                                { checklist = Nothing
                                , v1 = Just (StorageV1.toJS newmodel.storage)
                                }
            in
                (newmodel, newmsg)

        -- TODO(akavel): how to properly handle combination like below?
        (ListsPageMsg submsg, _) ->
            let
                submodel =
                    { emptyListsPage
                        | lists = model.storage }
            in
                update msg { model | currentPage = OnListsPage submodel }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        pagesubs =
            case model.currentPage of
                OnDefaultPage submodel ->
                    DefaultPage.subscriptions submodel
                    |> Sub.map DefaultPageMsg
                OnListsPage submodel ->
                    ListsPage.subscriptions submodel
                    |> Sub.map ListsPageMsg
    in
        Sub.batch
            [ pagesubs
            , loaded Loaded
            ]


---- PORTS ----


-- FIXME(akavel): try to use string encoding & decoding, to solve upgrades
-- purely in Elm, without need for help on JS side
type alias Storage =
    { -- Backwards compatibility with data saved via StorageV0
      checklist : Maybe StorageV0.Checklist
      -- StorageV1
    , v1 : Maybe StorageV1.JS
    }

port save : Storage -> Cmd msg
port load : () -> Cmd msg
port loaded : (Maybe Storage -> msg) -> Sub msg

emptyStorage : Storage
emptyStorage =
    { checklist = Nothing
    , v1 = Just <| StorageV1.toJS StorageV1.empty
    }

saveV1 : StorageV1.Model -> Cmd msg
saveV1 v1 =
    save
        { checklist = Nothing
        , v1 = Just <| StorageV1.toJS v1
        }



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.currentPage of
        OnDefaultPage submodel ->
            DefaultPage.view submodel
            |> Html.map DefaultPageMsg
        OnListsPage submodel ->
            ListsPage.view submodel
            |> Html.map ListsPageMsg


---- UTILS ----


addChecklist : Model -> Model
addChecklist model =
    let
        newModel =
            { storage = newStorage
            , currentPage = OnDefaultPage { emptyDefaultPage | checklist = newStorage |> Slit.peek }
            }
        newStorage =
            model.storage
            |> Slit.addAfter (StorageV1.Checklist newName [])
        newName =
            prefix ++ (toString (highest + 1))
        prefix =
            "New List "
        highest =
            model.storage
            |> Slit.toList
            |> fold -1 (\checklist highest ->
                checklist.name
                |> trimPrefix prefix
                |> Result.andThen String.toInt
                |> (\suffix -> case suffix of
                    Ok n ->
                        max n highest
                    Err _ ->
                        highest)
                )
                -- |> Result.map (max highest)
                -- |> Result.withDefault highest
        fold =
            flip List.foldl
        trimPrefix prefix s =
            if String.startsWith prefix s
            then Ok (s |> String.dropLeft (String.length prefix))
            else Err ""
    in newModel

