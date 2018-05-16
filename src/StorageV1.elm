-- Module StorageV1 contains the core part of data model, which needs to be
-- stored in persistent storage, so that it can be restored after the app is
-- closed and reopened.
port module StorageV1 exposing (..)

import Slit exposing (Slit)
import StorageV0

type alias Model =
    Slit Checklist

type alias Checklist =
    { name : String
    , tasks : List Task
    }

type alias Task =
    { text : String
    , done : Bool
    }

empty : Model
empty =
    Slit.fromElement <| Checklist "New List 0" []

type alias JS =
    (Int, List Checklist)

toJS : Model -> JS
toJS model =
    (Slit.position model, Slit.toList model)

fromJS : JS -> Maybe Model
fromJS (pos, list) =
    Slit.fromList list |> Maybe.map (Slit.scroll pos)

fromV0 : StorageV0.Model -> Model
fromV0 v0 =
    Slit.fromElement v0.checklist

