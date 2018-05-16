-- Module StorageV1 contains the core part of data model, which needs to be
-- stored in persistent storage, so that it can be restored after the app is
-- closed and reopened.
port module StorageV1 exposing (..)

import Slit exposing (Slit)

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

