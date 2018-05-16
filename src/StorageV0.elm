-- Module StorageV0 contains the core part of data model, which needs to be
-- stored in persistent storage, so that it can be restored after the app is
-- closed and reopened.
module StorageV0 exposing (..)

type alias Model =
    { checklist : Checklist
    -- TODO(akavel): add newTask, editTask*
    }

type alias Checklist =
    { name : String
    , tasks : List Task
    }
-- tasks : Focus { r | tasks : a } a
-- tasks = Focus.create .tasks (\f r -> { r | tasks = f r.tasks })

type alias Task =
    { text : String
    , done : Bool
    }

-- port save : Model -> Cmd msg
-- port load : () -> Cmd msg
-- port loaded : (Maybe Model -> msg) -> Sub msg
