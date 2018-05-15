port module StorageV0 exposing (..)

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

port save : Model -> Cmd msg
port load : () -> Cmd msg
port loaded : (Maybe Model -> msg) -> Sub msg
