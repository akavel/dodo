module Slit exposing (..)

type alias Slit a =
    { pre : List a
    , mid : a
    , post : List a
    }

base : a -> Slit a
base a =
    Slit [] a []

