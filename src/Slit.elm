module Slit exposing (Slit, base, peek, poke)

type Slit a =
    Slit
        { pre : List a
        , mid : a
        , post : List a
        }

base : a -> Slit a
base a =
    Slit
        { pre = []
        , mid = a
        , post = []
        }

peek : Slit a -> a
peek (Slit slit) =
    slit.mid

poke : a -> Slit a -> Slit a
poke mid (Slit slit) =
    Slit { slit | mid = mid }
