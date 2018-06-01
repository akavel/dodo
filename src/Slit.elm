module Slit exposing (Slit, fromElement, peek, poke, position, toList, fromList, scroll, scrollTo, addAfter)

type Slit a =
    Slit
        -- !!! IMPORTANT NOTE: to make scroll easier, the pre list has REVERSED ORDER
        { pre : List a
        , mid : a
        , post : List a
        }

fromElement : a -> Slit a
fromElement a =
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

position : Slit a -> Int
position (Slit slit) =
    List.length slit.pre

toList : Slit a -> List a
toList slit =
    let
        pos =
            position slit
        (Slit scrolled) =
            scroll (-pos) slit
    in
        scrolled.mid :: scrolled.post

fromList : List a -> Maybe (Slit a)
fromList list =
    case list of
        a :: more ->
            Just (Slit
                { pre = []
                , mid = a
                , post = more
                })
        [] ->
            Nothing

-- FIXME(akavel): optimize
scroll : Int -> Slit a -> Slit a
scroll by (Slit slit) =
    case (compare by 0, slit.pre, slit.post) of
        -- Positive scroll (by>0)
        (GT, pre, a :: post) ->
            scroll (by-1) (Slit
                -- NOTE: pre is reversed
                { pre = slit.mid :: pre
                , mid = a
                , post = post
                })
        (GT, _, []) ->
            -- cannot scroll anymore
            Slit slit
        -- No scroll (by==0)
        (EQ, _, _) ->
            Slit slit
        -- Negative scroll (by<0)
        (LT, a :: pre, post) ->
            scroll (by+1) (Slit
                -- NOTE: pre is reversed
                { pre = pre
                , mid = a
                , post = slit.mid :: post
                })
        (LT, [], _) ->
            -- cannot scroll anymore
            Slit slit

scrollTo : Int -> Slit a -> Slit a
scrollTo idx slit =
    let
        old = position slit
    in
        scroll (idx - old) slit

addAfter : a -> Slit a -> Slit a
addAfter a (Slit slit) =
    Slit
        -- NOTE: pre is reversed
        { pre = slit.mid :: slit.pre
        , mid = a
        , post = slit.post
        }

