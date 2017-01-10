module Misc exposing (..)


(+|+) : Bool -> List a -> List a -> List a
(+|+) append xs2 xs1 =
    if append then
        xs1 ++ xs2
    else
        xs1
