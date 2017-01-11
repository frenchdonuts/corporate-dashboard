module Misc exposing (..)

import Date


(+|+) : Bool -> List a -> List a -> List a
(+|+) append xs2 xs1 =
    if append then
        xs1 ++ xs2
    else
        xs1


toMonth : Int -> Date.Month
toMonth i =
    if i % 12 == 0 then
        Date.Dec
    else if i % 11 == 0 then
        Date.Nov
    else if i % 10 == 0 then
        Date.Oct
    else if i % 9 == 0 then
        Date.Sep
    else if i % 8 == 0 then
        Date.Aug
    else if i % 7 == 0 then
        Date.Jul
    else if i % 6 == 0 then
        Date.Jun
    else if i % 5 == 0 then
        Date.May
    else if i % 4 == 0 then
        Date.Apr
    else if i % 3 == 0 then
        Date.Mar
    else if i % 2 == 0 then
        Date.Feb
    else
        Date.Jan


monthToInt : Date.Month -> Int
monthToInt month =
    case month of
        Date.Jan ->
            1

        Date.Feb ->
            2

        Date.Mar ->
            3

        Date.Apr ->
            4

        Date.May ->
            5

        Date.Jun ->
            6

        Date.Jul ->
            7

        Date.Aug ->
            8

        Date.Sep ->
            9

        Date.Oct ->
            10

        Date.Nov ->
            11

        Date.Dec ->
            12


msInMonth : Float
msInMonth =
    1000 * 60 * 60 * 24 * 30
