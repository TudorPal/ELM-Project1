module Model.Date exposing (Date, Month(..), compare, compareMonth, full, month, monthToString, monthsBetween, monthsBetweenMonths, offsetMonths, onlyYear, view, year)

import Html exposing (..)
import Model.Util exposing (chainCompare)
import Html.Events exposing (..)
import Html.Attributes exposing (class)

type Date
    = Date { year : Int, month : Maybe Month }


year : Date -> Int
year (Date d) =
    d.year


month : Date -> Maybe Month
month (Date d) =
    d.month


full : Int -> Month -> Date
full y m =
    Date { year = y, month = Just m }


onlyYear : Int -> Date
onlyYear y =
    Date { year = y, month = Nothing }


{-| Given two `Date`s it returns the number of months between the two dates as an **absolute** value.
The month fields are handled as follows:

  - If both are present (`Just`), they are included normally in the calculation
  - If both are missing (`Nothing`), the number of years between the two dates is calculated
  - Otherwise the result is undefined (`Nothing`)

```
    monthsBetween (full 2020 Jan) (full 2020 Feb) --> Just 1
    monthsBetween (full 2020 Mar) (full 2020 Jan) --> Just 2
    monthsBetween (full 2020 Jan) (full 2020 Dec) --> Just 11

    monthsBetween (full 2020 Jan) (full 2021 Feb) --> Just 13
    monthsBetween (full 2021 Jan) (full 2020 Feb) --> Just 11

    monthsBetween (onlyYear 2020) (full 2021 Jan) --> Nothing
    monthsBetween (full 2020 Jan) (onlyYear 2021) --> Nothing

    monthsBetween (full 2020 Dec) (full 2021 Jan) --> Just 1

    monthsBetween (onlyYear 2020) (onlyYear 2021) --> Just 12
    monthsBetween (onlyYear 2020) (onlyYear 2022) --> Just 24
```

-}
monthsBetween : Date -> Date -> Maybe Int
monthsBetween dA dB =
    case ( month dA, month dB ) of
        ( Just mA, Just mB ) ->
            Just <| abs <| (year dA * 12 + monthToInt mA) - (year dB * 12 + monthToInt mB)  -- Ambele au month, deci se pot calcula folosind montshbetweenMonths

        ( Nothing, Nothing ) ->
            Just <| abs (year dA - year dB) * 12  -- Ambele au doar year, deci se calculeaza diferenta de ani inmultita cu 12

        _ ->
            Nothing -- Daca una are month si una nu, rezultatul este nedefinit


{-| Compares two dates.
First, dates are compared by the year field. If it's equal, the month fields are used as follows:

  - If both are present (`Just`), they are compared the result is returned
  - If both are missing (`Nothing`), the dates are equal
  - Otherwise the date without month is greater

```
    Model.Date.compare (full 2020 Jan) (full 2021 Jan) --> LT
    Model.Date.compare (full 2021 Dec) (full 2021 Jan) --> GT

    Model.Date.compare (full 2020 Jan) (full 2020 Dec) --> LT
    Model.Date.compare (onlyYear 2020) (onlyYear 2021) --> LT

    Model.Date.compare (onlyYear 2020) (full 2020 Dec) --> GT
    Model.Date.compare (onlyYear 2019) (full 2020 Dec) --> LT
```

-}
compare : Date -> Date -> Order
compare (Date d1) (Date d2) =
    chainCompare (compareMonth d1.month d2.month) (compareYear d1.year d2.year)

{-| Given a current date and the number of months, it returns a new date with the given number of months passed.

-}
offsetMonths : Int -> Date -> Date
offsetMonths months (Date d) =
    let
        addMonths =
            modBy 12 months

        addedMonths =
            d.month
                |> Maybe.map monthToInt
                |> Maybe.map ((+) addMonths)

        newMonth =
            addedMonths
                |> Maybe.map (modBy 12)
                |> Maybe.andThen intToMonth

        addYears =
            months // 12

        extraYear =
            if Maybe.withDefault 0 addedMonths >= 12 then
                1

            else
                0
    in
    Date { year = d.year + addYears + extraYear, month = newMonth }


view : Date -> Html msg
view (Date d) =
    let
        monthString =
            d.month
                |> Maybe.map monthToString      -- daca month e Just, atunci se aplica functia monthToString
                |> Maybe.withDefault ""         -- daca month e Nothing, atunci valoarea este ""
    in
        div[class "date"]
            [ text <| String.fromInt d.year ++ " " ++ monthString ]
    -- Debug.todo "Implement Model.Date.view"


-- MONTH


type Month
    = Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec


intToMonth : Int -> Maybe Month
intToMonth idx =
    case idx of
        0 ->
            Just Jan

        1 ->
            Just Feb

        2 ->
            Just Mar

        3 ->
            Just Apr

        4 ->
            Just May

        5 ->
            Just Jun

        6 ->
            Just Jul

        7 ->
            Just Aug

        8 ->
            Just Sep

        9 ->
            Just Oct

        10 ->
            Just Nov

        11 ->
            Just Dec

        _ ->
            Nothing


monthToInt : Month -> Int
monthToInt m =
    case m of
        Jan ->
            0

        Feb ->
            1

        Mar ->
            2

        Apr ->
            3

        May ->
            4

        Jun ->
            5

        Jul ->
            6

        Aug ->
            7

        Sep ->
            8

        Oct ->
            9

        Nov ->
            10

        Dec ->
            11


monthToString : Month -> String
monthToString m =
    case m of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


-- functie pentru a verifica care dintre doua luni este mai mare
compareMonth : Maybe Month -> Maybe Month -> Order
compareMonth m1 m2 =
    case ( m1, m2 ) of
        ( Just mA, Just mB ) ->
            if monthToInt mA < monthToInt mB then
                LT
            else
                GT

        ( Nothing, Nothing ) ->
            EQ

        ( Just _, Nothing ) -> -- daca una are month si una nu, atunci cea care nu are month este mai mare
            LT

        ( Nothing, Just _ ) ->
            GT

-- functie pentru a verifica care dintre ani este mai mare
compareYear : Int -> Int -> Order
compareYear y1 y2 =
    if y1 < y2 then
        LT
    else
        if y1 > y2 then
            GT
        else
            EQ


{-| Returns the number of months between two months as an **absolute** value.

    monthsBetweenMonths Jan Jan --> 0

    monthsBetweenMonths Jan Apr --> 3

    monthsBetweenMonths Apr Jan --> 3

-}
monthsBetweenMonths : Month -> Month -> Int
monthsBetweenMonths m1 m2 =
    abs (monthToInt m1 - monthToInt m2)
    -- Debug.todo "Implement Date.monthsBetweenMonths"
