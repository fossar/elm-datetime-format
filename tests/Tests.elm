module Tests exposing (all)

import Test exposing (..)
import Fuzz exposing (..)
import Expect
import Time.Date exposing (Weekday(..))
import Time.DateTime.Format as Format exposing ((<>))
import Time.DateTime as DateTime exposing (DateTime)


all : Test
all =
    concat
        [ describe "Simple formatters"
            [ fuzz dateTime "weekdayIndex" <|
                \dt ->
                    (Format.weekdayIndex dt) |> Expect.equal (weekdayToIndex (DateTime.weekday dt))
            , fuzz dateTime "dayPadded" <|
                \dt ->
                    (Format.dayPadded dt) |> Expect.equal (String.padLeft 2 '0' (toString (DateTime.day dt)))
            , fuzz dateTime "dayUnpadded" <|
                \dt ->
                    (Format.dayUnpadded dt) |> Expect.equal (toString (DateTime.day dt))
            , fuzz dateTime "monthPadded" <|
                \dt ->
                    (Format.monthPadded dt) |> Expect.equal (String.padLeft 2 '0' (toString (DateTime.month dt)))
            , fuzz dateTime "monthUnpadded" <|
                \dt ->
                    (Format.monthUnpadded dt) |> Expect.equal (toString (DateTime.month dt))
            , fuzz dateTime "yearShort" <|
                \dt ->
                    (Format.yearShort dt) |> Expect.equal (String.padLeft 2 '0' (toString (DateTime.year dt % 100)))
            , fuzz dateTime "yearFull" <|
                \dt ->
                    (Format.yearFull dt) |> Expect.equal (toString (DateTime.year dt))
            , fuzz dateTime "hour" <|
                \dt ->
                    (Format.hour dt) |> Expect.equal (String.padLeft 2 '0' (toString (DateTime.hour dt)))
            , fuzz dateTime "minute" <|
                \dt ->
                    (Format.minute dt) |> Expect.equal (String.padLeft 2 '0' (toString (DateTime.minute dt)))
            , fuzz dateTime "second" <|
                \dt ->
                    (Format.second dt) |> Expect.equal (String.padLeft 2 '0' (toString (DateTime.second dt)))
            , fuzz dateTime "millisecondPadded" <|
                \dt ->
                    (Format.millisecondPadded dt) |> Expect.equal (String.padLeft 3 '0' (toString (DateTime.millisecond dt)))
            , fuzz dateTime "millisecondUnpadded" <|
                \dt ->
                    (Format.millisecondUnpadded dt) |> Expect.equal (toString (DateTime.millisecond dt))
            , fuzz (tuple ( dateTime, string )) "s" <|
                \( dt, str ) ->
                    (Format.s str dt) |> Expect.equal str
            ]
        , describe "Composition properties"
            [ fuzz (tuple3 ( formatter, formatter, dateTime )) "(f <> g) dt == f dt ++ g dt" <|
                \( f, g, dt ) ->
                    ((f <> g) dt) |> Expect.equal (f dt ++ g dt)
            , fuzz (tuple4 ( formatter, formatter, formatter, dateTime )) "associativity" <|
                \( f, g, h, dt ) ->
                    (((f <> g) <> h) dt) |> Expect.equal ((f <> (g <> h)) dt)
            ]
        , describe "Complex formatters"
            [ fuzz dateTime "sqlDateTime" <|
                \dt ->
                    let
                        date =
                            toString (DateTime.year dt) ++ "-" ++ String.padLeft 2 '0' (toString (DateTime.month dt)) ++ "-" ++ String.padLeft 2 '0' (toString (DateTime.day dt))

                        time =
                            String.padLeft 2 '0' (toString (DateTime.hour dt)) ++ ":" ++ String.padLeft 2 '0' (toString (DateTime.minute dt)) ++ ":" ++ String.padLeft 2 '0' (toString (DateTime.second dt))

                        expected =
                            date ++ " " ++ time
                    in
                        (Format.sqlDateTime dt) |> Expect.equal expected
            ]
        ]



-- HELPERS


mkDateTime : Int -> Int -> Int -> Int -> Int -> Int -> Int -> DateTime
mkDateTime day month year hour minute second millisecond =
    DateTime.dateTime
        { day = day
        , month = month
        , year = year
        , hour = hour
        , minute = minute
        , second = second
        , millisecond = millisecond
        }


weekdayToIndex : Weekday -> String
weekdayToIndex weekday =
    case weekday of
        Mon ->
            "1"

        Tue ->
            "2"

        Wed ->
            "3"

        Thu ->
            "4"

        Fri ->
            "5"

        Sat ->
            "6"

        Sun ->
            "0"



-- FUZZERS


dateTime : Fuzzer DateTime
dateTime =
    let
        day =
            intRange 1 31

        month =
            intRange 1 12

        year =
            intRange 1700 10000

        hour =
            intRange 0 59

        minute =
            intRange 0 59

        second =
            intRange 0 59

        millisecond =
            intRange 0 999
    in
        map mkDateTime day
            |> andMap month
            |> andMap year
            |> andMap hour
            |> andMap minute
            |> andMap second
            |> andMap millisecond


formatter : Fuzzer (DateTime -> String)
formatter =
    frequencyOrCrash
        [ ( 1, constant Format.weekdayIndex )
        , ( 1, constant Format.dayPadded )
        , ( 1, constant Format.dayUnpadded )
        , ( 1, constant Format.monthPadded )
        , ( 1, constant Format.monthUnpadded )
        , ( 1, constant Format.yearShort )
        , ( 1, constant Format.yearFull )
        , ( 1, constant Format.hour )
        , ( 1, constant Format.minute )
        , ( 1, constant Format.second )
        , ( 1, constant Format.millisecondPadded )
        , ( 1, constant Format.millisecondUnpadded )
        ]
