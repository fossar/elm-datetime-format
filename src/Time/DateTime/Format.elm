module Time.DateTime.Format
    exposing
        ( Formatter
        , (<>)
        , compose
        , weekdayIndex
        , dayPadded
        , dayUnpadded
        , monthPadded
        , monthUnpadded
        , yearShort
        , yearFull
        , hour
        , minute
        , second
        , millisecondPadded
        , millisecondUnpadded
        , s
        , sqlDateTime
        )

{-| Type-safe composable `DateTime` formatters

@docs Formatter

## Composition
@docs compose, (<>)

## Simple formatters
@docs weekdayIndex, dayPadded, dayUnpadded, monthPadded, monthUnpadded, yearShort, yearFull, hour, minute, second, millisecondPadded, millisecondUnpadded, s

## Complex formatters
@docs sqlDateTime
-}

import Time.Date exposing (Weekday(..))
import Time.DateTime as DateTime exposing (DateTime)


{-| Formatter is simply a function that converts a `DateTime` object into a `String`.
-}
type alias Formatter =
    DateTime -> String


{-| Compose two formatters together.
The result will be a new formatter, with the same output as the outputs of the two original formatters concatenated.
-}
compose : Formatter -> Formatter -> Formatter
compose f g datetime =
    f datetime ++ g datetime


{-| Infix variant of the composition function
-}
(<>) : Formatter -> Formatter -> Formatter
(<>) =
    compose
infixr 8 <>


weekdayToIndex : Weekday -> Int
weekdayToIndex weekday =
    case weekday of
        Mon ->
            1

        Tue ->
            2

        Wed ->
            3

        Thu ->
            4

        Fri ->
            5

        Sat ->
            6

        Sun ->
            0


{-| Placeholder for the index of a week day; 0 is Sunday, 6 is Saturday
-}
weekdayIndex : Formatter
weekdayIndex datetime =
    toString (weekdayToIndex (DateTime.weekday datetime))


{-| Placeholder for the day of the month as a zero-padded decimal number
-}
dayPadded : Formatter
dayPadded datetime =
    String.padLeft 2 '0' (toString (DateTime.day datetime))


{-| Placeholder for the day of the month as a decimal number
-}
dayUnpadded : Formatter
dayUnpadded datetime =
    toString (DateTime.day datetime)


{-| Placeholder for the month as a zero-padded decimal number
-}
monthPadded : Formatter
monthPadded datetime =
    String.padLeft 2 '0' (toString (DateTime.month datetime))


{-| Placeholder for the month as a decimal number
-}
monthUnpadded : Formatter
monthUnpadded datetime =
    toString (DateTime.month datetime)


{-| Placeholder for the two-digit form of a year
-}
yearShort : Formatter
yearShort datetime =
    String.padLeft 2 '0' (toString (DateTime.year datetime % 100))


{-| Placeholder for the full year
-}
yearFull : Formatter
yearFull datetime =
    String.padLeft 4 '0' (toString (DateTime.year datetime))


{-| Placeholder for the hour as a zero-padded decimal number
-}
hour : Formatter
hour datetime =
    String.padLeft 2 '0' (toString (DateTime.hour datetime))


{-| Placeholder for the minute as a zero-padded decimal number
-}
minute : Formatter
minute datetime =
    String.padLeft 2 '0' (toString (DateTime.minute datetime))


{-| Placeholder for the second as a zero-padded decimal number
-}
second : Formatter
second datetime =
    String.padLeft 2 '0' (toString (DateTime.second datetime))


{-| Placeholder for the millisecond as a zero-padded decimal number
-}
millisecondPadded : Formatter
millisecondPadded datetime =
    String.padLeft 3 '0' (toString (DateTime.millisecond datetime))


{-| Placeholder for the millisecond as a decimal number
-}
millisecondUnpadded : Formatter
millisecondUnpadded datetime =
    toString (DateTime.millisecond datetime)


{-| Just a string to be placed in the output verbatim
-}
s : String -> Formatter
s =
    always


{-| ANSI SQL datetime format
-}
sqlDateTime : Formatter
sqlDateTime =
    yearFull
        <> s "-"
        <> monthPadded
        <> s "-"
        <> dayPadded
        <> s " "
        <> hour
        <> s ":"
        <> minute
        <> s ":"
        <> second
