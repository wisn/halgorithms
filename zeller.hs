{-
    Zeller's Congruence Algorithm
    <https://en.wikipedia.org/wiki/Zeller%27s_congruence>

    An algorithm to calculate the day of the week.
    Based on Gregorian calendar.

    Usage:
    - day is an Int from 1 to 31
    - month is an Int from 1 to 12
    - year is an 4 digits Int

    Example, 02 October 1998

    getDayFrom 2 10 1998
    => "Friday"
-}

module Zeller where

  getDayFrom :: Int -> Int -> Int -> String
  getDayFrom day month' year' =
    let month = if (month' < 3)
                  then month' + 12
                  else month'

        year  = if (month > 12)
                  then year' - 1
                  else year'

        k     = year `mod` 100
        j     = year `div` 100

        h = ( day
            + ((13 * (month + 1)) `div` 5)
            + k
            + (k `div` 4)
            + (j `div` 4)
            - (2 * j)
            ) `mod` 7

        days = [ "Saturday"
               , "Sunday"
               , "Monday"
               , "Tuesday"
               , "Wednesday"
               , "Thursday"
               , "Friday" ]

    in (days !! h)
