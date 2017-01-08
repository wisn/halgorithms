{-
    Round-robin Scheduling Algorithm [Simple]
    <https://en.wikipedia.org/wiki/Round-robin_tournament#Scheduling_algorithm>

    An algorithm for creates a fair schedule.

    Usage:
    - list is a collection (list) of the participants

    Example, [1, 2, 3, 4]

    printOut [1, 2, 3, 4]
    =>  Round 1
        1 vs 4
        2 vs 3

        Round 2
        1 vs 3
        4 vs 2

        Round 3
        1 vs 2
        3 vs 4

    NOTE: This algorithm will be improved later using Berger tables.
-}

module RoundRobinScheduling where

  -- Rotate participants on a List
  rotater :: [a] -> [a]
  rotater list =
    let n      = length list
        middle = n `div` 2
        divide = splitAt middle list
        left   = fst divide
        right  = (reverse . snd) divide
        left'  = (head left) : (head right) : (init . tail) left
        right' = (tail right) ++ [last left]
    in left' ++ (reverse right')

  -- Create the schedule. Here is the main Round-robin Schedule Algorithm
  schedulizer :: [a] -> [[(a, a)]]
  schedulizer list = makesSchedule list (length list) (-1)
    where
      makesSchedule :: [a] -> Int -> Int -> [[(a, a)]]
      makesSchedule _ _ 0        = []
      makesSchedule list n games =
        let games'  = if games == (-1)
                        then do
                          if even n then (n - 1) else n
                        else games

            middle  = n `div` 2
            divide  = splitAt middle list

            left  = fst divide
            right = if even n
                       then (reverse . snd) divide
                       else (reverse . init . snd) divide

            zipped = zip left right
        in [zipped] ++ makesSchedule (rotater list) n (games' - 1)

  -- Collect data in one round
  collect :: (Show a) => [(a, a)] -> [String]
  collect []     = []
  collect (x:xs) =
    let home = fst x
        away = snd x
    in [(show home) ++ " vs " ++ (show away)] ++ collect xs

  -- Beautify the Output
  beautifyOut :: (Show a) => [a] -> [String]
  beautifyOut list = beautifySchedule (schedulizer list) 0 (-1)
    where
      beautifySchedule :: (Show a) => [[(a, a)]] -> Int -> Int -> [String]
      beautifySchedule _ _ 0         = []
      beautifySchedule list i rounds =
        let rounds' = if rounds == (-1)
                        then do
                          let n = length list
                          if even n then (n - 1) else n
                        else rounds
            play    = (unlines . collect) (list !! i)

        in ([ "Round " ++ (show $ i + 1) ++ "\n" ++ play]
           ++ beautifySchedule list (i + 1) (rounds' - 1))

  -- Print out the result
  printOut :: (Show a) => [a] -> IO ()
  printOut list =
    let schedule = beautifyOut list
    in (putStr . unlines) schedule
