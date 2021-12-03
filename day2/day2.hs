-- Part 1

type Position = (Int, Int)

update :: Position -> [String] -> Position
update (horiz, depth) [command, value]
    | command == "forward" = (horiz + (read $ value), depth)
    | command == "down" = (horiz, depth + (read $ value))
    | command == "up" = (horiz, depth - (read $ value))
    | otherwise = (0,0)

part1 :: IO ()
part1 = do
    input <- readFile "day2_input.txt"
    let contents = map words $ lines input
    let (depth, horizontal) = foldl update (0,0) contents
    print $ depth * horizontal