-- Part 1 & 2

type Position = (Int, Int, Int)

update :: Position -> [String] -> Position
update (horiz, depth, aim) [command, number]
    | command == "forward" = (horiz + units, depth + aim * units, aim)
    | command == "down" = (horiz, depth, aim + units)
    | command == "up" = (horiz, depth, aim - units)
    | otherwise = (0,0,0)
    where units = read $ number

part1 :: IO ()
part1 = do
    input <- readFile "day2_input.txt"
    let contents = map words $ lines input
    let (horizontal, depth, aim) = foldl update (0,0,0) contents
    print $ horizontal * depth